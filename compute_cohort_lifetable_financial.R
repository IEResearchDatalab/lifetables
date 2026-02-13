################################################################################
#
# Cohort Life Table and Financial Impact Analysis
# 
# This script computes a cohort life table for a population starting at age 20
# in 2023, following them through 2099, with climate-adjusted mortality rates.
#
# Key features:
# - Mortality multipliers with RR >= 1 constraint
# - Daily-step risk averaging for annual mean RR
# - Three adaptation scenarios (0%, 50%, 90% heat attenuation)
# - Three RCP scenarios (2.6, 4.5, 7.0)
# - EPV calculations for life annuity-due and life insurance
# - Financial impact analysis (%Delta Z)
#
# Methodology follows main.tex equations and Masselot et al. (2025)
#
################################################################################

library(terra)
library(data.table)
library(arrow)
library(dplyr)
library(dlnm)
library(splines)

#------------------------------------------------------------------------------
# Load city configuration
#------------------------------------------------------------------------------
source("config.R")

#------------------------------------------------------------------------------
# Helper Functions
#------------------------------------------------------------------------------

# String concatenation
`%+%` <- function(a, b) paste0(a, b)
Rep <- function(x, n) paste(rep(x, n), collapse = "")

# Function to compute ax (fraction of year lived by those who die)
get_ax <- function(age) {
  ifelse(age == 0, 0.1, 0.5)
}

# Convert mx to qx
mx_to_qx <- function(mx, ax) {
  mx / (1 + (1 - ax) * mx)
}

# Convert qx to mx
qx_to_mx <- function(qx, ax) {
  qx / (1 - (1 - ax) * qx)
}

#------------------------------------------------------------------------------
# Step 1: Load 2023 ERA5 Temperature Data (Baseline)
#------------------------------------------------------------------------------

cat("=" %+% Rep("=", 70) %+% "\n")
cat("COHORT LIFE TABLE AND FINANCIAL IMPACT ANALYSIS\n")
cat("City: ", city_name, " (", city_code, ")\n", sep = "")
cat("Cohort: Age ", cohort_start_age, " in ", cohort_start_year, 
    " through ", cohort_end_year, "\n", sep = "")
cat("=" %+% Rep("=", 70) %+% "\n\n")

cat("Step 1: Loading 2023 ERA5 temperature data...\n")

nc_files <- list.files("data/2023_temp", pattern = paste0(city_name_lower, "_2023_\\d{2}\\.nc$"), 
                       full.names = TRUE)
nc_files <- sort(nc_files)

temp_2023_list <- lapply(nc_files, function(f) {
  r <- rast(f)
  vals <- global(r, "mean", na.rm = TRUE)$mean
  vals_celsius <- vals - 273.15
  month <- as.numeric(gsub(".*_(\\d{2})\\.nc$", "\\1", f))
  n_days <- nlyr(r)
  start_date <- as.Date(paste0("2023-", sprintf("%02d", month), "-01"))
  dates <- seq(start_date, by = "day", length.out = n_days)
  data.table(date = dates, tmean = vals_celsius)
})

temp_2023 <- rbindlist(temp_2023_list)
temp_2023 <- temp_2023[order(date)]

cat(sprintf("  Loaded %d days of 2023 temperature data\n", nrow(temp_2023)))
cat(sprintf("  Temperature range: %.1f°C to %.1f°C\n", 
            min(temp_2023$tmean), max(temp_2023$tmean)))

#------------------------------------------------------------------------------
# Step 2: Load Projected Temperature Data
#------------------------------------------------------------------------------

cat("\nStep 2: Loading projected temperature data...\n")

rcp85_paths <- c(
  "data/tmeanproj_rcp85.gz.parquet",
  "cordex_data/processed/tmeanproj_rcp85.gz.parquet"
)
rcp85_path <- rcp85_paths[file.exists(rcp85_paths)][1]

proj_data_base <- open_dataset("data/tmeanproj.gz.parquet") %>%
  filter(URAU_CODE == city_code) %>%
  collect() %>%
  as.data.table()

if (is.na(rcp85_path) || rcp85_path == "") {
  proj_data <- proj_data_base
} else {
  cat(sprintf("Found RCP8.5 parquet: %s\n", rcp85_path))
  proj_data_rcp85 <- open_dataset(rcp85_path) %>%
    filter(URAU_CODE == city_code) %>%
    collect() %>%
    as.data.table()
  proj_data <- rbindlist(list(proj_data_base, proj_data_rcp85), fill = TRUE)
}

proj_data[, ssp := as.character(ssp)]

proj_data[, year := year(date)]

gcm_cols <- names(proj_data)[grepl("^tas_", names(proj_data))]
gcm_cols <- gcm_cols[!gsub("tas_", "", gcm_cols) %in% gcmexcl]

cat(sprintf("  Loaded %d rows of projection data\n", nrow(proj_data)))
cat(sprintf("  Using %d GCMs\n", length(gcm_cols)))

#------------------------------------------------------------------------------
# Step 3: Load RR Coefficients for Bucharest
#------------------------------------------------------------------------------

cat("\nStep 3: Loading RR coefficients...\n")

coefs_all <- fread("data/coefs.csv")
coefs_city <- coefs_all[URAU_CODE == city_code]

cat(sprintf("  Loaded coefficients for %d age groups\n", nrow(coefs_city)))
print(coefs_city[, .(agegroup)])

#------------------------------------------------------------------------------
# Step 4: Define Basis Function Parameters (Historical Reference)
#------------------------------------------------------------------------------

cat("\nStep 4: Defining basis function parameters from historical data...\n")

hist_data <- proj_data[ssp == "hist"]
hist_temps <- unlist(hist_data[, ..gcm_cols], use.names = FALSE)
hist_temps <- hist_temps[!is.na(hist_temps)]

varknots <- quantile(hist_temps, varper / 100, na.rm = TRUE)
varbound <- range(hist_temps, na.rm = TRUE)

argvar <- list(fun = varfun, degree = vardegree, knots = varknots, Bound = varbound)

cat(sprintf("  Historical temperature range: %.1f°C to %.1f°C\n", varbound[1], varbound[2]))
cat(sprintf("  Knots at percentiles (%s): %.1f, %.1f, %.1f°C\n", 
            paste(varper, collapse = ", "), varknots[1], varknots[2], varknots[3]))

#------------------------------------------------------------------------------
# Step 5: Build RR Curves for Each Age Group
#------------------------------------------------------------------------------

cat("\nStep 5: Building RR curves for each age group...\n")

temp_seq <- seq(varbound[1], varbound[2], by = 0.1)  # Fine resolution
n_temp <- length(temp_seq)

basis <- do.call(onebasis, c(list(x = temp_seq), argvar))

# Store RR matrix: rows = temperature, columns = age groups
rr_matrix_raw <- matrix(NA, nrow = n_temp, ncol = length(agelabs))
mmt_vec <- numeric(length(agelabs))
coef_list <- list()

for (i in seq_along(agelabs)) {
  age_grp <- agelabs[i]
  coef_row <- coefs_city[agegroup == age_grp]
  coefs <- as.numeric(coef_row[, .(b1, b2, b3, b4, b5)])
  coef_list[[age_grp]] <- coefs
  
  log_rr <- basis %*% coefs
  
  # Find MMT in 25-99 percentile range
  ind <- temp_seq >= quantile(temp_seq, 0.25) & temp_seq <= quantile(temp_seq, 0.99)
  mmt <- temp_seq[ind][which.min(log_rr[ind])]
  mmt_vec[i] <- mmt
  
  # Center at MMT
  cenvec <- do.call(onebasis, c(list(x = mmt), argvar))
  log_rr_centered <- log_rr - drop(cenvec %*% coefs)
  
  # Constraint: RR >= 1 (avoid spline noise)
  rr <- pmax(exp(log_rr_centered), 1)
  rr_matrix_raw[, i] <- as.vector(rr)
  
  cat(sprintf("  %s (midpoint: %.1f): MMT = %.1f°C\n", age_grp, age_midpoints[i], mmt))
}

names(mmt_vec) <- agelabs

#------------------------------------------------------------------------------
# Step 6: Interpolate RR to Single-Year Ages
#------------------------------------------------------------------------------

cat("\nStep 6: Interpolating RR to single-year ages...\n")

# Ensure cohort_years exists (defined later in the script) and determine
# single-year age range to interpolate RR over. This covers the RR model
# support (20-100) and the cohort years defined in config.
if (!exists("cohort_years")) cohort_years <- cohort_start_year:cohort_end_year
age_min <- min(20, cohort_start_age)
age_max <- max(100, cohort_start_age + length(cohort_years) - 1)
age_range <- age_min:age_max

# For each temperature, interpolate RR across ages
rr_single_age <- matrix(NA, nrow = n_temp, ncol = length(age_range))
colnames(rr_single_age) <- age_range
rownames(rr_single_age) <- temp_seq

for (t_idx in seq_len(n_temp)) {
  rr_at_temp <- rr_matrix_raw[t_idx, ]
  # Linear interpolation with extrapolation at boundaries
  rr_interp <- approx(x = age_midpoints, y = rr_at_temp, 
                      xout = age_range, rule = 2)$y
  rr_single_age[t_idx, ] <- rr_interp
}

# Also interpolate MMT for each single-year age
mmt_single_age <- approx(x = age_midpoints, y = mmt_vec, 
                         xout = age_range, rule = 2)$y
names(mmt_single_age) <- age_range

cat(sprintf("  Interpolated to %d single-year ages (%d-%d)\n", length(age_range), age_min, age_max))

#------------------------------------------------------------------------------
# Step 7: Fast Function to Compute Daily-Step Average RR with Adaptation
#------------------------------------------------------------------------------

# Pre-compute temperature to index mapping for fast lookup
temp_to_idx <- function(t) {
  pmax(1, pmin(n_temp, round((t - varbound[1]) / 0.1) + 1))
}

# Function to apply adaptation to excess risk (heat only)
apply_adaptation_vec <- function(rr_vec, temps, mmt, adapt_level) {
  if (adapt_level == 0) return(rr_vec)
  heat_mask <- temps > mmt
  rr_vec[heat_mask] <- 1 + (1 - adapt_level) * (rr_vec[heat_mask] - 1)
  return(rr_vec)
}

# Fast vectorized function to compute daily-step average RR for all ages
compute_daily_avg_rr_all_ages <- function(temps, mmt_vec, adapt_level = 0) {
  temps <- temps[!is.na(temps)]
  if (length(temps) == 0) return(rep(NA_real_, length(age_range)))
  
  # Map temperatures to indices (vectorized)
  temp_indices <- temp_to_idx(temps)
  
  # Get RR values for all temperatures and ages at once
  rr_vals <- rr_single_age[temp_indices, , drop = FALSE]
  
  # Apply adaptation for each age (vectorized)
  avg_rr <- numeric(length(age_range))
  for (j in seq_along(age_range)) {
    mmt <- mmt_vec[j]
    rr_col <- rr_vals[, j]
    rr_adapted <- apply_adaptation_vec(rr_col, temps, mmt, adapt_level)
    avg_rr[j] <- mean(rr_adapted)
  }
  
  return(avg_rr)
}

#------------------------------------------------------------------------------
# Step 8: Compute 2023 Reference RR (Used for Normalization)
#------------------------------------------------------------------------------

cat("\nStep 8: Computing 2023 reference RR for normalization...\n")

# Use 2023 ERA5 data as the reference to ensure M_{x,2023,g} = 1.0 exactly
rr_2023_by_age <- compute_daily_avg_rr_all_ages(temp_2023$tmean, mmt_single_age, 0)
names(rr_2023_by_age) <- age_range

cat(sprintf("  2023 reference RR range: %.4f to %.4f\n", 
            min(rr_2023_by_age), max(rr_2023_by_age)))

#------------------------------------------------------------------------------
# Step 9: Validation - Confirm Multiplier at 2023 = 1.0
#------------------------------------------------------------------------------

cat("\nStep 9: Validation check...\n")

# Since we use 2023 as reference, multiplier at 2023 = RR_2023 / RR_2023 = 1.0
multiplier_2023 <- rr_2023_by_age / rr_2023_by_age  # All equal to 1

cat(sprintf("  Multiplier at age %d, year 2023: %.6f (should be exactly 1.0)\n", 
            cohort_start_age, multiplier_2023[as.character(cohort_start_age)]))

#------------------------------------------------------------------------------
# Step 10: Load Eurostat Projected Mortality Data
#------------------------------------------------------------------------------

cat("\nStep 10: Loading Eurostat projected mortality data...\n")

# Load Eurostat EUROPOP2019 regional projections for Bucharest
# This provides year-specific qx with built-in mortality improvement assumptions
mort_proj <- fread(sprintf("data/%s_mortality_projections.csv", city_name_lower))

# Filter for ages >= cohort start (cohort starts at age defined in config)
mort_proj <- mort_proj[age >= 20]

# Ensure we have the cohort years
setkey(mort_proj, year, age)

cat(sprintf("  Loaded Eurostat projections: %d records\n", nrow(mort_proj)))
cat(sprintf("  Age range: %d to %d\n", min(mort_proj$age), max(mort_proj$age)))
cat(sprintf("  Year range: %d to %d\n", min(mort_proj$year), max(mort_proj$year)))

# Verify coverage for our cohort (cohort start age in 2023 -> final age in cohort_end_year)
cohort_check <- mort_proj[year == 2023 & age == cohort_start_age]
if (nrow(cohort_check) == 0) {
  stop(sprintf("Missing 2023 data for age %d in mortality projections!", cohort_start_age))
}
cat(sprintf("  Cohort start qx (age %d, 2023): %.6f\n", cohort_start_age, cohort_check$qx[1]))

# Create baseline lookup table (for compatibility with existing code)
# This is used for the reference period normalization
baseline_lt <- mort_proj[year == 2023, .(age, qx, mx, ax)]
setkey(baseline_lt, age)

#------------------------------------------------------------------------------
# Step 11: Compute Mortality Multipliers for All Years and Scenarios
#------------------------------------------------------------------------------

cat("\nStep 11: Computing mortality multipliers for all scenarios...\n")

# Years for cohort (2023 to 2099 = 77 years)
cohort_years <- cohort_start_year:cohort_end_year

# Storage for multipliers
mult_results <- list()

for (ssp_val in ssp_codes) {
  rcp_lab <- rcp_labels[ssp_val]
  cat(sprintf("\n  Processing %s...\n", rcp_lab))
  
  ssp_data <- proj_data[ssp == ssp_val]
  
  for (adapt_idx in seq_along(adaptation_levels)) {
    adapt_final <- adaptation_levels[adapt_idx]
    adapt_lab <- adaptation_labels[adapt_idx]
    
    cat(sprintf("    Adaptation: %s\n", adapt_lab))
    
    for (yr in cohort_years) {
      # Skip 2023 (use baseline)
      if (yr < 2024) next
      
      year_data <- ssp_data[year == yr]
      if (nrow(year_data) == 0) next
      
      # Compute adaptation level for this year (linear from t0 to tf)
      adapt_t <- ifelse(yr <= t0_adapt, 0,
                        ifelse(yr >= tf_adapt, adapt_final,
                               adapt_final * (yr - t0_adapt) / (tf_adapt - t0_adapt)))
      
      # Pool temperatures from all GCMs (vectorized)
      all_temps <- unlist(year_data[, ..gcm_cols], use.names = FALSE)
      all_temps <- all_temps[!is.na(all_temps)]
      
      # Compute multiplier for all ages at once (vectorized)
      avg_rr_vec <- compute_daily_avg_rr_all_ages(all_temps, mmt_single_age, adapt_t)
      multiplier_vec <- avg_rr_vec / rr_2023_by_age
      
      # Store results for all ages
      mult_results[[length(mult_results) + 1]] <- data.table(
        year = yr,
        age = age_range,
        ssp = ssp_val,
        rcp = rcp_lab,
        adaptation = adapt_lab,
        avg_rr = avg_rr_vec,
        multiplier = multiplier_vec
      )
    }
  }
}

multipliers <- rbindlist(mult_results)

# Add 2023 baseline (multiplier = 1 for all)
baseline_mult <- CJ(
  year = 2023,
  age = age_range,
  ssp = ssp_codes,
  adaptation = adaptation_labels
)
baseline_mult[, rcp := rcp_labels[ssp]]
baseline_mult[, avg_rr := rr_2023_by_age[as.character(age)], by = age]
baseline_mult[, multiplier := multiplier_2023[as.character(age)], by = age]

multipliers <- rbind(baseline_mult, multipliers)
setkey(multipliers, ssp, adaptation, year, age)

cat(sprintf("\n  Computed %d multiplier records\n", nrow(multipliers)))

#------------------------------------------------------------------------------
# Step 12: Build Cohort Life Tables
#------------------------------------------------------------------------------

cat("\nStep 12: Building cohort life tables...\n")

# Function to build cohort life table with year-specific projected mortality
build_cohort_lifetable <- function(mort_proj_dt, mult_dt, ssp_val, adapt_lab) {
  
  rcp_lab <- rcp_labels[ssp_val]
  
  # Cohort ages from 20 to end (age 20 in 2023, age 21 in 2024, ..., age 96 in 2099)
  cohort_age <- cohort_start_age:(cohort_start_age + length(cohort_years) - 1)
  cohort_years_vec <- cohort_years
  
  # Initialize life table
  lt <- data.table(
    age = cohort_age,
    year = cohort_years_vec[1:length(cohort_age)],
    rcp = rcp_lab,
    adaptation = adapt_lab
  )
  
  # Get YEAR-SPECIFIC baseline qx and mx from Eurostat projections (key change!)
  # This includes mortality improvement over time
  lt <- merge(lt, 
              mort_proj_dt[, .(year, age, qx_base = qx, mx_base = mx, ax)], 
              by = c("year", "age"), 
              all.x = TRUE)
  
  # Check for missing mortality data
  if (any(is.na(lt$qx_base))) {
    missing <- lt[is.na(qx_base), .(year, age)]
    warning(sprintf("Missing mortality data for %d age-year combinations", nrow(missing)))
    # Use last available year's data for ages beyond projection
    lt[is.na(qx_base), qx_base := mort_proj_dt[year == max(year) & age == .BY$age, qx], by = age]
    lt[is.na(mx_base), mx_base := mort_proj_dt[year == max(year) & age == .BY$age, mx], by = age]
    lt[is.na(ax), ax := 0.5]
  }
  
  # Get climate mortality multipliers
  lt <- merge(lt, 
              mult_dt[ssp == ssp_val & adaptation == adapt_lab, 
                      .(year, age, multiplier)],
              by = c("year", "age"), all.x = TRUE)
  
  # Fill missing multipliers with 1 (baseline year 2023 or years without climate data)
  lt[is.na(multiplier), multiplier := 1]
  
  # Climate-adjusted mortality rate = baseline (with improvement) × climate multiplier
  lt[, mx_clim := mx_base * multiplier]
  
  # Convert mx to qx for climate-adjusted rates
  lt[, qx_clim := mx_to_qx(mx_clim, ax)]
  
  # Ensure qx <= 1
  lt[qx_base > 1, qx_base := 1]
  lt[qx_clim > 1, qx_clim := 1]
  
  # Compute lx and dx (survivors and deaths)
  lt <- lt[order(age)]
  
  # Baseline (with mortality improvement, no climate effect)
  lt[, lx_base := radix]
  for (i in 2:nrow(lt)) {
    lt$lx_base[i] <- lt$lx_base[i-1] * (1 - lt$qx_base[i-1])
  }
  lt[, dx_base := lx_base * qx_base]
  
  # Climate-adjusted (improvement + climate multiplier)
  lt[, lx_clim := radix]
  for (i in 2:nrow(lt)) {
    lt$lx_clim[i] <- lt$lx_clim[i-1] * (1 - lt$qx_clim[i-1])
  }
  lt[, dx_clim := lx_clim * qx_clim]
  
  return(lt)
}

# Build life tables for all scenarios
lifetables <- list()

for (ssp_val in ssp_codes) {
  for (adapt_lab in adaptation_labels) {
    key <- paste(ssp_val, adapt_lab, sep = "_")
    lifetables[[key]] <- build_cohort_lifetable(mort_proj, multipliers, ssp_val, adapt_lab)
  }
}

cat(sprintf("  Built %d cohort life tables\n", length(lifetables)))

#------------------------------------------------------------------------------
# Step 13: Calculate Actuarial Quantities (EPV)
#------------------------------------------------------------------------------

cat("\nStep 13: Calculating actuarial quantities...\n")

# Function to compute EPV of deferred term annuity-due
# Purchased at cohort start age, payments from `annuity_age_start` to `annuity_age_end`
# Formula: _{d|x}ä_x = sum_{k=d}^{d+term-1} v^k * k_p_x where d = annuity_age_start - cohort_start_age
compute_annuity_epv <- function(
  lt,
  qx_col = "qx_base",
  age_purchase = 20,
  age_start = 65,
  age_end = 84) {
  n <- nrow(lt)
  v <- discount_factor
  
  # Survival probabilities
  px <- 1 - lt[[qx_col]]
  
  # k-year survival probability from age x (starting age 20)
  kpx <- cumprod(c(1, px[-n]))  # kpx[k+1] = k_p_x
  
  # Deferral: 45 years (until age 65)
  # Term: 20 years (payments at ages 65, 66, ..., 84)
  # k ranges from 45 to 64 (R indices 46 to 65)  
  k_start <- age_start - age_purchase      # first payment at k=45 (age 65)
  k_end <- age_end - age_purchase  # last payment at k=64 (age 84)
  
  # Ensure we don't exceed available data
  k_end <- min(k_end, n - 1)
  
  k_range <- k_start:k_end
  vk <- v^k_range
  survival <- kpx[k_range + 1]  # +1 for R 1-indexing
  
  epv <- sum(vk * survival)
  return(epv)
}

# Function to compute EPV of unit life insurance (Ax)
compute_insurance_epv <- function(lt, qx_col = "qx_base") {
  n <- nrow(lt)
  v <- discount_factor
  
  qx <- lt[[qx_col]]
  px <- 1 - qx
  
  # k-year survival probability from age x
  kpx <- cumprod(c(1, px[-n]))
  
  # EPV = sum_{k=0}^{n-1} v^{k+1} * k_p_x * q_{x+k}
  vk1 <- v^(1:n)
  
  epv <- sum(vk1 * kpx * qx)
  return(epv)
}

# Compute EPVs for all scenarios
epv_results <- list()

for (key in names(lifetables)) {
  lt <- lifetables[[key]]
  
  # Extract scenario info from first row
  rcp_lab <- lt$rcp[1]
  adapt_lab <- lt$adaptation[1]
  
  # Baseline EPVs
  annuity_base <- compute_annuity_epv(
    lt,
    "qx_base",
    age_purchase = cohort_start_age,
    age_start = annuity_age_start,
    age_end = annuity_age_end
  )
  insurance_base <- compute_insurance_epv(lt, "qx_base")
  
  # Climate-adjusted EPVs
  annuity_clim <- compute_annuity_epv(
    lt,
    "qx_clim",
    age_purchase = cohort_start_age,
    age_start = annuity_age_start,
    age_end = annuity_age_end
  )
  insurance_clim <- compute_insurance_epv(lt, "qx_clim")
  
  # Compute changes
  delta_annuity <- annuity_clim - annuity_base
  delta_insurance <- insurance_clim - insurance_base
  
  pct_delta_annuity <- 100 * delta_annuity / annuity_base
  pct_delta_insurance <- 100 * delta_insurance / insurance_base
  
  # Reserve calculations (split between annuity and life insurance)
  # Annuity reserve: EPV of future annuity payments to be made
  # Life insurance reserve: EPV of future death benefit payable
  # For single premium contracts, reserve equals the EPV of benefits
  reserve_annuity_base <- annuity_base
  reserve_annuity_clim <- annuity_clim
  delta_reserve_annuity <- reserve_annuity_clim - reserve_annuity_base
  pct_delta_reserve_annuity <- 100 * delta_reserve_annuity / reserve_annuity_base
  
  reserve_ins_base <- insurance_base
  reserve_ins_clim <- insurance_clim
  delta_reserve_ins <- reserve_ins_clim - reserve_ins_base
  pct_delta_reserve_ins <- 100 * delta_reserve_ins / reserve_ins_base
  
  # Total reserve (combined portfolio)
  reserve_total_base <- annuity_base + insurance_base
  reserve_total_clim <- annuity_clim + insurance_clim
  delta_reserve_total <- reserve_total_clim - reserve_total_base
  pct_delta_reserve_total <- 100 * delta_reserve_total / reserve_total_base
  
  epv_results[[key]] <- data.table(
    rcp = rcp_lab,
    adaptation = adapt_lab,
    annuity_base = annuity_base,
    annuity_clim = annuity_clim,
    delta_annuity = delta_annuity,
    pct_delta_annuity = pct_delta_annuity,
    insurance_base = insurance_base,
    insurance_clim = insurance_clim,
    delta_insurance = delta_insurance,
    pct_delta_insurance = pct_delta_insurance,
    reserve_annuity_base = reserve_annuity_base,
    reserve_annuity_clim = reserve_annuity_clim,
    delta_reserve_annuity = delta_reserve_annuity,
    pct_delta_reserve_annuity = pct_delta_reserve_annuity,
    reserve_ins_base = reserve_ins_base,
    reserve_ins_clim = reserve_ins_clim,
    delta_reserve_ins = delta_reserve_ins,
    pct_delta_reserve_ins = pct_delta_reserve_ins,
    reserve_total_base = reserve_total_base,
    reserve_total_clim = reserve_total_clim,
    delta_reserve_total = delta_reserve_total,
    pct_delta_reserve_total = pct_delta_reserve_total
  )
}

epv_summary <- rbindlist(epv_results)

cat("\n  EPV Summary:\n")
print(epv_summary[, .(rcp, adaptation, annuity_base = round(annuity_base, 3), 
                       pct_delta_annuity = round(pct_delta_annuity, 3),
                       insurance_base = round(insurance_base, 4),
                       pct_delta_insurance = round(pct_delta_insurance, 3))])

#------------------------------------------------------------------------------
# Step 14: Prepare Output Data
#------------------------------------------------------------------------------

cat("\nStep 14: Preparing output data...\n")

# Combine all life tables into one dataset
all_lifetables <- rbindlist(lifetables, idcol = "scenario")

# Format for output
output_lt <- all_lifetables[, .(
  age = age,
  year = year,
  rcp = rcp,
  adaptation = adaptation,
  qx_base = qx_base,
  qx_clim = qx_clim,
  mx_base = mx_base,
  mx_clim = mx_clim,
  multiplier = multiplier,
  lx_base = lx_base,
  lx_clim = lx_clim,
  dx_base = dx_base,
  dx_clim = dx_clim
)]

#------------------------------------------------------------------------------
# Step 15: Create Validation Data (2023 Temperature Distribution)
#------------------------------------------------------------------------------

cat("\nStep 15: Creating validation data...\n")

# Temperature distribution for 2023
temp_dist_2023 <- temp_2023[, .(n_days = .N), by = .(temp_bin = round(tmean))]
temp_dist_2023 <- temp_dist_2023[order(temp_bin)]
temp_dist_2023[, proportion := n_days / sum(n_days)]

# Validation: Multiplier at cohort start age, year 2023 should be exactly 1
validation_summary <- data.table(
  metric = c("Mean temperature 2023", 
             "Min temperature 2023", 
             "Max temperature 2023",
             "Number of days 2023",
             sprintf("Multiplier at age %d, 2023", cohort_start_age),
             sprintf("Reference RR at age %d (2023)", cohort_start_age),
             "Interest rate used"),
  value = c(mean(temp_2023$tmean),
            min(temp_2023$tmean),
            max(temp_2023$tmean),
            nrow(temp_2023),
            multiplier_2023[as.character(cohort_start_age)],
            rr_2023_by_age[as.character(cohort_start_age)],
            interest_rate)
)

#------------------------------------------------------------------------------
# Step 16: Save Results
#------------------------------------------------------------------------------

cat("\nStep 16: Saving results...\n")

# Create output directory if needed
if (!dir.exists("results_csv")) dir.create("results_csv")

# Save cohort life table
fwrite(output_lt, sprintf("results_csv/%s_cohort_lifetable_climate.csv", city_name_lower))
cat(sprintf("  Saved: results_csv/%s_cohort_lifetable_climate.csv\n", city_name_lower))

# Save EPV summary
fwrite(epv_summary, sprintf("results_csv/%s_financial_impact_summary.csv", city_name_lower))
cat(sprintf("  Saved: results_csv/%s_financial_impact_summary.csv\n", city_name_lower))

# Save validation data
fwrite(temp_dist_2023, sprintf("results_csv/%s_2023_temp_distribution.csv", city_name_lower))
cat(sprintf("  Saved: results_csv/%s_2023_temp_distribution.csv\n", city_name_lower))

fwrite(validation_summary, sprintf("results_csv/%s_validation_summary.csv", city_name_lower))
cat(sprintf("  Saved: results_csv/%s_validation_summary.csv\n", city_name_lower))

# Save multipliers for reference
fwrite(multipliers, sprintf("results_csv/%s_mortality_multipliers_cohort.csv", city_name_lower))
cat(sprintf("  Saved: results_csv/%s_mortality_multipliers_cohort.csv\n", city_name_lower))

#------------------------------------------------------------------------------
# Step 17: Print Summary Report
#------------------------------------------------------------------------------

cat("\n")
cat("=" %+% Rep("=", 70) %+% "\n")
cat("SUMMARY REPORT\n")
cat("=" %+% Rep("=", 70) %+% "\n")

cat("\n--- Cohort Parameters ---\n")
cat(sprintf("  Starting population: %s individuals\n", format(radix, big.mark = ",")))
cat(sprintf("  Starting age: %d (year %d)\n", cohort_start_age, cohort_start_year))
cat(sprintf("  Ending age: %d (year %d)\n", 
            cohort_start_age + length(cohort_years) - 1, cohort_end_year))
cat(sprintf("  Interest rate: %.1f%%\n", interest_rate * 100))

cat("\n--- Baseline Mortality Source ---\n")
cat("  Source: Eurostat EUROPOP2019 Regional Projections (proj_19raasmr3 + proj_19rp3)\n")
cat("  Region: București (Bucharest) - NUTS 3\n")
cat("  Years: 2019-2100 (with built-in mortality improvement assumptions)\n")
cat("  Sex: Population-weighted combination of male and female\n")
cat(sprintf("  Baseline qx at age %d, 2023: %.6f\n", cohort_start_age, mort_proj[year == 2023 & age == cohort_start_age, qx]))
cat(sprintf("  Baseline qx at age 60, 2023: %.6f\n", mort_proj[year == 2023 & age == 60, qx]))
cat(sprintf("  Baseline qx at age 60, 2050: %.6f (%.1f%% improvement)\n", 
            mort_proj[year == 2050 & age == 60, qx],
            (1 - mort_proj[year == 2050 & age == 60, qx] / mort_proj[year == 2023 & age == 60, qx]) * 100))

cat("\n--- Validation ---\n")
cat(sprintf("  Climate mortality multiplier at age %d, 2023: %.6f\n", cohort_start_age, multiplier_2023[as.character(cohort_start_age)]))
cat("  (Should be 1.0 for proper normalization - climate effect relative to 2023 baseline)\n")

cat("\n--- Financial Impact Summary (% Change vs Baseline) ---\n")
  # Describe annuity dynamically using configuration values
  deferral_years <- annuity_age_start - cohort_start_age
  payment_label <- sprintf("payments ages %d-%d", annuity_age_start, annuity_age_end)
  cat(sprintf("\nDeferred Term Annuity-Due (defer %d years, %s):\n", deferral_years, payment_label))
for (i in 1:nrow(epv_summary)) {
  cat(sprintf("  %s, Adaptation %s: %+.3f%%\n", 
              epv_summary$rcp[i], epv_summary$adaptation[i], 
              epv_summary$pct_delta_annuity[i]))
}

cat("\nUnit Life Insurance (Ax):\n")
for (i in 1:nrow(epv_summary)) {
  cat(sprintf("  %s, Adaptation %s: %+.3f%%\n", 
              epv_summary$rcp[i], epv_summary$adaptation[i], 
              epv_summary$pct_delta_insurance[i]))
}

# Create summary table for LaTeX
cat("\n--- Summary Table for LaTeX (Table format) ---\n")
# Print summaries for the adaptation scenarios defined in config.R
for (adapt_lab in adaptation_labels) {
  cat(sprintf("\nAdaptation = %s:\n", adapt_lab))
  summary_dt <- epv_summary[adaptation == adapt_lab,
                            .(rcp,
                              annuity_pct = sprintf("%+.2f", pct_delta_annuity),
                              insurance_pct = sprintf("%+.2f", pct_delta_insurance),
                              reserve_annuity_pct = sprintf("%+.2f", pct_delta_reserve_annuity),
                              reserve_ins_pct = sprintf("%+.2f", pct_delta_reserve_ins))]
  print(summary_dt)
}

cat("\n--- Writing LaTeX Summary Table ---\n")

# Determine RCP display/order and adaptation display dynamically from config
rcp_order <- as.character(rcp_labels[ssp_codes])
rcp_header <- gsub("RCP[[:space:]]*", "", rcp_order)
adapt_order <- adaptation_labels
adapt_display <- gsub("%", "\\\\%", adapt_order)

format_pct <- function(x) sprintf("%+.2f", x)
latex_val <- function(x) sprintf("$%s$", format_pct(x))

build_rows <- function(quantity_label, value_col) {
  rows <- character(0)
  for (i in seq_along(adapt_order)) {
    adapt <- adapt_order[i]
    adapt_label <- adapt_display[i]
    row_vals <- vapply(rcp_order, function(rcp_val) {
      val <- epv_summary[rcp == rcp_val & adaptation == adapt, get(value_col)]
      val <- val[1]
      if (length(val) == 0 || is.na(val)) "NA" else latex_val(val)
    }, character(1))
    label <- if (i == 1) quantity_label else ""
    row_text <- paste(c(label, adapt_label, row_vals), collapse = " & ")
    rows <- c(rows, paste0(row_text, " ", intToUtf8(92), intToUtf8(92)))
  }
  rows
}

header_rcp <- paste(c("", "", "\\textbf{RCP}", "", "", ""), collapse = " & ")
header_cols <- paste(c("\\textbf{Quantity (\\%)}", "\\textbf{Adaptation}",
                       sprintf("\\textbf{%s}", rcp_header)), collapse = " & ")

tex_lines <- c(
  "\\begin{tabular}{llrrrr}",
  "\\toprule",
  paste0(header_rcp, " ", intToUtf8(92), intToUtf8(92)),
  paste0(header_cols, " ", intToUtf8(92), intToUtf8(92)),
  "\\midrule",
  build_rows("Annuity EPV", "pct_delta_annuity"),
  "\\midrule",
  build_rows("Life insurance EPV", "pct_delta_insurance"),
  "\\bottomrule",
  "\\end{tabular}"
)

tex_file <- sprintf("img/%s_financial_impact_summary.tex", city_name_lower)
writeLines(tex_lines, tex_file)
cat(sprintf("  Saved: %s\n", tex_file))

cat("\n" %+% Rep("=", 70) %+% "\n")
cat("DONE!\n")
cat(Rep("=", 70) %+% "\n")