################################################################################
#
# Cohort Life Table and Financial Impact Analysis
# 
# This script computes a cohort life table for a population starting at age 20
# in the cohort start year, following them through 2099, with climate-adjusted
# mortality rates. The baseline temperature reference uses a multi-year GCM
# climatological average (configurable via baseline_temp_period in config.R)
# rather than a single year of observed data.
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

library(data.table)
library(arrow)
library(dplyr)
library(dlnm)
library(splines)

#------------------------------------------------------------------------------
# Load city configuration
#------------------------------------------------------------------------------
source("legacy/config.R")

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
# Step 1: Header and Configuration Summary
#------------------------------------------------------------------------------

cat("=" %+% Rep("=", 70) %+% "\n")
cat("COHORT LIFE TABLE AND FINANCIAL IMPACT ANALYSIS\n")
cat("City: ", city_name, " (", city_code, ")\n", sep = "")
cat("Cohort: Age ", cohort_start_age, " in ", cohort_start_year, 
    " through ", cohort_end_year, "\n", sep = "")
cat("Baseline temperature period: ", baseline_temp_label, "\n", sep = "")
cat("=" %+% Rep("=", 70) %+% "\n\n")

#------------------------------------------------------------------------------
# Step 2: Load Projected Temperature Data
#------------------------------------------------------------------------------

cat("\nStep 2: Loading projected temperature data...\n")

proj_data <- open_dataset("data/tmeanproj.gz.parquet") %>%
  filter(URAU_CODE == city_code) %>%
  collect() %>%
  as.data.table()

proj_data[, year := year(date)]
proj_data[, doy := as.integer(format(date, "%j"))]
proj_data[doy > 365, doy := 365L]  # cap leap-year day 366

gcm_cols <- names(proj_data)[grepl("^tas_", names(proj_data))]
gcm_cols <- gcm_cols[!gsub("tas_", "", gcm_cols) %in% gcmexcl]

cat(sprintf("  Loaded %d rows of projection data\n", nrow(proj_data)))
cat(sprintf("  Using %d GCMs\n", length(gcm_cols)))

# --- Load seasonal mortality weights ---
seasonal_weights_file <- sprintf("results_csv/seasonal_weights_daily_%s.csv", city_name_lower)
if (file.exists(seasonal_weights_file)) {
  sw_dt <- fread(seasonal_weights_file)
  # Build lookup matrix: rows = ages 20..100 (81), cols = doy 1..365
  sw_matrix <- matrix(1 / 365, nrow = 81, ncol = 365,
                      dimnames = list(20:100, 1:365))
  for (i in seq_len(nrow(sw_dt))) {
    a <- sw_dt$age[i]; d <- sw_dt$doy[i]
    sw_matrix[as.character(a), d] <- sw_dt$weight[i]
  }
  use_seasonal_weights <- TRUE
  cat("  Loaded seasonal mortality weights (age × DOY)\n")
} else {
  use_seasonal_weights <- FALSE
  cat("  Seasonal weights not found — using uniform weighting\n")
}

#------------------------------------------------------------------------------
# Step 3: Load RR Coefficients (single-year ages from results_csv)
#------------------------------------------------------------------------------

cat("\nStep 3: Loading RR coefficients (single-year ages)...\n")

coefs_all <- fread(sprintf("results_csv/coefs_%s.csv", city_name_lower))
coefs_city <- coefs_all[URAU_CODE == city_code]

# agegroup is stored as "20","21",... -> make it integer
coefs_city[, agegroup := as.integer(agegroup)]
setkey(coefs_city, agegroup)

# Single-year age range
age_range <- 20:100

# Sanity check: must have exactly one row per age
missing_ages <- setdiff(age_range, coefs_city$agegroup)
if (length(missing_ages) > 0) {
  stop(sprintf("Missing coefficients for ages: %s", paste(missing_ages, collapse = ", ")))
}
dup_ages <- coefs_city[, .N, by = agegroup][N > 1]
if (nrow(dup_ages) > 0) {
  stop(sprintf("Duplicate coefficient rows for ages: %s", paste(dup_ages$agegroup, collapse = ", ")))
}

cat(sprintf("  Loaded coefficients for %d single-year ages (%d-%d)\n",
            nrow(coefs_city), min(coefs_city$agegroup), max(coefs_city$agegroup)))

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
# Step 5: Build RR Curves for Each Single-Year Age (20-100) and compute MMT
#------------------------------------------------------------------------------

cat("\nStep 5: Building RR curves for each single-year age (20-100)...\n")

temp_seq <- seq(varbound[1], varbound[2], by = 0.1)  # Fine resolution
n_temp <- length(temp_seq)

basis <- do.call(onebasis, c(list(x = temp_seq), argvar))

# Coefficient columns (b1..bk)
coef_cols <- names(coefs_city)[grepl("^b[0-9]+$", names(coefs_city))]
if (length(coef_cols) == 0) stop("No coefficient columns b1..bk found in coefs_city.")

# MMT search band (robust)
q25 <- quantile(temp_seq, 0.25)
q99 <- quantile(temp_seq, 0.99)
ind <- temp_seq >= q25 & temp_seq <= q99

# Store RR matrix: rows = temperature, cols = single-year ages
rr_single_age <- matrix(NA_real_, nrow = n_temp, ncol = length(age_range))
colnames(rr_single_age) <- age_range
rownames(rr_single_age) <- temp_seq

# Store MMT per age
mmt_single_age <- numeric(length(age_range))
names(mmt_single_age) <- age_range

for (j in seq_along(age_range)) {
  a <- age_range[j]

  row <- coefs_city[agegroup == a]
  # row is a 1-row data.table
  coefs <- as.numeric(row[, ..coef_cols])

  log_rr <- basis %*% coefs

  # Find MMT
  mmt <- temp_seq[ind][which.min(log_rr[ind])]
  mmt_single_age[j] <- mmt

  # Center at MMT
  cenvec <- do.call(onebasis, c(list(x = mmt), argvar))
  log_rr_centered <- log_rr - drop(cenvec %*% coefs)

  # Enforce RR >= 1
  rr <- pmax(exp(log_rr_centered), 1)

  rr_single_age[, j] <- as.vector(rr)
}

cat(sprintf("  Built RR(T) for %d ages on %d temperature points\n",
            length(age_range), n_temp))

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
# component: "total" (default), "heat" (days > MMT only), or "cold" (days <= MMT only)
# doys: optional integer vector of day-of-year (1-365) paralleling temps,
#       used for seasonal mortality weighting when use_seasonal_weights = TRUE.
compute_daily_avg_rr_all_ages <- function(temps, mmt_vec, adapt_level = 0,
                                          component = rr_component,
                                          doys = NULL) {
  temps <- temps[!is.na(temps)]
  if (length(temps) == 0) return(rep(NA_real_, length(age_range)))
  
  # Map temperatures to indices (vectorized)
  temp_indices <- temp_to_idx(temps)
  
  # Get RR values for all temperatures and ages at once
  rr_vals <- rr_single_age[temp_indices, , drop = FALSE]
  
  # Apply adaptation and component filtering for each age (vectorized)
  avg_rr <- numeric(length(age_range))
  for (j in seq_along(age_range)) {
    mmt <- mmt_vec[j]
    rr_col <- rr_vals[, j]
    rr_adapted <- apply_adaptation_vec(rr_col, temps, mmt, adapt_level)
    
    # Component filtering: isolate heat or cold contribution
    if (component == "heat") {
      rr_adapted[temps <= mmt] <- 1
    } else if (component == "cold") {
      rr_adapted[temps > mmt] <- 1
    }
    
    # Seasonal mortality weighting
    if (use_seasonal_weights && !is.null(doys)) {
      w <- sw_matrix[as.character(age_range[j]), doys]
      avg_rr[j] <- weighted.mean(rr_adapted, w)
    } else {
      avg_rr[j] <- mean(rr_adapted)
    }
  }
  
  return(avg_rr)
}

#------------------------------------------------------------------------------
# Step 8: Compute Baseline Reference RR (Used for Normalization)
#------------------------------------------------------------------------------

cat("\nStep 8: Computing baseline reference RR for normalization...\n")
cat(sprintf("  Baseline temperature period: %s\n", baseline_temp_label))
cat(sprintf("  RR component: %s\n", rr_component))

# Pool daily GCM temperatures over the baseline period across all GCMs.
# Historical years (ssp == "hist") and early projection years (any SSP, as
# forcings barely diverge before ~2030) are combined.
baseline_hist <- proj_data[ssp == "hist" & year %in% baseline_temp_period]
baseline_proj <- proj_data[ssp %in% ssp_codes & year %in% baseline_temp_period & year > 2014]

# Pool all daily temperatures from all GCMs (with matching DOYs)
baseline_temps_hist <- unlist(baseline_hist[, ..gcm_cols], use.names = FALSE)
baseline_temps_proj <- unlist(baseline_proj[, ..gcm_cols], use.names = FALSE)
baseline_temps_all  <- c(baseline_temps_hist, baseline_temps_proj)

baseline_doys_hist <- rep(baseline_hist$doy, length(gcm_cols))
baseline_doys_proj <- rep(baseline_proj$doy, length(gcm_cols))
baseline_doys_all  <- c(baseline_doys_hist, baseline_doys_proj)

# Remove NAs in parallel
valid_bl <- !is.na(baseline_temps_all)
baseline_temps_all <- baseline_temps_all[valid_bl]
baseline_doys_all  <- baseline_doys_all[valid_bl]

cat(sprintf("  Pooled %s daily temperature values for baseline\n",
            format(length(baseline_temps_all), big.mark = ",")))
cat(sprintf("  Baseline temperature range: %.1f°C to %.1f°C\n",
            min(baseline_temps_all), max(baseline_temps_all)))
cat(sprintf("  Baseline mean temperature: %.2f°C\n", mean(baseline_temps_all)))

# Compute baseline RR by age using the climatological temperature distribution
rr_baseline_by_age <- compute_daily_avg_rr_all_ages(baseline_temps_all, mmt_single_age, 0,
                                                     doys = baseline_doys_all)
names(rr_baseline_by_age) <- age_range

cat(sprintf("  Baseline reference RR range: %.4f to %.4f\n", 
            min(rr_baseline_by_age), max(rr_baseline_by_age)))

#------------------------------------------------------------------------------
# Step 9: Validation - Baseline multiplier check
#------------------------------------------------------------------------------

cat("\nStep 9: Validation check...\n")

# The multiplier at cohort_start_year will NOT be exactly 1.0 anymore,
# since the baseline is now a climatological average, not a single year.
# This is expected: years warmer than the 1990-2019 average will have M > 1.
cat("  Note: With a multi-year climatological baseline, multipliers at\n")
cat("  cohort start year are expected to be > 1 (climate already warmer).\n")

#------------------------------------------------------------------------------
# Step 10: Load Eurostat Projected Mortality Data
#------------------------------------------------------------------------------

cat("\nStep 10: Loading Eurostat projected mortality data...\n")

# Load Eurostat EUROPOP2019 regional projections for target
# This provides year-specific qx with built-in mortality improvement assumptions
mort_proj <- fread(sprintf("results_csv/mortality_projections_%s.csv", city_name_lower))

# Filter for ages 20+ (cohort starts at age 20)
mort_proj <- mort_proj[age >= 20]

# Ensure we have the cohort years
setkey(mort_proj, year, age)

cat(sprintf("  Loaded Eurostat projections: %d records\n", nrow(mort_proj)))
cat(sprintf("  Age range: %d to %d\n", min(mort_proj$age), max(mort_proj$age)))
cat(sprintf("  Year range: %d to %d\n", min(mort_proj$year), max(mort_proj$year)))

# Verify coverage for our cohort
cohort_check <- mort_proj[year == cohort_start_year & age == cohort_start_age]
if (nrow(cohort_check) == 0) {
  stop(sprintf("Missing %d data for age %d in mortality projections!",
               cohort_start_year, cohort_start_age))
}
cat(sprintf("  Cohort start qx (age %d, %d): %.6f\n",
            cohort_start_age, cohort_start_year, cohort_check$qx[1]))

# Create baseline lookup table
baseline_lt <- mort_proj[year == cohort_start_year, .(age, qx, mx, ax)]
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
      # Skip cohort start year (handled separately as baseline_mult)
      if (yr == cohort_start_year) next
      
      year_data <- ssp_data[year == yr]
      if (nrow(year_data) == 0) next
      
      # Compute adaptation level for this year (linear from t0 to tf)
      adapt_t <- ifelse(yr <= t0_adapt, 0,
                        ifelse(yr >= tf_adapt, adapt_final,
                               adapt_final * (yr - t0_adapt) / (tf_adapt - t0_adapt)))
      
      # Pool temperatures from all GCMs (vectorized) with DOYs
      all_temps <- unlist(year_data[, ..gcm_cols], use.names = FALSE)
      all_doys  <- rep(year_data$doy, length(gcm_cols))
      valid_yr  <- !is.na(all_temps)
      all_temps <- all_temps[valid_yr]
      all_doys  <- all_doys[valid_yr]
      
      # Compute multiplier for all ages at once (vectorized)
      avg_rr_vec <- compute_daily_avg_rr_all_ages(all_temps, mmt_single_age, adapt_t,
                                                   doys = all_doys)
      multiplier_vec <- avg_rr_vec / rr_baseline_by_age
      
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

# Add cohort start year entry
# With a historical baseline, the start year multiplier may be > 1.
# Compute the start year's RR from GCM data for proper normalization.
start_year_data_hist <- proj_data[ssp == "hist" & year == cohort_start_year]
start_year_data_proj <- proj_data[ssp %in% ssp_codes & year == cohort_start_year & year > 2014]
start_year_temps <- c(
  unlist(start_year_data_hist[, ..gcm_cols], use.names = FALSE),
  unlist(start_year_data_proj[, ..gcm_cols], use.names = FALSE)
)
start_year_doys <- c(
  rep(start_year_data_hist$doy, length(gcm_cols)),
  rep(start_year_data_proj$doy, length(gcm_cols))
)
valid_sy <- !is.na(start_year_temps)
start_year_temps <- start_year_temps[valid_sy]
start_year_doys  <- start_year_doys[valid_sy]
rr_start_year <- compute_daily_avg_rr_all_ages(start_year_temps, mmt_single_age, 0,
                                                doys = start_year_doys)
multiplier_start_year <- rr_start_year / rr_baseline_by_age
names(multiplier_start_year) <- age_range

baseline_mult <- CJ(
  year = cohort_start_year,
  age = age_range,
  ssp = ssp_codes,
  adaptation = adaptation_labels
)
baseline_mult[, rcp := rcp_labels[ssp]]
baseline_mult[, avg_rr := rr_start_year[as.character(age)], by = age]
baseline_mult[, multiplier := multiplier_start_year[as.character(age)], by = age]

cat(sprintf("  Multiplier at cohort start (age 20, %d): %.4f\n",
            cohort_start_year, multiplier_start_year["20"]))

multipliers <- rbind(baseline_mult, multipliers)
setkey(multipliers, ssp, adaptation, year, age)

cat(sprintf("\n  Computed %d multiplier records\n", nrow(multipliers)))

#------------------------------------------------------------------------------
# Helper to build a temp distribution table (same format as baseline)
#------------------------------------------------------------------------------
make_temp_dist <- function(temps) {
  dt <- data.table(tmean = temps)
  dt <- dt[!is.na(tmean)]
  dt <- dt[, .(n_days = .N), by = .(temp_bin = round(tmean))]
  dt <- dt[order(temp_bin)]
  dt[, proportion := n_days / sum(n_days)]
  dt
}

#------------------------------------------------------------------------------
# Step 11.5: Save ONE wide table of scenario-year temp distributions
#            temp_bin, rcp, n_days_2019, n_days_2020, ...
#------------------------------------------------------------------------------
cat("\nStep 11.5: Building wide temperature distribution table...\n")

if (!dir.exists("results_csv")) dir.create("results_csv")

# Helper: count days by rounded temperature bin
count_temp_bins <- function(temps) {
  dt <- data.table(tmean = temps)
  dt <- dt[!is.na(tmean)]
  dt[, .(n_days = .N), by = .(temp_bin = round(tmean))]
}

# Choose which years to include in the wide table:
# Option A: all years present in projection data (recommended)
years_all <- sort(unique(proj_data$year))

# If you want only cohort years instead, use:
# years_all <- cohort_years

wide_list <- list()

for (ssp_val in ssp_codes) {

  rcp_lab  <- rcp_labels[ssp_val]
  rcp_safe <- gsub("[^A-Za-z0-9]+", "", rcp_lab)   # e.g., "RCP45"

  ssp_data <- proj_data[ssp == ssp_val]

  for (yr in years_all) {

    year_data <- ssp_data[year == yr]
    if (nrow(year_data) == 0) next

    all_temps <- unlist(year_data[, ..gcm_cols], use.names = FALSE)
    all_temps <- all_temps[!is.na(all_temps)]
    if (length(all_temps) == 0) next

    dt_counts <- count_temp_bins(all_temps)

    # Rename n_days column to n_days_<year>
    setnames(dt_counts, "n_days", sprintf("n_days_%d", yr))

    dt_counts[, `:=`(rcp = rcp_safe)]

    wide_list[[length(wide_list) + 1]] <- dt_counts
  }
}

# Combine all (rcp, year) bin counts (wide columns already named)
wide_dt <- rbindlist(wide_list, fill = TRUE)

# Aggregate in case of duplicates (shouldn't happen, but safe)
num_cols <- setdiff(names(wide_dt), c("temp_bin", "rcp"))
wide_dt <- wide_dt[, lapply(.SD, function(x) sum(x, na.rm = TRUE)),
                   by = .(temp_bin, rcp),
                   .SDcols = num_cols]

# Ensure all year columns exist (fill missing with 0)
year_cols <- sprintf("n_days_%d", years_all)
missing_cols <- setdiff(year_cols, names(wide_dt))
for (cc in missing_cols) wide_dt[, (cc) := 0L]

# Order columns and rows nicely
setcolorder(wide_dt, c("temp_bin", "rcp", year_cols))
setorder(wide_dt, rcp, temp_bin)

# Write output
out_file <- sprintf("results_csv/temp_distribution_projection_%s.csv", city_name_lower)
fwrite(wide_dt, out_file)

cat(sprintf("  Saved: %s\n", out_file))
cat(sprintf("  Rows: %d (temp_bin × rcp)\n", nrow(wide_dt)))
cat(sprintf("  Year columns: %d\n", length(year_cols)))

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
  
  # Fill missing multipliers with 1 (years without climate data)
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
# Purchased at age 20, payments from age 65 to 84 (20 years)
# Formula: _{45|20}ä_x = sum_{k=45}^{64} v^k * k_p_x
compute_annuity_epv <- function(lt, qx_col = "qx_base") {
  n <- nrow(lt)
  v <- discount_factor
  
  # Survival probabilities
  px <- 1 - lt[[qx_col]]
  
  # k-year survival probability from age x (starting age 20)
  kpx <- cumprod(c(1, px[-n]))  # kpx[k+1] = k_p_x
  
  # Deferral: 45 years (until age 65)
  # Term: 20 years (payments at ages 65, 66, ..., 84)
  # k ranges from 45 to 64 (R indices 46 to 65)
  defer <- 45  # years until first payment
  term <- 20   # number of payments
  
  k_start <- defer      # first payment at k=45 (age 65)
  k_end <- defer + term - 1  # last payment at k=64 (age 84)
  
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
  annuity_base <- compute_annuity_epv(lt, "qx_base")
  insurance_base <- compute_insurance_epv(lt, "qx_base")
  
  # Climate-adjusted EPVs
  annuity_clim <- compute_annuity_epv(lt, "qx_clim")
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
# Step 15: Create Validation Data (Baseline Temperature Distribution)
#------------------------------------------------------------------------------

cat("\nStep 15: Creating validation data...\n")

# Temperature distribution for baseline period
temp_dist_baseline <- data.table(tmean = baseline_temps_all)
temp_dist_baseline <- temp_dist_baseline[, .(n_days = .N), by = .(temp_bin = round(tmean))]
temp_dist_baseline <- temp_dist_baseline[order(temp_bin)]
temp_dist_baseline[, proportion := n_days / sum(n_days)]

# Validation summary
validation_summary <- data.table(
  metric = c(sprintf("Mean temperature %s", baseline_temp_label),
             sprintf("Min temperature %s", baseline_temp_label),
             sprintf("Max temperature %s", baseline_temp_label),
             sprintf("Number of daily values %s", baseline_temp_label),
             sprintf("Multiplier at age 20, %d", cohort_start_year),
             sprintf("Reference RR at age 20 (%s)", baseline_temp_label),
             "Interest rate used"),
  value = c(mean(baseline_temps_all),
            min(baseline_temps_all),
            max(baseline_temps_all),
            length(baseline_temps_all),
            multiplier_start_year["20"],
            rr_baseline_by_age["20"],
            interest_rate)
)

#------------------------------------------------------------------------------
# Step 16: Save Results
#------------------------------------------------------------------------------

cat("\nStep 16: Saving results...\n")

# Create output directory if needed
if (!dir.exists("results_csv")) dir.create("results_csv")

# Save cohort life table
fwrite(output_lt, sprintf("results_csv/cohort_lifetable_climate_%s.csv", city_name_lower))
cat(sprintf("  Saved: results_csv/cohort_lifetable_climate_%s.csv\n", city_name_lower))

# Save EPV summary
fwrite(epv_summary, sprintf("results_csv/financial_impact_summary_%s.csv", city_name_lower))
cat(sprintf("  Saved: results_csv/financial_impact_summary_%s.csv\n", city_name_lower))

# Save validation data
fwrite(temp_dist_baseline, sprintf("results_csv/temp_distribution_baseline_%s.csv", city_name_lower))
cat(sprintf("  Saved: results_csv/temp_distribution_baseline_%s.csv\n", city_name_lower))

fwrite(validation_summary, sprintf("results_csv/validation_summary_%s.csv", city_name_lower))
cat(sprintf("  Saved: results_csv/validation_summary_%s.csv\n", city_name_lower))

# Save multipliers for reference
fwrite(multipliers, sprintf("results_csv/mortality_multipliers_cohort_%s.csv", city_name_lower))
cat(sprintf("  Saved: results_csv/mortality_multipliers_cohort_%s.csv\n", city_name_lower))

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
cat(sprintf("  Region: %s - NUTS 3\n", city_name))
cat("  Years: 2019-2100 (with built-in mortality improvement assumptions)\n")
cat("  Sex: Population-weighted combination of male and female\n")
cat(sprintf("  Baseline qx at age %d, %d: %.6f\n",
            cohort_start_age, cohort_start_year,
            mort_proj[year == cohort_start_year & age == cohort_start_age, qx]))
cat(sprintf("  Baseline qx at age 60, %d: %.6f\n",
            cohort_start_year,
            mort_proj[year == cohort_start_year & age == 60, qx]))
cat(sprintf("  Baseline qx at age 60, 2050: %.6f (%.1f%% improvement)\n", 
            mort_proj[year == 2050 & age == 60, qx],
            (1 - mort_proj[year == 2050 & age == 60, qx] /
                 mort_proj[year == cohort_start_year & age == 60, qx]) * 100))

cat("\n--- Baseline Temperature Reference ---\n")
cat(sprintf("  Period: %s (climatological average from GCM data)\n", baseline_temp_label))
cat(sprintf("  RR component: %s\n", rr_component))
cat(sprintf("  Mean temperature: %.2f°C\n", mean(baseline_temps_all)))
cat(sprintf("  Reference RR at age 20: %.4f\n", rr_baseline_by_age["20"]))

cat("\n--- Validation ---\n")
cat(sprintf("  Climate mortality multiplier at age 20, %d: %.6f\n",
            cohort_start_year, multiplier_start_year["20"]))
cat("  (With a historical baseline, start-year multiplier > 1 is expected)\n")

cat("\n--- Financial Impact Summary (% Change vs Baseline) ---\n")
cat("\nDeferred Term Annuity-Due (45|20 äx, payments ages 65-84):\n")
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
cat("\nAdaptation = 0% (No Adaptation):\n")
summary_0 <- epv_summary[adaptation == "0%", 
                          .(rcp, 
                            annuity_pct = sprintf("%+.2f", pct_delta_annuity),
                            insurance_pct = sprintf("%+.2f", pct_delta_insurance),
                            reserve_annuity_pct = sprintf("%+.2f", pct_delta_reserve_annuity),
                            reserve_ins_pct = sprintf("%+.2f", pct_delta_reserve_ins))]
print(summary_0)

cat("\nAdaptation = 50%:\n")
summary_50 <- epv_summary[adaptation == "50%", 
                           .(rcp, 
                             annuity_pct = sprintf("%+.2f", pct_delta_annuity),
                             insurance_pct = sprintf("%+.2f", pct_delta_insurance),
                             reserve_annuity_pct = sprintf("%+.2f", pct_delta_reserve_annuity),
                             reserve_ins_pct = sprintf("%+.2f", pct_delta_reserve_ins))]
print(summary_50)

cat("\nAdaptation = 90%:\n")
summary_90 <- epv_summary[adaptation == "90%", 
                           .(rcp, 
                             annuity_pct = sprintf("%+.2f", pct_delta_annuity),
                             insurance_pct = sprintf("%+.2f", pct_delta_insurance),
                             reserve_annuity_pct = sprintf("%+.2f", pct_delta_reserve_annuity),
                             reserve_ins_pct = sprintf("%+.2f", pct_delta_reserve_ins))]
print(summary_90)

cat("\n" %+% Rep("=", 70) %+% "\n")
cat("DONE!\n")
cat(Rep("=", 70) %+% "\n")
