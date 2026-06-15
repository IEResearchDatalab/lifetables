################################################################################
# Simon Project: Modular Pipeline Functions
# 
# All reusable functions for the multi-city, multi-scenario pipeline
# 
# FIXED: Variable name collision in compute_multipliers_for_scenario
################################################################################

library(data.table)
library(arrow)
library(dlnm)
library(splines)

# ============================================================================
# 1. CITY PRECOMPUTATION FUNCTIONS
# ============================================================================

#' Precompute all city-specific data (stage 1)
precompute_city <- function(city_code, 
                           coefs_path = "data/coefs.csv",
                           temp_path = "data/tmeanproj.gz.parquet",
                           gcmexcl = c("CMCC_CM2_SR5", "TaiESM1"),
                           output_dir = "precomputed/cities") {
  
  cat(sprintf("[%s] Starting precomputation for %s\n", Sys.time(), city_code))
  
  # Load coefficient functions
  source("R/load_coefficients.R")
  source("R/load_data.R")
  
  # 1. Load and interpolate ERF coefficients
  coefs <- load_city_coefficients(city_code, coefs_path = coefs_path)
  
  agelabs <- c("20-44", "45-64", "65-74", "75-84", "85+")
  age_midpoints <- c(32.5, 55, 70, 80, 92.5)
  age_range <- 20:100
  
  coefs_single_age <- interpolate_coefs_to_single_age(
    coefs$city, coefs$all,
    agelabs, age_midpoints,
    age_range = age_range,
    city_code = city_code
  )
  
  age_coefs <- coefs_single_age[, .(agegroup, b1, b2, b3, b4, b5)]
  
  # 2. Load temperature data
  temp_data <- load_projected_temperatures(city_code, 
                                          parquet_path = temp_path,
                                          gcmexcl = gcmexcl)
  
  proj_data <- temp_data$proj_data
  gcm_cols <- temp_data$gcm_cols
  
  # 3. Define basis parameters
  varfun <- "bs"
  vardegree <- 2
  varper <- c(10, 75, 90)
  
  hist_temps <- unlist(proj_data[ssp == "hist", ..gcm_cols], use.names = FALSE)
  hist_temps <- hist_temps[!is.na(hist_temps)]
  
  varknots <- quantile(hist_temps, varper / 100, na.rm = TRUE)
  varbound <- range(hist_temps, na.rm = TRUE)
  
  argvar <- list(fun = varfun, degree = vardegree, 
                 knots = varknots, Bound = varbound)
  
  # 4. Compute MMT by age
  temp_seq <- seq(varbound[1], varbound[2], by = 0.1)
  basis_seq <- do.call(onebasis, c(list(x = temp_seq), argvar))
  
  mmt_by_age <- compute_mmt_for_ages(age_coefs, basis_seq, temp_seq)
  
  # 5. Compute baseline RR
  rr_baseline <- compute_baseline_rr(proj_data, gcm_cols, age_coefs, 
                                     mmt_by_age, argvar)
  
  # 6. Package precomputed data
  city_precomputed <- list(
    city_code = city_code,
    # Temperature data
    proj_data = proj_data,
    gcm_cols = gcm_cols,
    # ERF data
    age_coefs = age_coefs,
    mmt_by_age = mmt_by_age,
    # Baseline
    rr_baseline = rr_baseline,
    # Basis parameters
    argvar = argvar,
    varknots = varknots,
    varbound = varbound,
    # Metadata
    timestamp = Sys.time()
  )
  
  # 7. Save
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  output_file <- file.path(output_dir, sprintf("%s.rds", city_code))
  saveRDS(city_precomputed, output_file, compress = "xz")
  
  cat(sprintf("[%s] Completed %s (%.1f MB)\n", 
              Sys.time(), city_code, 
              file.size(output_file) / 1024^2))
  
  # Clean up
  rm(proj_data, temp_data)
  gc()
  
  return(output_file)
}

#' Compute MMT for all ages
compute_mmt_for_ages <- function(age_coefs, basis_seq, temp_seq) {
  
  mmt_by_age <- age_coefs[, {
    coefs_vec <- as.numeric(.SD[1, .(b1, b2, b3, b4, b5)])
    log_rr_seq <- basis_seq %*% coefs_vec
    
    ind <- temp_seq >= quantile(temp_seq, 0.25) &
           temp_seq <= quantile(temp_seq, 0.99)
    
    mmt <- temp_seq[ind][which.min(log_rr_seq[ind])]
    .(mmt = mmt)
  }, by = agegroup]
  
  return(mmt_by_age)
}

#' Compute baseline RR from historical data
compute_baseline_rr <- function(proj_data, gcm_cols, age_coefs, 
                               mmt_by_age, argvar) {
  
  # Extract historical temperatures
  baseline_period <- proj_data[ssp == "hist"]
  
  baseline_temps <- melt(
    baseline_period,
    id.vars = c("URAU_CODE", "date", "year", "doy"),
    measure.vars = gcm_cols,
    variable.name = "gcm",
    value.name = "temp"
  )
  
  # Cross-join with ages
  baseline_temps[, key := 1]
  age_coefs[, key := 1]
  
  dt_baseline <- merge(baseline_temps, age_coefs, by = "key", 
                       allow.cartesian = TRUE)
  dt_baseline[, key := NULL]
  
  # Compute RR
  basis_baseline <- do.call(onebasis, c(list(x = dt_baseline$temp), argvar))
  coef_mat <- as.matrix(dt_baseline[, .(b1, b2, b3, b4, b5)])
  
  dt_baseline <- merge(dt_baseline, mmt_by_age, by = "agegroup")
  
  basis_at_temp <- do.call(onebasis, c(list(x = dt_baseline$temp), argvar))
  basis_at_mmt <- do.call(onebasis, c(list(x = dt_baseline$mmt), argvar))
  
  log_rr_centered <- rowSums((basis_at_temp - basis_at_mmt) * coef_mat)
  dt_baseline[, RR := pmax(exp(log_rr_centered), 1)]
  
  # Aggregate to average RR by age
  rr_baseline <- dt_baseline[, .(
    RR_baseline = mean(RR, na.rm = TRUE)
  ), by = agegroup]
  
  # Clean up
  rm(dt_baseline, baseline_temps, basis_baseline, basis_at_temp, basis_at_mmt)
  gc()
  
  return(rr_baseline)
}

# ============================================================================
# 2. SCENARIO COMPUTATION FUNCTIONS
# ============================================================================

#' Compute single scenario: city × SSP × adaptation
compute_scenario <- function(city_code, target_ssp, adaptation,
                            precomputed_dir = "precomputed/cities",
                            mort_proj_dir = "data/mortality_projections",
                            output_summary = TRUE,
                            output_detailed = FALSE,
                            cohort_birth_year = 2020,
                            cohort_start_age = 20,
                            radix = 100000) {
  
  start_time <- Sys.time()
  
  tryCatch({
    # 1. Load precomputed data
    precomp_file <- file.path(precomputed_dir, sprintf("%s.rds", city_code))
    city_data <- readRDS(precomp_file)
    
    # 2. Compute multipliers
    multipliers <- compute_multipliers_for_scenario(
      city_data, target_ssp, adaptation
    )
    
    # 3. Load baseline mortality
    mort_proj <- load_baseline_mortality(city_code, mort_proj_dir)
    
    # 4. Apply multipliers
    mort_climate <- merge(
      mort_proj,
      multipliers[, .(age, year, multiplier = multiplier_mean)],
      by = c("age", "year"),
      all.x = TRUE
    )
    mort_climate[is.na(multiplier), multiplier := 1.0]
    mort_climate[, mx_climate := mx * multiplier]
    
    # 5. Build cohort life table
    lifetable <- build_cohort_lifetable(
      mort_climate,
      birth_year = cohort_birth_year,
      start_age = cohort_start_age,
      radix = radix
    )
    
    # 6. Compute lifespan inequality
    li_results <- compute_lifespan_inequality(
      lifetable, cohort_start_age, radix
    )
    
    # 7. Add metadata
    li_results[, `:=`(
      city_code = city_code,
      ssp = target_ssp,
      adaptation = adaptation,
      runtime_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    )]
    
    # 8. Save detailed outputs (optional)
    if (output_detailed) {
      save_detailed_outputs(city_code, target_ssp, adaptation, 
                           lifetable, multipliers, li_results)
    }
    
    cat(sprintf("[%s] Completed %s | SSP%d | Adapt%.0f%% | %.1fs\n",
                Sys.time(), city_code, target_ssp, adaptation * 100,
                li_results$runtime_sec))
    
    return(li_results)
    
  }, error = function(e) {
    cat(sprintf("[ERROR] %s | SSP%d | Adapt%.0f%%: %s\n",
                city_code, target_ssp, adaptation * 100, e$message))
    return(NULL)
  })
}

#' Compute mortality multipliers for a scenario
compute_multipliers_for_scenario <- function(city_data, target_ssp, adaptation) {
  
  # Filter to selected SSP (FIXED: renamed parameter to avoid collision)
  proj_data_ssp <- city_data$proj_data[city_data$proj_data$ssp == target_ssp]
  
  if (nrow(proj_data_ssp) == 0) {
    stop(sprintf("No data found for SSP%d", target_ssp))
  }
  
  # Reshape to long
  temp_long <- melt(
    proj_data_ssp,
    id.vars = c("URAU_CODE", "date", "year", "doy", "ssp"),
    measure.vars = city_data$gcm_cols,
    variable.name = "gcm",
    value.name = "temp"
  )
  
  # Cross-join with ages
  temp_long[, key := 1]
  city_data$age_coefs[, key := 1]
  
  dt <- merge(temp_long, city_data$age_coefs, by = "key", allow.cartesian = TRUE)
  dt[, key := NULL]
  
  # Compute RR
  basis <- do.call(onebasis, c(list(x = dt$temp), city_data$argvar))
  coef_mat <- as.matrix(dt[, .(b1, b2, b3, b4, b5)])
  
  dt <- merge(dt, city_data$mmt_by_age, by = "agegroup")
  
  basis_at_temp <- do.call(onebasis, c(list(x = dt$temp), city_data$argvar))
  basis_at_mmt <- do.call(onebasis, c(list(x = dt$mmt), city_data$argvar))
  
  log_rr_centered <- rowSums((basis_at_temp - basis_at_mmt) * coef_mat)
  dt[, RR := pmax(exp(log_rr_centered), 1)]
  
  # Apply adaptation
  dt[, adapt_factor := fifelse(
    year < 2030, 0,
    fifelse(year > 2100, adaptation,
            adaptation * (year - 2030) / (2100 - 2030))
  )]
  
  dt[, RR_adapted := 1 + (RR - 1) * (1 - adapt_factor)]
  
  # Aggregate to annual
  dt_annual <- dt[, .(
    RR_mean = mean(RR_adapted, na.rm = TRUE)
  ), by = .(year, agegroup, gcm)]
  
  # Merge baseline
  dt_annual <- merge(dt_annual, city_data$rr_baseline, by = "agegroup")
  
  # Compute multipliers
  dt_annual[, multiplier := RR_mean / RR_baseline]
  
  # Ensemble statistics
  multipliers <- dt_annual[, .(
    multiplier_mean = mean(multiplier, na.rm = TRUE),
    multiplier_sd   = sd(multiplier, na.rm = TRUE),
    multiplier_q05  = quantile(multiplier, 0.05, na.rm = TRUE),
    multiplier_q95  = quantile(multiplier, 0.95, na.rm = TRUE)
  ), by = .(year, agegroup)]
  
  multipliers[, age := as.integer(agegroup)]
  
  # Clean up
  rm(dt, dt_annual, basis, basis_at_temp, basis_at_mmt, coef_mat)
  gc()
  
  return(multipliers)
}

#' Load baseline mortality (Eurostat or synthetic)
load_baseline_mortality <- function(city_code, mort_proj_dir) {
  
  # Extract country code (first 2 characters)
  country_code <- substr(city_code, 1, 2)
  
  # Try to load Eurostat data
  mort_file <- file.path(mort_proj_dir, sprintf("%s_mortality.csv", country_code))
  
  if (file.exists(mort_file)) {
    mort_proj <- fread(mort_file)
    mort_proj <- mort_proj[age >= 20 & age <= 100 & year >= 2015 & year <= 2100]
  } else {
    # Generate synthetic Gompertz mortality
    mort_proj <- CJ(age = 20:100, year = 2015:2100)
    
    a <- 0.0001
    b <- 0.08
    mort_proj[, mx := a * exp(b * age) * (0.99 ^ ((year - 2015) / 10))]
  }
  
  setkey(mort_proj, year, age)
  return(mort_proj)
}

#' Build cohort life table
build_cohort_lifetable <- function(mort_data, birth_year, start_age, radix = 100000) {
  
  mort_data <- copy(mort_data)
  mort_data[, cohort_year := year - age]
  
  cohort_data <- mort_data[cohort_year == birth_year & age >= start_age]
  setorder(cohort_data, age)
  
  if (nrow(cohort_data) == 0) {
    stop("No data found for this cohort")
  }
  
  n <- nrow(cohort_data)
  
  # Convert mx to qx
  cohort_data[, qx_base := mx / (1 + 0.5 * mx)]
  cohort_data[, qx_climate := mx_climate / (1 + 0.5 * mx_climate)]
  
  cohort_data[qx_base > 1, qx_base := 1]
  cohort_data[qx_climate > 1, qx_climate := 1]
  
  # Initialize
  cohort_data[, lx_base := as.numeric(NA)]
  cohort_data[, lx_climate := as.numeric(NA)]
  
  cohort_data[1, lx_base := radix]
  cohort_data[1, lx_climate := radix]
  
  # Iterative computation
  for (i in 1:n) {
    if (i < n) {
      cohort_data[i, dx_base := lx_base * qx_base]
      cohort_data[i + 1, lx_base := cohort_data[i, lx_base - dx_base]]
      
      cohort_data[i, dx_climate := lx_climate * qx_climate]
      cohort_data[i + 1, lx_climate := cohort_data[i, lx_climate - dx_climate]]
    } else {
      cohort_data[i, dx_base := lx_base]
      cohort_data[i, dx_climate := lx_climate]
    }
  }
  
  # Person-years lived
  cohort_data[, Lx_base := (lx_base + c(lx_base[-1], 0)) / 2]
  cohort_data[, Lx_climate := (lx_climate + c(lx_climate[-1], 0)) / 2]
  
  cohort_data[n, Lx_base := ifelse(mx > 0, lx_base / mx, lx_base)]
  cohort_data[n, Lx_climate := ifelse(mx_climate > 0, lx_climate / mx_climate, lx_climate)]
  
  # Life expectancy
  cohort_data[, Tx_base := rev(cumsum(rev(Lx_base)))]
  cohort_data[, Tx_climate := rev(cumsum(rev(Lx_climate)))]
  
  cohort_data[, ex_base := Tx_base / lx_base]
  cohort_data[, ex_climate := Tx_climate / lx_climate]
  
  return(cohort_data)
}

#' Compute lifespan inequality metrics
compute_lifespan_inequality <- function(lifetable, start_age, radix) {
  
  # Life expectancy
  e20_base <- lifetable[age == start_age, ex_base]
  e20_climate <- lifetable[age == start_age, ex_climate]
  
  # Gini coefficient
  gini_base <- compute_gini(lifetable$age, lifetable$dx_base)
  gini_climate <- compute_gini(lifetable$age, lifetable$dx_climate)
  
  # Standard deviation
  sd_base <- compute_sd_age_at_death(lifetable$age, lifetable$dx_base)
  sd_climate <- compute_sd_age_at_death(lifetable$age, lifetable$dx_climate)
  
  # IQR
  iqr_base <- compute_iqr_age_at_death(lifetable$age, lifetable$dx_base)
  iqr_climate <- compute_iqr_age_at_death(lifetable$age, lifetable$dx_climate)
  
  # Threshold analysis
  lifetable[, diff_from_e20 := abs(ex_base - e20_base)]
  threshold_age <- lifetable[which.min(diff_from_e20), age]
  
  lifetable[, below_threshold := age < threshold_age]
  lifetable[, excess_deaths := dx_climate - dx_base]
  
  excess_below <- sum(lifetable[below_threshold == TRUE, excess_deaths])
  excess_above <- sum(lifetable[below_threshold == FALSE, excess_deaths])
  total_excess <- sum(lifetable$excess_deaths)
  
  # Package results
  results <- data.table(
    # Life expectancy
    e20_base = e20_base,
    e20_climate = e20_climate,
    delta_e20 = e20_climate - e20_base,
    pct_delta_e20 = (e20_climate - e20_base) / e20_base * 100,
    
    # Gini
    gini_base = gini_base,
    gini_climate = gini_climate,
    delta_gini = gini_climate - gini_base,
    pct_change_gini = (gini_climate - gini_base) / gini_base * 100,
    
    # SD
    sd_base = sd_base,
    sd_climate = sd_climate,
    delta_sd = sd_climate - sd_base,
    pct_change_sd = (sd_climate - sd_base) / sd_base * 100,
    
    # IQR
    iqr_base = iqr_base$iqr,
    iqr_climate = iqr_climate$iqr,
    delta_iqr = iqr_climate$iqr - iqr_base$iqr,
    
    # Threshold
    threshold_age = threshold_age,
    
    # Excess deaths
    total_excess_deaths = total_excess,
    excess_below_threshold = excess_below,
    excess_above_threshold = excess_above,
    pct_below = excess_below / total_excess * 100,
    pct_above = excess_above / total_excess * 100
  )
  
  return(results)
}

# Helper functions for inequality metrics

compute_gini <- function(ages, deaths) {
  valid <- deaths > 0
  ages <- ages[valid]
  deaths <- deaths[valid]
  
  n <- length(ages)
  total_deaths <- sum(deaths)
  mean_age <- sum(ages * deaths) / total_deaths
  
  numerator <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      numerator <- numerator + abs(ages[i] - ages[j]) * deaths[i] * deaths[j]
    }
  }
  
  denominator <- 2 * mean_age * total_deaths^2
  gini <- numerator / denominator
  return(gini)
}

compute_sd_age_at_death <- function(ages, deaths) {
  total_deaths <- sum(deaths)
  mean_age <- sum(ages * deaths) / total_deaths
  variance <- sum((ages - mean_age)^2 * deaths) / total_deaths
  sd <- sqrt(variance)
  return(sd)
}

compute_iqr_age_at_death <- function(ages, deaths) {
  cumsum_deaths <- cumsum(deaths)
  total_deaths <- sum(deaths)
  
  q25_idx <- which(cumsum_deaths >= 0.25 * total_deaths)[1]
  q25 <- ages[q25_idx]
  
  q75_idx <- which(cumsum_deaths >= 0.75 * total_deaths)[1]
  q75 <- ages[q75_idx]
  
  iqr <- q75 - q25
  return(list(q25 = q25, q75 = q75, iqr = iqr))
}

#' Save detailed outputs for a scenario
save_detailed_outputs <- function(city_code, ssp, adaptation, 
                                  lifetable, multipliers, li_results,
                                  output_dir = "results/detailed") {
  
  # Create directories
  dir.create(file.path(output_dir, "lifetables"), 
             showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(output_dir, "multipliers"), 
             showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(output_dir, "excess_deaths"), 
             showWarnings = FALSE, recursive = TRUE)
  
  # Save life table
  if (!is.null(lifetable)) {
    lt_path <- sprintf("%s/lifetables/%s_ssp%d_adapt%02d.csv",
                       output_dir, city_code, ssp, adaptation * 100)
    fwrite(lifetable, lt_path)
  }
  
  # Save multipliers
  if (!is.null(multipliers)) {
    mult_path <- sprintf("%s/multipliers/%s_ssp%d_adapt%02d.csv",
                         output_dir, city_code, ssp, adaptation * 100)
    fwrite(multipliers, mult_path)
  }
  
  # Save age-specific excess deaths
  if (!is.null(lifetable)) {
    excess <- lifetable[, .(
      age, year,
      dx_base, dx_climate,
      excess_deaths = dx_climate - dx_base,
      multiplier
    )]
    
    excess_path <- sprintf("%s/excess_deaths/%s_ssp%d_adapt%02d.csv",
                           output_dir, city_code, ssp, adaptation * 100)
    fwrite(excess, excess_path)
  }
  
  invisible(TRUE)
}
