################################################################################
# Simon Project: Modular Pipeline Functions (Male baseline, GCM-specific outputs)
#
# WHAT THIS VERSION DOES
# - Keeps the overall pipeline structure unchanged.
# - Uses Eurostat male mortality as the baseline mortality input.
# - Removes the synthetic mortality fallback.
# - Computes mortality multipliers separately for each GCM.
# - Builds one life table per city × SSP × adaptation × GCM.
# - Writes a summary CSV with one row per city × SSP × adaptation × GCM.
# - Uses constant extension in years and ages for the mortality and multiplier tail.
# - Uses a cleaned inequality summary (LE, Gini, SD, IQR) without threshold outputs.
################################################################################

library(data.table)
library(arrow)
library(dlnm)
library(splines)
library(eurostat)

# ============================================================================
# 0. HELPERS
# ============================================================================

safe_id <- function(x) {
  gsub("[^A-Za-z0-9_]+", "_", x)
}

extend_year_constant <- function(dt, target_year_min = NULL, target_year_max = NULL) {
  dt <- copy(dt)
  source_year_min <- min(dt$year)
  source_year_max <- max(dt$year)
  out <- copy(dt)

  if (!is.null(target_year_min) && target_year_min < source_year_min) {
    template_min <- dt[year == source_year_min]
    past_years <- target_year_min:(source_year_min - 1L)
    past_ext <- rbindlist(lapply(past_years, function(y) {
      tmp <- copy(template_min)
      tmp[, year := y]
      tmp
    }))
    out <- rbindlist(list(out, past_ext), use.names = TRUE)
  }

  if (!is.null(target_year_max) && target_year_max > source_year_max) {
    template_max <- dt[year == source_year_max]
    future_years <- (source_year_max + 1L):target_year_max
    future_ext <- rbindlist(lapply(future_years, function(y) {
      tmp <- copy(template_max)
      tmp[, year := y]
      tmp
    }))
    out <- rbindlist(list(out, future_ext), use.names = TRUE)
  }

  out <- unique(out, by = c("age", "year", intersect("gcm", names(out))))
  setorder(out, year, age)
  out
}

extend_age_constant <- function(dt, target_age_max, source_age_max = max(dt$age)) {
  dt <- copy(dt)
  out <- copy(dt)

  if (target_age_max > source_age_max) {
    template_max <- dt[age == source_age_max]
    future_ages <- (source_age_max + 1L):target_age_max
    age_ext <- rbindlist(lapply(future_ages, function(a) {
      tmp <- copy(template_max)
      tmp[, age := a]
      if ("agegroup" %in% names(tmp)) tmp[, agegroup := a]
      if ("decade" %in% names(tmp)) tmp[, decade := (a %/% 10L) * 10L]
      tmp
    }))
    out <- rbindlist(list(out, age_ext), use.names = TRUE, fill = TRUE)
  }

  out <- unique(out, by = c("age", "year", intersect("gcm", names(out))))
  setorder(out, year, age)
  out
}

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
    proj_data = proj_data,
    gcm_cols = gcm_cols,
    age_coefs = age_coefs,
    mmt_by_age = mmt_by_age,
    rr_baseline = rr_baseline,
    argvar = argvar,
    varknots = varknots,
    varbound = varbound,
    timestamp = Sys.time()
  )

  # 7. Save
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  output_file <- file.path(output_dir, sprintf("%s.rds", city_code))
  saveRDS(city_precomputed, output_file, compress = "xz")

  cat(sprintf("[%s] Completed %s (%.1f MB)\n",
              Sys.time(), city_code,
              file.size(output_file) / 1024^2))

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

  baseline_period <- proj_data[ssp == "hist"]

  baseline_temps <- melt(
    baseline_period,
    id.vars = c("URAU_CODE", "date", "year", "doy"),
    measure.vars = gcm_cols,
    variable.name = "gcm",
    value.name = "temp"
  )

  baseline_temps[, key := 1]
  age_coefs[, key := 1]

  dt_baseline <- merge(baseline_temps, age_coefs, by = "key",
                       allow.cartesian = TRUE)
  dt_baseline[, key := NULL]

  coef_mat <- as.matrix(dt_baseline[, .(b1, b2, b3, b4, b5)])
  dt_baseline <- merge(dt_baseline, mmt_by_age, by = "agegroup")

  basis_at_temp <- do.call(onebasis, c(list(x = dt_baseline$temp), argvar))
  basis_at_mmt <- do.call(onebasis, c(list(x = dt_baseline$mmt), argvar))

  log_rr_centered <- rowSums((basis_at_temp - basis_at_mmt) * coef_mat)
  dt_baseline[, RR := pmax(exp(log_rr_centered), 1)]

  rr_baseline <- dt_baseline[, .(
    RR_baseline = mean(RR, na.rm = TRUE)
  ), by = agegroup]

  rm(dt_baseline, baseline_temps, basis_at_temp, basis_at_mmt, coef_mat)
  gc()

  return(rr_baseline)
}

# ============================================================================
# 2. SCENARIO COMPUTATION FUNCTIONS
# ============================================================================

#' Compute single scenario: city × SSP × adaptation, returning one row per GCM
compute_scenario <- function(city_code, target_ssp, adaptation,
                            precomputed_dir = "precomputed/cities",
                            mort_proj_dir = "data/mortality_projections",
                            output_summary = TRUE,
                            output_detailed = FALSE,
                            cohort_birth_year = 2002,
                            cohort_start_age = 20,
                            cohort_terminal_age = 120,
                            radix = 100000,
                            sex_code = "M",
                            baseline_year_min = 2022,
                            baseline_year_max = 2100,
                            age_min = 20,
                            age_max_source = 100,
                            age_max = 120,
                            extend_baseline_constant = TRUE,
                            extend_multiplier_constant = TRUE,
                            summary_dir = "results/summary") {

  start_time <- Sys.time()

  tryCatch({
    precomp_file <- file.path(precomputed_dir, sprintf("%s.rds", city_code))
    city_data <- readRDS(precomp_file)

    requested_year_min <- cohort_birth_year + cohort_start_age
    requested_year_max <- cohort_birth_year + cohort_terminal_age

    # 1. Compute multipliers by GCM
    multipliers <- compute_multipliers_by_gcm_for_scenario(
      city_data, target_ssp, adaptation
    )

    mult_source_age_max <- max(multipliers$age)

    if (extend_multiplier_constant) {
      multipliers <- extend_year_constant(
        multipliers,
        target_year_min = requested_year_min,
        target_year_max = requested_year_max
      )
    }

    if (cohort_terminal_age > mult_source_age_max) {
      multipliers <- extend_age_constant(
        multipliers,
        target_age_max = cohort_terminal_age,
        source_age_max = mult_source_age_max
      )
    }

    # 2. Load male baseline mortality from Eurostat
    mort_proj <- load_baseline_mortality(
      city_code = city_code,
      mort_proj_dir = mort_proj_dir,
      sex_code = sex_code,
      baseline_year_min = baseline_year_min,
      baseline_year_max = baseline_year_max,
      requested_year_min = requested_year_min,
      requested_year_max = requested_year_max,
      age_min = age_min,
      age_max_source = age_max_source,
      age_max = age_max,
      extend_baseline_constant = extend_baseline_constant
    )

    # 3. Apply multipliers by GCM
    mort_climate <- merge(
      mort_proj,
      multipliers[, .(age, year, gcm, multiplier)],
      by = c("age", "year"),
      all.x = TRUE,
      allow.cartesian = TRUE
    )

    if (anyNA(mort_climate$multiplier)) {
      stop("Missing climate multipliers remain after merge.")
    }

    mort_climate[, mx_climate := mx * multiplier]

    # 4. Build one life table per GCM and summarize
    gcm_ids <- sort(unique(mort_climate$gcm))

    results_list <- lapply(gcm_ids, function(gcm_id) {
      gcm_data <- mort_climate[gcm == gcm_id]

      lifetable <- build_cohort_lifetable(
        gcm_data,
        birth_year = cohort_birth_year,
        start_age = cohort_start_age,
        terminal_age = cohort_terminal_age,
        radix = radix
      )

      li_results <- compute_lifespan_inequality(
        lifetable,
        start_age = cohort_start_age
      )

      li_results[, `:=`(
        city_code = city_code,
        ssp = target_ssp,
        adaptation = adaptation,
        gcm = gcm_id,
        sex = sex_code,
        cohort_birth_year = cohort_birth_year,
        cohort_start_age = cohort_start_age,
        cohort_terminal_age = cohort_terminal_age,
        runtime_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      )]

      if (output_detailed) {
        save_detailed_outputs(
          city_code = city_code,
          ssp = target_ssp,
          adaptation = adaptation,
          gcm = gcm_id,
          lifetable = lifetable,
          multipliers = multipliers[gcm == gcm_id],
          li_results = li_results
        )
      }

      li_results
    })

    scenario_results <- rbindlist(results_list, use.names = TRUE, fill = TRUE)

    # 5. Save / append summary with one line per city × SSP × adaptation × GCM
    if (output_summary) {
      dir.create(summary_dir, showWarnings = FALSE, recursive = TRUE)
      summary_path <- file.path(summary_dir, "lifespan_inequality_by_gcm.csv")

      if (file.exists(summary_path)) {
        fwrite(scenario_results, summary_path, append = TRUE)
      } else {
        fwrite(scenario_results, summary_path)
      }
    }

    cat(sprintf("[%s] Completed %s | SSP%d | Adapt%.0f%% | Sex=%s | GCMs=%d | %.1fs\n",
                Sys.time(), city_code, target_ssp, adaptation * 100,
                sex_code, length(gcm_ids),
                as.numeric(difftime(Sys.time(), start_time, units = "secs"))))

    return(scenario_results)

  }, error = function(e) {
    cat(sprintf("[ERROR] %s | SSP%d | Adapt%.0f%% | Sex=%s: %s\n",
                city_code, target_ssp, adaptation * 100, sex_code, e$message))
    return(NULL)
  })
}

#' Compute mortality multipliers by GCM for a scenario
compute_multipliers_by_gcm_for_scenario <- function(city_data, target_ssp, adaptation) {

  proj_data_ssp <- city_data$proj_data[city_data$proj_data$ssp == target_ssp]

  if (nrow(proj_data_ssp) == 0) {
    stop(sprintf("No data found for SSP%d", target_ssp))
  }

  temp_long <- melt(
    proj_data_ssp,
    id.vars = c("URAU_CODE", "date", "year", "doy", "ssp"),
    measure.vars = city_data$gcm_cols,
    variable.name = "gcm",
    value.name = "temp"
  )

  temp_long[, key := 1]
  city_data$age_coefs[, key := 1]

  dt <- merge(temp_long, city_data$age_coefs, by = "key", allow.cartesian = TRUE)
  dt[, key := NULL]

  coef_mat <- as.matrix(dt[, .(b1, b2, b3, b4, b5)])
  dt <- merge(dt, city_data$mmt_by_age, by = "agegroup")

  basis_at_temp <- do.call(onebasis, c(list(x = dt$temp), city_data$argvar))
  basis_at_mmt <- do.call(onebasis, c(list(x = dt$mmt), city_data$argvar))

  log_rr_centered <- rowSums((basis_at_temp - basis_at_mmt) * coef_mat)
  dt[, RR := pmax(exp(log_rr_centered), 1)]

  dt[, adapt_factor := fifelse(
    year < 2030, 0,
    fifelse(year > 2100, adaptation,
            adaptation * (year - 2030) / (2100 - 2030))
  )]

  dt[, RR_adapted := 1 + (RR - 1) * (1 - adapt_factor)]

  dt_annual <- dt[, .(
    RR_mean = mean(RR_adapted, na.rm = TRUE)
  ), by = .(year, agegroup, gcm)]

  dt_annual <- merge(dt_annual, city_data$rr_baseline, by = "agegroup")
  dt_annual[, multiplier := RR_mean / RR_baseline]
  dt_annual[, age := as.integer(agegroup)]

  multipliers <- dt_annual[, .(
    year,
    age,
    agegroup,
    gcm,
    multiplier
  )]

  rm(dt, dt_annual, temp_long, basis_at_temp, basis_at_mmt, coef_mat)
  gc()

  return(multipliers)
}

#' Load baseline mortality from Eurostat (male by default), with constant year and age extension
load_baseline_mortality <- function(city_code,
                                    mort_proj_dir,
                                    sex_code = "M",
                                    baseline_year_min = 2022,
                                    baseline_year_max = 2100,
                                    requested_year_min = 2022,
                                    requested_year_max = 2100,
                                    age_min = 20,
                                    age_max_source = 100,
                                    age_max = 120,
                                    extend_baseline_constant = TRUE) {

  country_code <- substr(city_code, 1, 2)
  sex_label <- if (sex_code == "F") {
    "female"
  } else if (sex_code == "M") {
    "male"
  } else {
    stop("Unsupported sex_code: ", sex_code)
  }

  mort_file <- file.path(
    mort_proj_dir,
    sprintf("%s_mortality_%s.csv", country_code, sex_label)
  )

  if (file.exists(mort_file)) {
    mort_proj <- fread(mort_file)
  } else {
    raw <- as.data.table(eurostat::get_eurostat(
      id = "proj_23naasmr",
      filters = list(
        geo = country_code,
        projection = "BSL",
        sex = sex_code,
        sinceTimePeriod = baseline_year_min,
        untilTimePeriod = baseline_year_max
      ),
      time_format = "num",
      type = "code",
      stringsAsFactors = FALSE
    ))

    if (nrow(raw) == 0L) {
      stop("Eurostat returned zero rows for country=", country_code, ", sex=", sex_code)
    }
    if (nrow(raw[, .N, by = .(age, time)][N > 1L]) > 0L) {
      stop("Eurostat returned duplicate age-year rows after filtering.")
    }

    raw[, age_num := fifelse(
      age == "Y_LT1", 0L,
      fifelse(age == "Y_GE100", 100L, as.integer(sub("^Y", "", age)))
    )]

    mort_proj <- raw[
      age_num >= age_min & age_num <= age_max_source,
      .(
        age = age_num,
        year = as.integer(time),
        mx = as.numeric(values)
      )
    ]

    if (nrow(mort_proj) == 0L) {
      stop("No mortality rows left after Eurostat conversion.")
    }

    dir.create(mort_proj_dir, recursive = TRUE, showWarnings = FALSE)
    fwrite(mort_proj, mort_file)
  }

  if (extend_baseline_constant) {
    mort_proj <- extend_year_constant(
      mort_proj,
      target_year_min = requested_year_min,
      target_year_max = requested_year_max
    )
  }

  if (age_max > age_max_source) {
    mort_proj <- extend_age_constant(
      mort_proj,
      target_age_max = age_max,
      source_age_max = age_max_source
    )
  }

  mort_proj <- mort_proj[
    age >= age_min & age <= age_max &
    year >= requested_year_min & year <= requested_year_max
  ]

  if (nrow(mort_proj) == 0L) {
    stop("Baseline mortality table is empty after extension/filtering.")
  }
  if (nrow(mort_proj[, .N, by = .(age, year)][N > 1L]) > 0L) {
    stop("Baseline mortality table contains duplicate age-year rows.")
  }
  if (anyNA(mort_proj$mx)) {
    stop("Baseline mortality table contains missing mx values.")
  }

  setkey(mort_proj, year, age)
  mort_proj
}

#' Build cohort life table with a consistent closed final interval
build_cohort_lifetable <- function(mort_data, birth_year, start_age, terminal_age, radix = 100000) {

  mort_data <- copy(mort_data)
  mort_data[, cohort_year := year - age]

  cohort_data <- mort_data[
    cohort_year == birth_year & age >= start_age & age <= terminal_age
  ]
  setorder(cohort_data, age)

  if (nrow(cohort_data) == 0L) {
    stop("No data found for this cohort")
  }

  expected_ages <- as.integer(start_age:terminal_age)
  expected_years <- as.integer(birth_year + expected_ages)

  if (nrow(cohort_data) != length(expected_ages) ||
      !all(cohort_data$age == expected_ages) ||
      !all(cohort_data$year == expected_years)) {

    expected_path <- data.table(age = expected_ages, year = expected_years)
    observed_path <- unique(cohort_data[, .(age, year)])
    missing_path <- expected_path[!observed_path, on = .(age, year)]
    extra_path <- observed_path[!expected_path, on = .(age, year)]

    if (nrow(missing_path) > 0L) {
      cat("Missing cohort age-year cells:\n")
      print(missing_path)
    }
    if (nrow(extra_path) > 0L) {
      cat("Unexpected extra cohort age-year cells:\n")
      print(extra_path)
    }

    stop(
      "Cohort year trajectory is incomplete for birth year ", birth_year,
      ". Requested ages ", start_age, "-", terminal_age,
      " require years ", min(expected_years), "-", max(expected_years), "."
    )
  }

  n <- nrow(cohort_data)

  cohort_data[, qx_base := mx / (1 + 0.5 * mx)]
  cohort_data[, qx_climate := mx_climate / (1 + 0.5 * mx_climate)]

  cohort_data[qx_base > 1, qx_base := 1]
  cohort_data[qx_climate > 1, qx_climate := 1]

  # Final extended age is a closed one-year interval
  cohort_data[n, qx_base := 1]
  cohort_data[n, qx_climate := 1]

  cohort_data[, lx_base := as.numeric(NA)]
  cohort_data[, lx_climate := as.numeric(NA)]
  cohort_data[, dx_base := as.numeric(NA)]
  cohort_data[, dx_climate := as.numeric(NA)]

  cohort_data[1, lx_base := radix]
  cohort_data[1, lx_climate := radix]

  for (i in seq_len(n)) {
    cohort_data[i, dx_base := lx_base * qx_base]
    cohort_data[i, dx_climate := lx_climate * qx_climate]

    if (i < n) {
      cohort_data[i + 1, lx_base := cohort_data[i, lx_base - dx_base]]
      cohort_data[i + 1, lx_climate := cohort_data[i, lx_climate - dx_climate]]
    }
  }

  cohort_data[, lx_next_base := c(lx_base[-1], 0)]
  cohort_data[, lx_next_climate := c(lx_climate[-1], 0)]

  cohort_data[, Lx_base := (lx_base + lx_next_base) / 2]
  cohort_data[, Lx_climate := (lx_climate + lx_next_climate) / 2]

  cohort_data[n, Lx_base := lx_base / 2]
  cohort_data[n, Lx_climate := lx_climate / 2]

  cohort_data[, Tx_base := rev(cumsum(rev(Lx_base)))]
  cohort_data[, Tx_climate := rev(cumsum(rev(Lx_climate)))]

  cohort_data[, ex_base := Tx_base / lx_base]
  cohort_data[, ex_climate := Tx_climate / lx_climate]

  cohort_data
}

#' Compute lifespan inequality metrics (clean summary; no threshold outputs)
compute_lifespan_inequality <- function(lifetable, start_age) {

  e20_base <- lifetable[age == start_age, ex_base]
  e20_climate <- lifetable[age == start_age, ex_climate]

  gini_base <- compute_gini(lifetable$age, lifetable$dx_base)
  gini_climate <- compute_gini(lifetable$age, lifetable$dx_climate)

  sd_base <- compute_sd_age_at_death(lifetable$age, lifetable$dx_base)
  sd_climate <- compute_sd_age_at_death(lifetable$age, lifetable$dx_climate)

  iqr_base <- compute_iqr_age_at_death(lifetable$age, lifetable$dx_base)
  iqr_climate <- compute_iqr_age_at_death(lifetable$age, lifetable$dx_climate)

  results <- data.table(
    e20_base = e20_base,
    e20_climate = e20_climate,
    delta_e20 = e20_climate - e20_base,
    pct_delta_e20 = (e20_climate - e20_base) / e20_base * 100,

    gini_base = gini_base,
    gini_climate = gini_climate,
    delta_gini = gini_climate - gini_base,
    pct_change_gini = (gini_climate - gini_base) / gini_base * 100,

    sd_base = sd_base,
    sd_climate = sd_climate,
    delta_sd = sd_climate - sd_base,
    pct_change_sd = (sd_climate - sd_base) / sd_base * 100,

    iqr_base = iqr_base$iqr,
    iqr_climate = iqr_climate$iqr,
    delta_iqr = iqr_climate$iqr - iqr_base$iqr,

    q25_base = iqr_base$q25,
    q75_base = iqr_base$q75,
    q25_climate = iqr_climate$q25,
    q75_climate = iqr_climate$q75
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
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      numerator <- numerator + abs(ages[i] - ages[j]) * deaths[i] * deaths[j]
    }
  }

  denominator <- 2 * mean_age * total_deaths^2
  numerator / denominator
}

compute_sd_age_at_death <- function(ages, deaths) {
  total_deaths <- sum(deaths)
  mean_age <- sum(ages * deaths) / total_deaths
  variance <- sum((ages - mean_age)^2 * deaths) / total_deaths
  sqrt(variance)
}

compute_iqr_age_at_death <- function(ages, deaths) {
  cumsum_deaths <- cumsum(deaths)
  total_deaths <- sum(deaths)

  q25_idx <- which(cumsum_deaths >= 0.25 * total_deaths)[1]
  q75_idx <- which(cumsum_deaths >= 0.75 * total_deaths)[1]

  q25 <- ages[q25_idx]
  q75 <- ages[q75_idx]
  iqr <- q75 - q25

  list(q25 = q25, q75 = q75, iqr = iqr)
}

#' Save detailed outputs for a scenario and GCM
save_detailed_outputs <- function(city_code, ssp, adaptation,
                                  gcm,
                                  lifetable, multipliers, li_results,
                                  output_dir = "results/detailed") {

  dir.create(file.path(output_dir, "lifetables"),
             showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(output_dir, "multipliers"),
             showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(output_dir, "excess_deaths"),
             showWarnings = FALSE, recursive = TRUE)

  gcm_tag <- safe_id(gcm)

  if (!is.null(lifetable)) {
    lt_path <- sprintf("%s/lifetables/%s_ssp%d_adapt%02d_%s.csv",
                       output_dir, city_code, ssp, adaptation * 100, gcm_tag)
    fwrite(lifetable, lt_path)
  }

  if (!is.null(multipliers)) {
    mult_path <- sprintf("%s/multipliers/%s_ssp%d_adapt%02d_%s.csv",
                         output_dir, city_code, ssp, adaptation * 100, gcm_tag)
    fwrite(multipliers, mult_path)
  }

  if (!is.null(lifetable)) {
    excess <- lifetable[, .(
      age, year,
      dx_base, dx_climate,
      excess_deaths = dx_climate - dx_base,
      multiplier
    )]

    excess_path <- sprintf("%s/excess_deaths/%s_ssp%d_adapt%02d_%s.csv",
                           output_dir, city_code, ssp, adaptation * 100, gcm_tag)
    fwrite(excess, excess_path)
  }

  invisible(TRUE)
}
