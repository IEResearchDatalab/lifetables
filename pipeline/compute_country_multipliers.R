################################################################################
#
# Compute Country-Level Mortality Multipliers by Age
#
# Mirrors pipeline/compute_multipliers.R but operates on country-level
# aggregated data.
#
# Country ERF methodology (rigorous curve-averaging approach):
#   For each city in a country, the city's B-spline basis is built from
#   that city's own historical temperature distribution (city-specific
#   percentile knots). The city ERF is then evaluated as uncentered log-RR
#   on the COMMON country temperature grid. Population-weighted averages of
#   these log-RR curves are taken across all cities in the country. The
#   country MMT is found on the averaged curve, and centering + flooring at 1
#   is applied AFTER averaging.
#
#   This avoids the basis-mismatch error of coefficient averaging: each
#   city's B-spline coefficients are only ever multiplied by the basis built
#   from that same city's temperature distribution.
#
# Inputs:
#   - data/coefs.csv                  (city-level B-spline coefficients)
#   - data/tmeanproj.gz.parquet       (city-level projected temperatures)
#   - data/tmeanproj_country.parquet  (pop-weighted country daily temperatures)
#   - data/city_results.csv           (population weights per city)
#
# Outputs (all in results_csv/):
#   mortality_multiplier_by_age_country.csv  — target years, all ages, all SSPs
#   erf_curves_country.csv                   — ERF curves per country × age
#   mmt_country.csv                          — MMT per country × age group
#   multiplier_ts_country.csv                — annual time series, age 65-74,
#                                              per GCM (for uncertainty ribbons)
#
################################################################################

library(data.table)
library(arrow)
library(dplyr)
library(dlnm)
library(splines)

source("R/utils.R")
source("R/rr_basis.R")
source("R/load_data.R")
source("R/load_coefficients.R")

# ── Configuration ────────────────────────────────────────────────────────────

varfun    <- "bs"
vardegree <- 2
varper    <- c(10, 75, 90)
gcmexcl   <- c("CMCC_CM2_SR5", "TaiESM1")

agelabs       <- c("20-44", "45-64", "65-74", "75-84", "85+")
age_midpoints <- c(32.5, 55, 70, 80, 92.5)
age_range     <- 20:100

target_years         <- c(2050, 2075, 2099)
target_ssps          <- c("1", "2", "3")
ssp_labels           <- c("1" = "SSP1-2.6", "2" = "SSP2-4.5", "3" = "SSP3-7.0")
rcp_labels           <- c("1" = "RCP 2.6", "2" = "RCP 4.5", "3" = "RCP 7.0")
components           <- c("heat", "cold", "total")
baseline_temp_period <- 1990:2019
proj_years           <- 2015:2099   # for time-series output

focus_agegroup <- "65-74"          # age group for time-series ribbons

if (!dir.exists("results_csv")) dir.create("results_csv")

# ── Load inputs ──────────────────────────────────────────────────────────────

cat_header("Country-Level Mortality Multiplier Computation")

cat_step(1, "Loading city-level coefficients and population weights")
coefs_all <- fread("data/coefs.csv")
coefs_all[, country_code := substr(URAU_CODE, 1, 2)]
country_codes <- sort(unique(coefs_all$country_code))
cat(sprintf("  %d cities in %d countries\n",
            uniqueN(coefs_all$URAU_CODE), length(country_codes)))

# Population weights for city-level ERF averaging
city_results  <- fread("data/city_results.csv",
                       select = c("URAU_CODE", "pop"))
city_pop      <- unique(city_results[, .(URAU_CODE, pop)])
city_pop_vec  <- setNames(city_pop$pop, city_pop$URAU_CODE)
cat(sprintf("  Population weights loaded for %d cities\n", length(city_pop_vec)))

cat_step(2, "Opening temperature parquets")
ds_city      <- open_dataset("data/tmeanproj.gz.parquet")
cat("  City-level parquet opened\n")
ds_country   <- open_dataset("data/tmeanproj_country.parquet")
gcm_cols_all <- names(ds_country)[grepl("^tas_", names(ds_country))]
gcm_cols     <- gcm_cols_all[!gsub("tas_", "", gcm_cols_all) %in% gcmexcl]
cat(sprintf("  Country parquet opened: %d GCMs (excluded: %s)\n",
            length(gcm_cols), paste(gcmexcl, collapse = ", ")))

# ── Main loop ────────────────────────────────────────────────────────────────

results_target  <- list()   # target years × all ages × all SSPs × components
results_full    <- list()   # all years × all ages × all SSPs × components
results_ts      <- list()   # all years × age group 65-74 × per GCM
erf_curves_list <- list()   # ERF curves per country
mmt_list        <- list()   # MMT per country × age group

for (ci in seq_along(country_codes)) {
  cntry <- country_codes[ci]
  cat(sprintf("\n[%d/%d] %s\n", ci, length(country_codes), cntry))

  # ── Load country temperature series (pop-weighted) ──────────────────────
  proj_data <- ds_country %>%
    filter(URAU_CODE == cntry) %>%
    select(c("URAU_CODE", "date", "ssp", all_of(gcm_cols))) %>%
    collect() %>%
    as.data.table()

  proj_data[, year := year(date)]
  proj_data[, doy  := as.integer(format(date, "%j"))]
  proj_data[doy > 365L, doy := 365L]

  # ── Country temperature range for the common ERF grid ────────────────────
  hist_temps   <- extract_hist_temps(proj_data, gcm_cols)
  basis_params <- build_basis_params(hist_temps, varfun, vardegree, varper)

  # ── Load city historical temperatures for ERF averaging ──────────────────
  city_codes_cntry <- unique(coefs_all[country_code == cntry, URAU_CODE])
  cat(sprintf("  Loading historical temps for %d cities...\n",
              length(city_codes_cntry)))

  city_hist_raw <- ds_city %>%
    filter(URAU_CODE %in% city_codes_cntry, ssp == "hist") %>%
    select(c("URAU_CODE", all_of(gcm_cols))) %>%
    collect() %>%
    as.data.table()

  # Build named list: city -> numeric vector of historical temperatures
  city_hist_temps <- lapply(city_codes_cntry, function(city) {
    rows <- city_hist_raw[URAU_CODE == city, ..gcm_cols]
    temps <- unlist(rows, use.names = FALSE)
    temps[!is.na(temps)]
  })
  names(city_hist_temps) <- city_codes_cntry

  # ── Country ERF: population-weighted average of city ERF curves ───────────
  # Each city's coefficients are evaluated on the COMMON country temperature
  # grid using the CITY's own basis (built from city-specific percentile knots).
  # Uncentered log-RR curves are averaged, then centred at the country MMT.
  coefs_cntry <- coefs_all[country_code == cntry]
  rr_result   <- compute_country_rr_curves(
    coefs_cities      = coefs_cntry,
    city_hist_temps   = city_hist_temps,
    city_pop_weights  = city_pop_vec,
    agelabs           = agelabs,
    age_midpoints     = age_midpoints,
    country_varbound  = basis_params$varbound,
    varfun            = varfun,
    vardegree         = vardegree,
    varper            = varper
  )

  # ── Store ERF curves ──────────────────────────────────────────────────────
  erf_dt <- as.data.table(rr_result$rr_matrix)
  setnames(erf_dt, agelabs)
  erf_dt[, country_code := cntry]
  erf_dt[, temp         := rr_result$temp_seq]
  erf_curves_list[[ci]] <- erf_dt

  # ── Store MMT ─────────────────────────────────────────────────────────────
  mmt_list[[ci]] <- data.table(
    country_code = cntry,
    agegroup     = agelabs,
    mmt          = rr_result$mmt_vec
  )

  # ── Interpolate RR to single-year ages ────────────────────────────────────
  rr_interp <- interpolate_rr_to_single_age(
    rr_result$rr_matrix, rr_result$mmt_vec,
    age_midpoints, age_range = age_range
  )

  # ── Baseline average RR ───────────────────────────────────────────────────
  baseline <- pool_baseline_temperatures(
    proj_data, gcm_cols, target_ssps, baseline_temp_period
  )

  baseline_rr <- setNames(
    lapply(components, function(comp) {
      compute_avg_rr_by_age(
        baseline$temps, rr_result$temp_seq,
        rr_interp$rr_single_age, rr_interp$mmt_single_age,
        age_range, component = comp,
        doys = baseline$doys, sw_matrix = NULL
      )
    }),
    components
  )

  # ── Target-year multipliers (all ages, all SSPs, all components) ──────────
  cat("  Target years: ")
  for (scen in target_ssps) {
    for (yr in target_years) {
      yr_rows   <- proj_data[ssp == scen & year == yr]
      all_temps <- unlist(yr_rows[, ..gcm_cols], use.names = FALSE)
      all_doys  <- rep(yr_rows$doy, length(gcm_cols))
      valid     <- !is.na(all_temps)

      for (comp in components) {
        avg_rr <- compute_avg_rr_by_age(
          all_temps[valid], rr_result$temp_seq,
          rr_interp$rr_single_age, rr_interp$mmt_single_age,
          age_range, component = comp,
          doys = all_doys[valid], sw_matrix = NULL
        )
        results_target[[length(results_target) + 1]] <- data.table(
          country_code = cntry,
          component    = comp,
          ssp          = scen,
          year         = yr,
          age          = age_range,
          avg_rr       = avg_rr,
          multiplier   = avg_rr / baseline_rr[[comp]]
        )
      }
      cat(sprintf("%s/%s ", scen, yr))
    }
  }
  cat("\n")

  # ── Full-period multipliers (all years, all ages, all SSPs, components) ──
  cat("  Full period: ")
  for (scen in target_ssps) {
    ssp_data <- proj_data[ssp == scen]

    for (yr in proj_years) {
      yr_rows <- ssp_data[year == yr]
      if (nrow(yr_rows) == 0L) next

      all_temps <- unlist(yr_rows[, ..gcm_cols], use.names = FALSE)
      all_doys  <- rep(yr_rows$doy, length(gcm_cols))
      valid     <- !is.na(all_temps)

      for (comp in components) {
        avg_rr <- compute_avg_rr_by_age(
          all_temps[valid], rr_result$temp_seq,
          rr_interp$rr_single_age, rr_interp$mmt_single_age,
          age_range, component = comp,
          doys = all_doys[valid], sw_matrix = NULL
        )
        results_full[[length(results_full) + 1]] <- data.table(
          country_code = cntry,
          component    = comp,
          ssp          = scen,
          year         = yr,
          age          = age_range,
          avg_rr       = avg_rr,
          multiplier   = avg_rr / baseline_rr[[comp]]
        )
      }
    }
    cat(sprintf("SSP%s ", scen))
  }
  cat("\n")

  # ── Annual time-series: age group 65-74, per GCM, total component ─────────
  cat("  Time series: ")
  ag_idx <- which(agelabs == focus_agegroup)

  # Baseline for age group 65-74 (total, pooled across all GCMs)
  ts_baseline_rr <- mean(rr_result$rr_matrix[
    vapply(baseline$temps, function(t) which.min(abs(rr_result$temp_seq - t)),
           integer(1)), ag_idx])

  for (scen in target_ssps) {
    ssp_data <- proj_data[ssp == scen]

    for (yr in proj_years) {
      yr_rows <- ssp_data[year == yr]
      if (nrow(yr_rows) == 0L) next

      for (gcm_col in gcm_cols) {
        temps <- yr_rows[[gcm_col]]
        temps <- temps[!is.na(temps)]
        if (length(temps) == 0L) next

        t_idx   <- vapply(temps,
                          function(t) which.min(abs(rr_result$temp_seq - t)),
                          integer(1))
        avg_rr  <- mean(rr_result$rr_matrix[t_idx, ag_idx])
        results_ts[[length(results_ts) + 1]] <- data.table(
          country_code = cntry,
          agegroup     = focus_agegroup,
          component    = "total",
          ssp          = scen,
          year         = yr,
          gcm          = gsub("tas_", "", gcm_col),
          avg_rr       = avg_rr,
          multiplier   = avg_rr / ts_baseline_rr
        )
      }
    }
    cat(sprintf("SSP%s ", scen))
  }
  cat("\n")
}

# ── Combine & label ──────────────────────────────────────────────────────────

cat_step(3, "Combining results")

results     <- rbindlist(results_target)
results_all <- rbindlist(results_full)
results_ts  <- rbindlist(results_ts)
erf_curves  <- rbindlist(erf_curves_list)
mmt_all     <- rbindlist(mmt_list)

results[,    ssp_label := ssp_labels[ssp]]
results_all[, ssp_label := ssp_labels[ssp]]
results_ts[, ssp_label := ssp_labels[ssp]]

# Requested export format: one CSV with country, RCP scenario, year, single-year
# age, and mortality multipliers in separate total/heat/cold columns.
results_all_wide <- dcast(
  results_all,
  country_code + ssp + year + age ~ component,
  value.var = "multiplier"
)
setnames(results_all_wide,
         old = c("country_code", "ssp", "total", "heat", "cold"),
         new = c("country", "rcp_scenario", "multiplier_total",
                 "multiplier_heat", "multiplier_cold"))
results_all_wide[, rcp_scenario := rcp_labels[rcp_scenario]]
setcolorder(results_all_wide,
            c("country", "rcp_scenario", "year", "age",
              "multiplier_total", "multiplier_heat", "multiplier_cold"))
setorder(results_all_wide, country, rcp_scenario, year, age)

# ── Save ─────────────────────────────────────────────────────────────────────

cat_step(4, "Saving outputs")

fwrite(results,    "results_csv/mortality_multiplier_by_age_country.csv")
fwrite(results_all, "results_csv/mortality_multiplier_by_age_country_all_years_long.csv")
fwrite(results_all_wide, "results_csv/mortality_multiplier_country_rcp_year_age.csv")
fwrite(results_ts, "results_csv/multiplier_ts_country.csv")
fwrite(erf_curves, "results_csv/erf_curves_country.csv")
fwrite(mmt_all,    "results_csv/mmt_country.csv")

cat(sprintf("  mortality_multiplier_by_age_country.csv : %s rows\n",
            format(nrow(results),    big.mark = ",")))
cat(sprintf("  mortality_multiplier_by_age_country_all_years_long.csv : %s rows\n",
            format(nrow(results_all), big.mark = ",")))
cat(sprintf("  mortality_multiplier_country_rcp_year_age.csv : %s rows\n",
            format(nrow(results_all_wide), big.mark = ",")))
cat(sprintf("  multiplier_ts_country.csv               : %s rows\n",
            format(nrow(results_ts), big.mark = ",")))
cat(sprintf("  erf_curves_country.csv                  : %s rows\n",
            format(nrow(erf_curves), big.mark = ",")))
cat(sprintf("  mmt_country.csv                         : %d rows\n",
            nrow(mmt_all)))

# ── Quick summary ─────────────────────────────────────────────────────────────

cat("\nTop 10 countries by total multiplier (SSP3-7.0, age 65, year 2099):\n")
summary_tbl <- results[
  component == "total" & ssp == "3" & year == 2099 & age == 65,
  .(country_code, multiplier = round(multiplier, 4))
][order(-multiplier)]
print(summary_tbl)

cat("\nDone.\n")
