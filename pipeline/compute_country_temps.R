################################################################################
#
# Compute Country-Level Temperature Data
#
# Aggregates city-level projected temperature data from data/tmeanproj.gz.parquet
# to the country level using a POPULATION-WEIGHTED mean of daily temperatures
# across all cities in each country, for each date, GCM, and SSP scenario.
#
# Methodology:
#   Each city's temperature is weighted by its total population (column "pop" from
#   data/city_results.csv). This makes the country temperature series represent the
#   temperature experienced by the average person in the country, consistent with
#   using a life-table multiplier to scale per-capita mortality risk.
#
#   Note: the country-level ERF coefficients (compute_country_coefs.R) use an
#   unweighted mean following Masselot's Extended Data Fig. 6, but temperature
#   weighting is independent and uses population weights.
#
# Input:  data/tmeanproj.gz.parquet   (city-level, URAU_CODE, date, GCMs, ssp)
#         data/coefs.csv              (used to get the list of 854 city codes)
#         data/city_results.csv       (provides "pop" column for population weights)
# Output: data/tmeanproj_country.parquet  (country-level, same schema)
#
################################################################################

library(data.table)
library(arrow)
library(dplyr)

# ---- Paths ----
parquet_in    <- "data/tmeanproj.gz.parquet"
coefs_path    <- "data/coefs.csv"
results_path  <- "data/city_results.csv"
parquet_out   <- "data/tmeanproj_country.parquet"

# GCMs to exclude (same as pipeline/config.R)
gcmexcl <- c("CMCC_CM2_SR5", "TaiESM1")

# ---- Build city-to-country mapping with population weights ----
cat("Building city-to-country mapping with population weights...\n")
coefs <- fread(coefs_path, select = "URAU_CODE")
city_country <- unique(coefs[, .(URAU_CODE, country_code = substr(URAU_CODE, 1, 2))])

# Load total city population (one row per city — take from any age group)
city_results <- fread(results_path, select = c("URAU_CODE", "pop"))
city_pop <- unique(city_results[, .(URAU_CODE, pop)])

# Merge population onto city-country mapping
city_country <- merge(city_country, city_pop, by = "URAU_CODE", all.x = TRUE)
n_missing_pop <- sum(is.na(city_country$pop))
if (n_missing_pop > 0) {
  cat(sprintf("  WARNING: %d cities have no population data — using equal weight\n",
              n_missing_pop))
  city_country[is.na(pop), pop := 1]
}

countries <- sort(unique(city_country$country_code))
cat(sprintf("  %d cities across %d countries\n",
            nrow(city_country), length(countries)))

# ---- Open source dataset ----
ds <- open_dataset(parquet_in)

# Identify GCM columns (all tas_* minus excluded)
gcm_cols <- names(ds)[grepl("^tas_", names(ds))]
gcm_cols_use <- gcm_cols[!gsub("tas_", "", gcm_cols) %in% gcmexcl]
cat(sprintf("  Using %d GCMs (excluded: %s)\n",
            length(gcm_cols_use), paste(gcmexcl, collapse = ", ")))

# ---- Process country by country ----
cat("\nAggregating temperature data by country...\n")

result_list <- vector("list", length(countries))

for (k in seq_along(countries)) {
  cntry      <- countries[k]
  city_codes <- city_country[country_code == cntry, URAU_CODE]
  pop_vals   <- city_country[country_code == cntry, pop]

  cat(sprintf("  [%d/%d] %s: %d cities... ",
              k, length(countries), cntry, length(city_codes)))

  # Load data for all cities in this country
  city_data <- ds %>%
    filter(URAU_CODE %in% city_codes) %>%
    select(c("URAU_CODE", "date", "ssp", all_of(gcm_cols_use))) %>%
    collect() %>%
    as.data.table()

  if (nrow(city_data) == 0) {
    cat("NO DATA — skipping\n")
    next
  }

  # Merge population weights onto city data
  pop_dt <- data.table(URAU_CODE = city_codes, pop = pop_vals)
  city_data <- merge(city_data, pop_dt, by = "URAU_CODE", all.x = TRUE)

  # Population-weighted mean across cities for each date × ssp
  country_mean <- city_data[, {
    w     <- pop / sum(pop, na.rm = TRUE)
    res   <- lapply(.SD, function(col) {
      valid <- !is.na(col)
      if (!any(valid)) return(NA_real_)
      sum(w[valid] * col[valid]) / sum(w[valid])
    })
    as.list(res)
  }, by = .(date, ssp), .SDcols = gcm_cols_use]

  country_mean[, URAU_CODE := cntry]
  setcolorder(country_mean, c("URAU_CODE", "date", gcm_cols_use, "ssp"))

  result_list[[k]] <- country_mean
  cat(sprintf("%d rows\n", nrow(country_mean)))
}

# ---- Combine and write ----
cat("\nCombining results...\n")
result_list <- Filter(Negate(is.null), result_list)
country_temps <- rbindlist(result_list, use.names = TRUE, fill = TRUE)

cat(sprintf("  Total rows: %d (%d countries)\n",
            nrow(country_temps), uniqueN(country_temps$URAU_CODE)))

cat(sprintf("Writing: %s\n", parquet_out))
write_parquet(country_temps, parquet_out)
cat("Done.\n")
