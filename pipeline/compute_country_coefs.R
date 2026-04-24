################################################################################
#
# [DEPRECATED] Compute Country-Level RR Coefficients
#
# This script is no longer used in the pipeline. The coefficient-averaging
# approach it implements is methodologically unsound: each city's B-spline
# coefficients are defined relative to that city's own temperature-percentile
# basis, so averaging them across cities and then evaluating on a third basis
# (the country distribution) produces numerically incoherent results.
#
# The rigorous replacement is in compute_country_multipliers.R, which uses
# compute_country_rr_curves() from R/rr_basis.R: for each city the ERF is
# evaluated as uncentered log-RR on the COMMON country temperature grid using
# the CITY's own basis, and the resulting curves are population-weighted averaged
# before the country MMT is found and centering applied.
#
# This file is retained for reference only.
#
################################################################################
#
# Compute Country-Level RR Coefficients
#
# Pools city-level B-spline coefficients from data/coefs.csv to the country
# level using a SIMPLE UNWEIGHTED mean across all cities in each country,
# for each age group separately.
#
# Methodology (Masselot et al. 2025, Extended Data Fig. 6):
#   Masselot's EU-level ERF is derived from the meta-regression fixed effects
#   only (intercept + age terms, region zeroed out), evaluated at the average
#   age of each age group. This gives the "average European city" ERF, purged
#   of city-specific spatial random effects.
#
#   We replicate this for each country by taking the SIMPLE (unweighted) mean
#   of city coefficient vectors within the country. The unweighted mean averages
#   out kriged random effects in the same spirit as the fixed-effects-only
#   prediction, and mirrors Masselot's unweighted temperature averaging for the
#   EU ERF basis.
#
#   NOTE: no population column is needed for this computation.
#
# Input:  data/coefs.csv          (URAU_CODE, agegroup, b1-b5)
#         data/city_results.csv   (URAU_CODE, agegroup, agepop)
# Output: data/coefs_country.csv  (country_code, agegroup, b1-b5)
#
################################################################################

library(data.table)

# ---- Paths ----
coefs_path   <- "data/coefs.csv"
output_path  <- "data/coefs_country.csv"

# ---- Load coefficients ----
cat("Loading city-level coefficients...\n")
coefs <- fread(coefs_path)

coef_cols <- names(coefs)[grepl("^b[0-9]+$", names(coefs))]
cat(sprintf("  %d cities, %d age groups, %d coefficient columns\n",
            uniqueN(coefs$URAU_CODE), uniqueN(coefs$agegroup), length(coef_cols)))

# ---- Extract country code from URAU_CODE ----
# URAU_CODE format: XX###C (first 2 chars = ISO country code)
coefs[, country_code := substr(URAU_CODE, 1, 2)]

countries <- sort(unique(coefs$country_code))
cat(sprintf("  Countries: %d\n", length(countries)))
cat(sprintf("  %s\n", paste(countries, collapse = ", ")))

# ---- Pool coefficients by country and age group (unweighted mean) ----
cat("\nPooling coefficients by country (unweighted mean)...\n")

coefs_country <- coefs[, {
  res <- lapply(.SD, function(x) mean(x, na.rm = TRUE))
  as.list(res)
}, by = .(country_code, agegroup), .SDcols = coef_cols]

# Rename country_code to URAU_CODE for compatibility with downstream code
setnames(coefs_country, "country_code", "URAU_CODE")

# Sort for readability
setorder(coefs_country, URAU_CODE, agegroup)

# ---- Report summary ----
cat(sprintf("  Output: %d rows (%d countries × %d age groups)\n",
            nrow(coefs_country),
            uniqueN(coefs_country$URAU_CODE),
            uniqueN(coefs_country$agegroup)))

# ---- Save ----
fwrite(coefs_country, output_path)
cat(sprintf("\nSaved: %s\n", output_path))

# ---- Validation: show sample ----
cat("\nSample (first 3 countries, first age group):\n")
sample_rows <- coefs_country[agegroup == "20-44"][1:3]
print(sample_rows)
