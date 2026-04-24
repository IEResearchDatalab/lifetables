################################################################################
#
# Extract Eurostat EUROPOP2019 Regional Projections (NEW FORMAT, NO RAW XLSX)
#
# Downloads Eurostat "raw" format in-memory (no intermediate xlsx),
# computes sex-combined qx and converts to mx, then saves:
#   - data/mortality_projections_<city>.csv
#   - data/mortality_projections_by_sex_<city>.csv
#
################################################################################

library(data.table)
library(eurostat)   # needed for get_eurostat()

#------------------------------------------------------------------------------
# Parse age codes in the NEW format
#   Examples: Y_LT1, Y1 ... Y99, Y_GE100
#------------------------------------------------------------------------------

parse_age_code <- function(age_code) {
  age <- rep(NA_integer_, length(age_code))

  age[grepl("^Y_LT1$", age_code, ignore.case = TRUE)] <- 0L
  age[grepl("^Y_GE100$", age_code, ignore.case = TRUE)] <- 100L

  m <- regmatches(age_code, regexec("^Y([0-9]+)$", age_code, ignore.case = TRUE))
  idx <- lengths(m) == 2
  age[idx] <- as.integer(vapply(m[idx], function(x) x[2], character(1)))

  age
}

#------------------------------------------------------------------------------
# Convert get_eurostat(..., time_format="raw") output to standard long DT
#------------------------------------------------------------------------------

raw_to_dt <- function(raw_df, sex_label, value_colname = "value") {
  dt <- as.data.table(raw_df)

  # Eurostat raw usually provides "time" + "values" and dimensions like age/geo/sex...
  expected <- c("age", "geo", "time")
  missing_cols <- setdiff(expected, names(dt))
  if (length(missing_cols) > 0) {
    stop(sprintf("Raw Eurostat data missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Determine numeric value column: prefer "values" if present, else "value"
  if ("values" %in% names(dt)) {
    dt[, (value_colname) := as.numeric(values)]
  } else if ("value" %in% names(dt)) {
    dt[, (value_colname) := as.numeric(value)]
  } else {
    stop("Raw Eurostat data has neither 'values' nor 'value' column.")
  }

  dt[, year := as.integer(as.character(time))]
  dt[, age_int := parse_age_code(as.character(age))]
  dt[, sex := sex_label]

  # keep minimal set used downstream
  out <- dt[!is.na(year) & !is.na(age_int) & !is.na(get(value_colname)),
            .(age = age_int, year, sex, geo, value = get(value_colname))]

  out[]
}

#------------------------------------------------------------------------------
# Download raw data from Eurostat (IN MEMORY ONLY)
#------------------------------------------------------------------------------

cat(sprintf("Downloading Eurostat EUROPOP2019 data for %s...\n\n", city_name))

# Mortality qx
raw_qx_m <- get_eurostat("proj_19raasmr3",
                         filters = list(geo = nuts3_code, sex = "M"),
                         time_format = "raw")
raw_qx_f <- get_eurostat("proj_19raasmr3",
                         filters = list(geo = nuts3_code, sex = "F"),
                         time_format = "raw")

# Population
raw_pop_m <- get_eurostat("proj_19rp3",
                          filters = list(geo = nuts3_code, sex = "M"),
                          time_format = "raw")
raw_pop_f <- get_eurostat("proj_19rp3",
                          filters = list(geo = nuts3_code, sex = "F"),
                          time_format = "raw")

#------------------------------------------------------------------------------
# Standardize to your long format (same shape as your xlsx reader output)
#------------------------------------------------------------------------------

cat("Standardizing mortality data...\n")
qx_male   <- raw_to_dt(raw_qx_m, "M"); setnames(qx_male, "value", "qx")
qx_female <- raw_to_dt(raw_qx_f, "F"); setnames(qx_female, "value", "qx")
qx_all <- rbindlist(list(qx_male, qx_female), use.names = TRUE, fill = TRUE)

cat("Standardizing population data...\n")
pop_male   <- raw_to_dt(raw_pop_m, "M"); setnames(pop_male, "value", "pop")
pop_female <- raw_to_dt(raw_pop_f, "F"); setnames(pop_female, "value", "pop")
pop_all <- rbindlist(list(pop_male, pop_female), use.names = TRUE, fill = TRUE)

# (Optional safety) Ensure we’re only using target NUTS3
qx_all  <- qx_all[geo == nuts3_code]
pop_all <- pop_all[geo == nuts3_code]

cat(sprintf("  %s (NUTS3=%s) qx records: %d\n", city_name, nuts3_code, nrow(qx_all)))
cat(sprintf("  %s (NUTS3=%s) pop records: %d\n\n", city_name, nuts3_code, nrow(pop_all)))

#------------------------------------------------------------------------------
# Merge mortality and population
#------------------------------------------------------------------------------

cat("Merging mortality and population data...\n")

merged <- merge(
  qx_all[, .(age, year, sex, qx)],
  pop_all[, .(age, year, sex, pop)],
  by = c("age", "year", "sex"),
  all.x = TRUE
)

cat(sprintf("  Merged records: %d\n", nrow(merged)))

missing_pop <- sum(is.na(merged$pop))
if (missing_pop > 0) {
  cat(sprintf("  Warning: %d records with missing population - using equal weights\n", missing_pop))
  merged[is.na(pop), pop := 1]
}

#------------------------------------------------------------------------------
# Compute sex-combined mortality rates
#------------------------------------------------------------------------------

cat("\nComputing sex-combined mortality rates...\n")

combined <- merged[, .(
  qx = sum(qx * pop) / sum(pop),
  pop_total = sum(pop),
  pop_male = sum(pop[sex == "M"]),
  pop_female = sum(pop[sex == "F"]),
  qx_male = qx[sex == "M"][1],
  qx_female = qx[sex == "F"][1]
), by = .(age, year)]

setorder(combined, year, age)

cat(sprintf("  Combined records: %d\n", nrow(combined)))
cat(sprintf("  Age range: %d to %d\n", min(combined$age, na.rm = TRUE), max(combined$age, na.rm = TRUE)))
cat(sprintf("  Year range: %d to %d\n", min(combined$year), max(combined$year)))

#------------------------------------------------------------------------------
# Convert to central death rate (mx) and add actuarial columns
#------------------------------------------------------------------------------

cat("\nAdding actuarial columns...\n")

combined[, ax := ifelse(age == 0, 0.1, 0.5)]
combined[, mx := qx / (1 - (1 - ax) * qx)]

combined[mx > 1, mx := 1]
combined[qx > 1, qx := 1]

#------------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------

cat("\nSaving results...\n")

output_file <- sprintf("results_csv/mortality_projections_%s.csv", tolower(city_name))
fwrite(combined, output_file)
cat(sprintf("  Saved: %s (%d rows)\n", output_file, nrow(combined)))

by_sex_file <- sprintf("results_csv/mortality_projections_by_sex_%s.csv", tolower(city_name))
fwrite(merged, by_sex_file)
cat(sprintf("  Saved: %s\n", by_sex_file))

cat("\nDone!\n")