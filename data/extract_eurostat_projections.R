################################################################################
#
# Extract Eurostat EUROPOP2019 Regional Projections
#
# This script extracts mortality rates (qx) and populations from Eurostat
# EUROPOP2019 regional projections and computes sex-combined mortality rates
# for use in cohort life table calculations.
#
# The city is configured via config.R (nuts3_code and city_name).
#
# Input files:
#   - proj_19raasmr3__custom_*.xlsx: Probability of dying by age, sex, NUTS 3
#   - proj_19rp3__custom_*.xlsx: Population by age, sex, NUTS 3
#
# Output:
#   - <city>_mortality_projections.csv: Combined qx by age and year
#
################################################################################

library(readxl)
library(data.table)
library(tidyr)

source("../config.R")

#------------------------------------------------------------------------------
# Helper function to read and reshape Eurostat xlsx
#------------------------------------------------------------------------------

read_eurostat_xlsx <- function(filepath, sheet, skip_rows = 9) {
  # Read the data, skipping header rows
  raw <- read_excel(filepath, sheet = sheet, skip = skip_rows)
  
  # First two columns are GEO and AGE
  names(raw)[1:2] <- c("geo", "age_label")
  
  # Remove the header row if present
  raw <- raw[raw$geo != "GEO (Labels)", ]
  
  # Get year columns (odd columns starting from 3)
  year_cols <- names(raw)[seq(3, ncol(raw), by = 2)]
  
  # Keep only geo, age_label, and year columns
  keep_cols <- c("geo", "age_label", year_cols)
  raw <- raw[, names(raw) %in% keep_cols | grepl("^[0-9]{4}$", names(raw))]
  
  # Convert to data.table
  dt <- as.data.table(raw)
  
  # Melt to long format
  dt_long <- melt(dt, 
                  id.vars = c("geo", "age_label"), 
                  variable.name = "year",
                  value.name = "value")
  
  # Convert year and value
  dt_long[, year := as.integer(as.character(year))]
  dt_long[, value := as.numeric(value)]
  
  # Parse age from label
  dt_long[, age := parse_age(age_label)]
  
  return(dt_long[!is.na(value)])
}

#------------------------------------------------------------------------------
# Parse age from Eurostat age labels
#------------------------------------------------------------------------------

parse_age <- function(age_label) {
  age <- rep(NA_integer_, length(age_label))
  
  # "Less than 1 year" -> 0
  age[grepl("Less than 1|Under 1", age_label, ignore.case = TRUE)] <- 0L
  
  # "Total" -> NA (skip)
  age[grepl("^Total$", age_label, ignore.case = TRUE)] <- NA_integer_
  
  # "X years" or "X year" -> X
  nums <- gsub("\\s*(years?|year).*", "", age_label)
  nums <- gsub("[^0-9]", "", nums)
  idx <- !is.na(as.integer(nums)) & is.na(age)
  age[idx] <- as.integer(nums[idx])
  
  # "100 years or over" or "100+" -> 100
  age[grepl("100.*over|100\\+", age_label, ignore.case = TRUE)] <- 100L
  
  return(age)
}

#------------------------------------------------------------------------------
# Main extraction
#------------------------------------------------------------------------------

cat(sprintf("Extracting Eurostat EUROPOP2019 projections for %s...\n\n", city_name))

# Find the files
mort_file <- list.files("data", pattern = "proj_19raasmr3.*\\.xlsx$", full.names = TRUE)[1]
pop_file <- list.files("data", pattern = "proj_19rp3.*\\.xlsx$", full.names = TRUE)[1]

cat("Mortality file:", mort_file, "\n")
cat("Population file:", pop_file, "\n\n")

#------------------------------------------------------------------------------
# Read mortality data (qx)
#------------------------------------------------------------------------------

cat("Reading mortality data...\n")

# Males (Sheet 1)
qx_male <- read_eurostat_xlsx(mort_file, sheet = "Sheet 1")
qx_male[, sex := "M"]
cat(sprintf("  Males: %d records\n", nrow(qx_male)))

# Females (Sheet 2)
qx_female <- read_eurostat_xlsx(mort_file, sheet = "Sheet 2")
qx_female[, sex := "F"]
cat(sprintf("  Females: %d records\n", nrow(qx_female)))

# Combine
qx_all <- rbind(qx_male, qx_female)
setnames(qx_all, "value", "qx")

# Filter for target city/region
# Uses nuts3_code from config.R to match the geo column
qx_all <- qx_all[grepl(city_name, geo, ignore.case = TRUE)]
cat(sprintf("  %s records: %d\n", city_name, nrow(qx_all)))

#------------------------------------------------------------------------------
# Read population data
#------------------------------------------------------------------------------

cat("\nReading population data...\n")

# Males (Sheet 1)
pop_male <- read_eurostat_xlsx(pop_file, sheet = "Sheet 1")
pop_male[, sex := "M"]
cat(sprintf("  Males: %d records\n", nrow(pop_male)))

# Females (Sheet 2)
pop_female <- read_eurostat_xlsx(pop_file, sheet = "Sheet 2")
pop_female[, sex := "F"]
cat(sprintf("  Females: %d records\n", nrow(pop_female)))

# Combine
pop_all <- rbind(pop_male, pop_female)
setnames(pop_all, "value", "pop")

# Filter for target city/region
pop_all <- pop_all[grepl(city_name, geo, ignore.case = TRUE)]
cat(sprintf("  %s records: %d\n", city_name, nrow(pop_all)))

#------------------------------------------------------------------------------
# Merge mortality and population
#------------------------------------------------------------------------------

cat("\nMerging mortality and population data...\n")

# Merge on age, year, sex
merged <- merge(qx_all[, .(age, year, sex, qx)],
                pop_all[, .(age, year, sex, pop)],
                by = c("age", "year", "sex"),
                all.x = TRUE)

cat(sprintf("  Merged records: %d\n", nrow(merged)))

# Check for missing populations (use equal weighting if missing)
missing_pop <- sum(is.na(merged$pop))
if (missing_pop > 0) {
  cat(sprintf("  Warning: %d records with missing population - using equal weights\n", missing_pop))
  merged[is.na(pop), pop := 1]
}

#------------------------------------------------------------------------------
# Compute sex-combined mortality rates
#------------------------------------------------------------------------------

cat("\nComputing sex-combined mortality rates...\n")

# qx_combined = (qx_M * pop_M + qx_F * pop_F) / (pop_M + pop_F)
combined <- merged[, .(
  qx = sum(qx * pop) / sum(pop),
  pop_total = sum(pop),
  pop_male = sum(pop[sex == "M"]),
  pop_female = sum(pop[sex == "F"]),
  qx_male = qx[sex == "M"][1],
  qx_female = qx[sex == "F"][1]
), by = .(age, year)]

# Sort by age and year
setorder(combined, year, age)

cat(sprintf("  Combined records: %d\n", nrow(combined)))
cat(sprintf("  Age range: %d to %d\n", min(combined$age, na.rm = TRUE), max(combined$age, na.rm = TRUE)))
cat(sprintf("  Year range: %d to %d\n", min(combined$year), max(combined$year)))

#------------------------------------------------------------------------------
# Convert to central death rate (mx) and add actuarial columns
#------------------------------------------------------------------------------

cat("\nAdding actuarial columns...\n")

# ax: fraction of year lived by those who die
combined[, ax := ifelse(age == 0, 0.1, 0.5)]

# Convert qx to mx: mx = qx / (1 - (1-ax)*qx)
# This is the inverse of: qx = mx / (1 + (1-ax)*mx)
combined[, mx := qx / (1 - (1 - ax) * qx)]

# Cap extreme values
combined[mx > 1, mx := 1]
combined[qx > 1, qx := 1]

#------------------------------------------------------------------------------
# Summary statistics
#------------------------------------------------------------------------------

cat("\n--- Summary Statistics ---\n")

# Sample years
for (yr in c(2023, 2050, 2099)) {
  yr_data <- combined[year == yr & age %in% c(20, 40, 60, 80)]
  if (nrow(yr_data) > 0) {
    cat(sprintf("\nYear %d (selected ages):\n", yr))
    print(yr_data[, .(age, qx = round(qx, 6), mx = round(mx, 6), pop_total)])
  }
}

# Life expectancy check at age 0 for 2023
if (2023 %in% combined$year) {
  lt_2023 <- combined[year == 2023][order(age)]
  # Simple life expectancy calculation
  lx <- 100000
  Lx_sum <- 0
  for (i in 1:nrow(lt_2023)) {
    qx_i <- lt_2023$qx[i]
    ax_i <- lt_2023$ax[i]
    dx <- lx * qx_i
    Lx <- lx - (1 - ax_i) * dx
    Lx_sum <- Lx_sum + Lx
    lx <- lx - dx
  }
  e0 <- Lx_sum / 100000
  cat(sprintf("\nEstimated life expectancy at birth (2023): %.2f years\n", e0))
}

#------------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------

cat("\nSaving results...\n")

# Full dataset
output_file <- sprintf("data/%s_mortality_projections.csv", tolower(city_name))
fwrite(combined, output_file)
cat(sprintf("  Saved: %s (%d rows)\n", output_file, nrow(combined)))

# Also save sex-specific for reference
by_sex_file <- sprintf("data/%s_mortality_projections_by_sex.csv", tolower(city_name))
fwrite(merged, by_sex_file)
cat(sprintf("  Saved: %s\n", by_sex_file))

cat("\nDone!\n")
