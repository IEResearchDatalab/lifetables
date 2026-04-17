################################################################################
#
# City Configuration
#
# Central configuration file for the climate-mortality pipeline.
# All pipeline scripts source this file to get city-specific parameters.
#
# To analyse a different city, either:
#   a) Set city_name before sourcing this file (used by run_cities.R loop)
#   b) Uncomment the city_name line below for standalone runs
#
################################################################################

# ---- City identifiers ----
# city_name <- "Bucharest"  # Uncomment for standalone runs
city_name_lower <- tolower(city_name)

# City-specific codes (must match config_city_codes.json)
city_configs <- list(
	Bucharest = list(city_code = "RO001C", nuts3_code = "RO321"),
	Helsinki  = list(city_code = "FI001C", nuts3_code = "FI1B1"),
	Rome      = list(city_code = "IT001C", nuts3_code = "ITI43"),
	Vienna    = list(city_code = "AT001C", nuts3_code = "AT130"),
	Berlin    = list(city_code = "DE001C", nuts3_code = "DE300"),
	Tallinn   = list(city_code = "EE001C", nuts3_code = "EE001"),
	Warsaw    = list(city_code = "PL001C", nuts3_code = "PL911")
)

if (!city_name %in% names(city_configs)) {
	stop(sprintf("City '%s' not recognized. Available: %s",
	             city_name, paste(names(city_configs), collapse = ", ")))
}

city_code  <- city_configs[[city_name]]$city_code
nuts3_code <- city_configs[[city_name]]$nuts3_code

# ---- Output directories ----
img_dir <- "img"

# ---- Cohort parameters ----
cohort_start_age  <- 20      # Starting age of the cohort
cohort_start_year <- 2019    # Calendar year at cohort start
cohort_end_year   <- 2099    # Calendar year at cohort end
cohort_years      <- cohort_start_year:cohort_end_year

# ---- Interest rate for EPV calculations ----
interest_rate   <- 0.02      # 2% annual discount rate
discount_factor <- 1 / (1 + interest_rate)

# ---- Age groups (for RR coefficients) ----
agebreaks     <- c(20, 45, 65, 75, 85, Inf)
agelabs       <- c("20-44", "45-64", "65-74", "75-84", "85+")
age_midpoints <- c(32.5, 55, 70, 80, 92.5)

# ---- Exposure-response function specification ----
varfun    <- "bs"
vardegree <- 2
varper    <- c(10, 75, 90)

# ---- Climate model settings ----
gcmexcl <- c("CMCC_CM2_SR5", "TaiESM1")

# ---- RCP / SSP scenarios ----
ssp_codes  <- c("1", "2", "3")
rcp_labels <- c("1" = "RCP 2.6", "2" = "RCP 4.5", "3" = "RCP 7.0")

# ---- RR component for mortality multiplier ----
# "total"  = full temperature-mortality curve (heat + cold effects)
# "heat"   = heat-only: RR kept for days > MMT, set to 1 for cold days
# "cold"   = cold-only: RR kept for days <= MMT, set to 1 for heat days
rr_component <- "total"

# ---- Adaptation scenarios (heat-risk attenuation by 2100) ----
adaptation_levels <- c(0, 0.50, 0.90)
adaptation_labels <- c("0%", "50%", "90%")

# ---- Adaptation time parameters ----
t0_adapt <- 2020
tf_adapt <- 2100

# ---- Historical reference period ----
histrange       <- c(2000, 2014)
hist_ref_period <- 2000:2014

# ---- Baseline temperature period ----
baseline_temp_period <- 1990:2019
baseline_temp_label  <- "1990-2019"

# ---- Temperature percentiles ----
predper <- c(seq(0, 1, 0.1), 2:98, seq(99, 100, 0.1))

# ---- Radix for life tables ----
radix <- 100000

# ---- Ensure output directories exist ----
if (!dir.exists(img_dir)) dir.create(img_dir, recursive = TRUE)
if (!dir.exists("results_csv")) dir.create("results_csv", recursive = TRUE)

cat(sprintf("Config loaded: %s (%s / %s)\n", city_name, city_code, nuts3_code))
