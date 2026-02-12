################################################################################
#
# City Configuration
#
# This file defines the target city for all analyses. All pipeline scripts
# source this file to get the city parameters.
#
# To analyse a different city, change the parameters below.
# The URAU_CODE must match an entry in data/coefs.csv and the projected
# temperature parquet. The NUTS3 code is used for Eurostat EUROPOP2019
# regional mortality projections.
#
################################################################################

# ---- City identifiers ----
city_code  <- "RO001C"       # URAU code (Urban Audit)
city_name  <- "Bucharest"    # Human-readable name (used in titles and captions)
nuts3_code <- "RO321"        # NUTS 3 code (Eurostat projections)

# ---- Output directory for figures (used by main.tex) ----
img_dir <- "img"

# ---- Cohort parameters ----
cohort_start_age  <- 20      # Starting age of the cohort
cohort_start_year <- 2023    # Calendar year at cohort start
cohort_end_year   <- 2099    # Calendar year at cohort end

# ---- Interest rate for EPV calculations ----
interest_rate   <- 0.02      # 2% annual discount rate
discount_factor <- 1 / (1 + interest_rate)

# ---- Age groups (for RR coefficients) ----
agebreaks    <- c(20, 45, 65, 75, 85, Inf)
agelabs      <- c("20-44", "45-64", "65-74", "75-84", "85+")
age_midpoints <- c(32.5, 55, 70, 80, 92.5)

# ---- Exposure-response function specification ----
varfun    <- "bs"
vardegree <- 2
varper    <- c(10, 75, 90)

# ---- Climate model settings ----
gcmexcl <- c("CMCC_CM2_SR5", "TaiESM1")

# ---- RCP / SSP scenarios ----
ssp_codes  <- c("1", "2", "3", "5")
rcp_labels <- c("1" = "RCP 2.6", "2" = "RCP 4.5", "3" = "RCP 7.0", "5" = "RCP 8.5")

# ---- Adaptation scenarios (heat-risk attenuation by 2100) ----
adaptation_levels <- c(0, 0.50, 0.90)
adaptation_labels <- c("0%", "50%", "90%")

# ---- Adaptation time parameters ----
t0_adapt <- 2020
tf_adapt <- 2100

# ---- Historical reference period ----
histrange       <- c(2000, 2014)
hist_ref_period <- 2000:2014

# ---- Temperature percentiles ----
predper <- c(seq(0, 1, 0.1), 2:98, seq(99, 100, 0.1))

# ---- Radix for life tables ----
radix <- 100000

# ---- Derived file paths (based on city_name) ----
city_name_lower <- tolower(city_name)

# Ensure output directory exists
if (!dir.exists(img_dir)) dir.create(img_dir, recursive = TRUE)
if (!dir.exists("results_csv")) dir.create("results_csv", recursive = TRUE)

cat(sprintf("Config loaded: %s (%s / %s)\n", city_name, city_code, nuts3_code))
