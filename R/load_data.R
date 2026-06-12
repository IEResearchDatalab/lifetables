################################################################################
#
# Data Loading Functions
#
# Common data loading operations used across multiple pipeline scripts:
# - Projected temperature data from parquet
# - Eurostat mortality projections
# - Seasonal weight matrices
#
################################################################################

library(data.table)
library(arrow)
library(dplyr)  # for the %>% pipe operator

#' Load projected temperature data for a city
#'
#' Reads the tmeanproj.gz.parquet file, filters for the city, and adds
#' year and day-of-year columns.
#'
#' @param city_code URAU city code (e.g., "RO001C")
#' @param parquet_path Path to the parquet file (default "data/tmeanproj.gz.parquet")
#' @param gcmexcl Character vector of GCM names to exclude
#' @return List with: proj_data (data.table), gcm_cols (character vector)
load_projected_temperatures <- function(city_code,
                                        parquet_path = "data/tmeanproj.gz.parquet",
                                        gcmexcl = character(0)) {
	proj_data <- open_dataset(parquet_path) %>%
		dplyr::filter(URAU_CODE == city_code) %>%
		dplyr::collect() %>%
		as.data.table()

	proj_data[, year := year(date)]
	proj_data[, doy := as.integer(format(date, "%j"))]
	proj_data[doy > 365, doy := 365L]  # cap leap-year day 366

	gcm_cols <- names(proj_data)[grepl("^tas_", names(proj_data))]
	gcm_cols <- gcm_cols[!gsub("tas_", "", gcm_cols) %in% gcmexcl]

	cat(sprintf("Loaded projected temperatures: %d rows, %d GCMs (city: %s)\n",
	            nrow(proj_data), length(gcm_cols), city_code))

	return(list(proj_data = proj_data, gcm_cols = gcm_cols))
}

#' Extract historical temperatures from projected data
#'
#' @param proj_data data.table from load_projected_temperatures
#' @param gcm_cols Character vector of GCM column names
#' @return Numeric vector of historical temperatures (no NAs)
extract_hist_temps <- function(proj_data, gcm_cols) {
	hist_data <- proj_data[ssp == "hist"]
	hist_temps <- unlist(hist_data[, ..gcm_cols], use.names = FALSE)
	hist_temps <- hist_temps[!is.na(hist_temps)]
	cat(sprintf("Historical temperatures: %d values, range [%.1f, %.1f]°C\n",
	            length(hist_temps), min(hist_temps), max(hist_temps)))
	return(hist_temps)
}

#' Pool baseline temperatures from historical + early projection data
#'
#' @param proj_data data.table from load_projected_temperatures
#' @param gcm_cols Character vector of GCM column names
#' @param ssp_codes Character vector of SSP codes
#' @param baseline_temp_period Integer vector of years to include
#' @return List with: temps (numeric vector), doys (integer vector)
pool_baseline_temperatures <- function(proj_data, gcm_cols, ssp_codes,
                                       baseline_temp_period) {
	baseline_hist <- proj_data[ssp == "hist" & year %in% baseline_temp_period]
	baseline_proj <- proj_data[ssp %in% ssp_codes &
	                           year %in% baseline_temp_period & year > 2014]

	temps <- c(
		unlist(baseline_hist[, ..gcm_cols], use.names = FALSE),
		unlist(baseline_proj[, ..gcm_cols], use.names = FALSE)
	)

	doys <- c(
		rep(baseline_hist$doy, length(gcm_cols)),
		rep(baseline_proj$doy, length(gcm_cols))
	)

	valid <- !is.na(temps)
	temps <- temps[valid]
	doys  <- doys[valid]

	cat(sprintf("Baseline temperatures (%d-%d): %d values, mean %.2f°C\n",
	            min(baseline_temp_period), max(baseline_temp_period),
	            length(temps), mean(temps)))

	return(list(temps = temps, doys = doys))
}

#' Load seasonal mortality weights into a matrix
#'
#' @param city_name_lower Lowercase city name
#' @return List with: sw_matrix (81 x 365 matrix), available (logical)
load_seasonal_weights <- function(city_name_lower) {
	sw_file <- sprintf("results_csv/seasonal_weights_daily_%s.csv", city_name_lower)

	if (!file.exists(sw_file)) {
		cat("Seasonal weights not found — using uniform weighting\n")
		return(list(sw_matrix = NULL, available = FALSE))
	}

	sw_dt <- fread(sw_file)
	sw_matrix <- matrix(1 / 365, nrow = 81, ncol = 365,
	                    dimnames = list(20:100, 1:365))

	for (i in seq_len(nrow(sw_dt))) {
		a <- sw_dt$age[i]
		d <- sw_dt$doy[i]
		sw_matrix[as.character(a), d] <- sw_dt$weight[i]
	}

	cat("Loaded seasonal mortality weights (age x DOY)\n")
	return(list(sw_matrix = sw_matrix, available = TRUE))
}
