################################################################################
#
# Coefficient Loading and Interpolation
#
# Functions to load MCC exposure-response coefficients from coefs.csv,
# filter for a target city, and interpolate from age-group midpoints
# to single-year ages.
#
################################################################################

#' Load MCC coefficients for a given city
#'
#' @param city_code URAU city code (e.g., "RO001C")
#' @param coefs_path Path to the coefficients file (default "data/coefs.csv")
#' @return data.table of coefficients filtered for the city
load_city_coefficients <- function(city_code, coefs_path = "data/coefs.csv") {
	coefs_all <- data.table::fread(coefs_path)
	coefs_city <- coefs_all[URAU_CODE == city_code]

	if (nrow(coefs_city) == 0) {
		stop(sprintf("No coefficients found for city_code = '%s' in %s", city_code, coefs_path))
	}

	cat(sprintf("Loaded coefficients for %s: %d age groups\n", city_code, nrow(coefs_city)))
	return(list(all = coefs_all, city = coefs_city))
}

#' Interpolate age-group coefficients to single-year ages
#'
#' @param coefs_city data.table of city coefficients (one row per age group)
#' @param coefs_all  data.table of all coefficients (for column order reference)
#' @param agelabs    Character vector of age group labels matching coefs_city$agegroup
#' @param age_midpoints Numeric midpoints for each age group
#' @param age_range  Integer vector of target single-year ages (default 20:100)
#' @param city_code  City code to fill in URAU_CODE column
#' @return data.table with one row per single-year age, same columns as original
interpolate_coefs_to_single_age <- function(coefs_city, coefs_all,
                                            agelabs, age_midpoints,
                                            age_range = 20:100,
                                            city_code = NULL) {
	# Identify coefficient columns (b1..bk)
	coef_cols <- names(coefs_city)[grepl("^b[0-9]+$", names(coefs_city))]
	if (length(coef_cols) == 0) stop("No coefficient columns b1..bk found.")

	# Order by agelabs
	coefs_city_ord <- coefs_city[match(agelabs, agegroup)]
	if (any(is.na(coefs_city_ord$agegroup))) {
		stop("Some agelabs not found in coefs_city$agegroup. Check config agelabs.")
	}

	# Interpolate each coefficient column
	coef_interp_dt <- data.table::data.table(agegroup = as.character(age_range))
	for (cc in coef_cols) {
		y <- coefs_city_ord[[cc]]
		coef_interp_dt[[cc]] <- stats::approx(
			x = age_midpoints, y = y,
			xout = age_range, rule = 2
		)$y
	}

	# Build output with same columns as original
	template <- coefs_city_ord[1]
	orig_cols <- names(coefs_all)
	coefs_single_age <- template[rep(1, length(age_range))]

	coefs_single_age[, agegroup := as.character(age_range)]
	for (cc in coef_cols) coefs_single_age[[cc]] <- coef_interp_dt[[cc]]

	if (!is.null(city_code) && "URAU_CODE" %in% names(coefs_single_age)) {
		coefs_single_age[, URAU_CODE := city_code]
	}

	# Ensure exact column order
	missing_cols <- setdiff(orig_cols, names(coefs_single_age))
	if (length(missing_cols) > 0) {
		stop(sprintf("Interpolated coefs missing columns: %s",
		             paste(missing_cols, collapse = ", ")))
	}
	coefs_single_age <- coefs_single_age[, ..orig_cols]

	cat(sprintf("Interpolated to %d single-year ages (%d-%d)\n",
	            nrow(coefs_single_age), min(age_range), max(age_range)))
	return(coefs_single_age)
}
