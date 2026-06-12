################################################################################
#
# RR Basis Construction and Prediction
#
# Functions to build the B-spline basis from historical temperature data,
# compute age-group RR curves, interpolate to single-year ages, find
# Minimum Mortality Temperature (MMT), and compute average RR for
# temperature vectors with optional heat/cold decomposition.
#
################################################################################

library(dlnm)
library(splines)

#' Build basis function parameters from historical temperature data
#'
#' @param hist_temps Numeric vector of historical temperatures
#' @param varfun Basis function type (default "bs")
#' @param vardegree Degree of basis (default 2)
#' @param varper Percentiles for knot placement (default c(10, 75, 90))
#' @return List with argvar (basis specification), varknots, varbound
build_basis_params <- function(hist_temps,
                               varfun = "bs",
                               vardegree = 2,
                               varper = c(10, 75, 90)) {
	varknots <- quantile(hist_temps, varper / 100, na.rm = TRUE)
	varbound <- range(hist_temps, na.rm = TRUE)

	argvar <- list(fun = varfun, degree = vardegree,
	               knots = varknots, Bound = varbound)

	cat(sprintf("Basis params: %s(degree=%d), %d knots, range [%.1f, %.1f]°C\n",
	            varfun, vardegree, length(varknots), varbound[1], varbound[2]))

	return(list(argvar = argvar, varknots = varknots, varbound = varbound))
}

#' Compute RR curves for each age group
#'
#' @param coefs_city data.table of city coefficients (one row per age group)
#' @param agelabs Character vector of age group labels
#' @param age_midpoints Numeric midpoints for each age group
#' @param argvar List of basis function parameters (from build_basis_params)
#' @param varbound Numeric vector of length 2 (temperature range)
#' @param temp_step Temperature grid step size (default 0.5)
#' @return List with: temp_seq, rr_matrix, mmt_vec, basis
compute_rr_curves <- function(coefs_city, agelabs, age_midpoints,
                              argvar, varbound, temp_step = 0.5) {
	temp_seq <- seq(varbound[1], varbound[2], by = temp_step)
	n_temp <- length(temp_seq)

	# Build basis on temperature grid
	basis <- do.call(onebasis, c(list(x = temp_seq), argvar))

	rr_matrix <- matrix(NA, nrow = n_temp, ncol = length(agelabs))
	mmt_vec <- numeric(length(agelabs))

	for (i in seq_along(agelabs)) {
		age <- agelabs[i]
		coef_row <- coefs_city[agegroup == age]
		coefs <- as.numeric(coef_row[, .(b1, b2, b3, b4, b5)])

		log_rr <- basis %*% coefs

		# Find MMT in 25-99 percentile range
		ind <- temp_seq >= quantile(temp_seq, 0.25) &
			   temp_seq <= quantile(temp_seq, 0.99)
		mmt <- temp_seq[ind][which.min(log_rr[ind])]
		mmt_vec[i] <- mmt

		# Center at MMT
		cenvec <- do.call(onebasis, c(list(x = mmt), argvar))
		log_rr_centered <- log_rr - drop(cenvec %*% coefs)

		rr <- pmax(exp(log_rr_centered), 1)
		rr_matrix[, i] <- as.vector(rr)

		cat(sprintf("  %s (midpoint: %.1f): MMT = %.1f°C\n",
		            age, age_midpoints[i], mmt))
	}

	return(list(
		temp_seq  = temp_seq,
		rr_matrix = rr_matrix,
		mmt_vec   = mmt_vec,
		basis     = basis
	))
}

#' Interpolate RR from age-group to single-year ages
#'
#' @param rr_matrix Matrix (n_temp x n_agegroups)
#' @param mmt_vec Numeric vector of MMT per age group
#' @param age_midpoints Numeric midpoints of age groups
#' @param age_range Integer vector of target single-year ages
#' @return List with: rr_single_age (matrix), mmt_single_age (vector)
interpolate_rr_to_single_age <- function(rr_matrix, mmt_vec,
                                         age_midpoints,
                                         age_range = 20:100) {
	n_temp <- nrow(rr_matrix)
	rr_single_age <- matrix(NA, nrow = n_temp, ncol = length(age_range))
	colnames(rr_single_age) <- age_range

	for (t_idx in seq_len(n_temp)) {
		rr_at_temp <- rr_matrix[t_idx, ]
		rr_single_age[t_idx, ] <- approx(
			x = age_midpoints, y = rr_at_temp,
			xout = age_range, rule = 2
		)$y
	}

	mmt_single_age <- approx(
		x = age_midpoints, y = mmt_vec,
		xout = age_range, rule = 2
	)$y

	cat(sprintf("Interpolated RR to %d single-year ages\n", length(age_range)))
	return(list(rr_single_age = rr_single_age, mmt_single_age = mmt_single_age))
}

#' Compute average RR for a temperature vector at each single-year age
#'
#' @param temps Numeric vector of daily temperatures
#' @param temp_seq Numeric vector of temperature grid points
#' @param rr_single_age Matrix (n_temp x n_ages) of RR values
#' @param mmt_single_age Numeric vector of MMT per single-year age
#' @param age_range Integer vector of ages
#' @param component One of "total", "heat", "cold"
#' @param doys Integer vector of day-of-year corresponding to temps (optional)
#' @param sw_matrix Seasonal weight matrix (age x doy) or NULL for uniform
#' @return Named numeric vector of average RR per age
compute_avg_rr_by_age <- function(temps, temp_seq, rr_single_age,
                                  mmt_single_age, age_range,
                                  component = "total",
                                  doys = NULL, sw_matrix = NULL) {
	temps <- temps[!is.na(temps)]
	if (length(temps) == 0) return(rep(NA_real_, length(age_range)))

	# Map temperatures to nearest index in temp_seq
	temp_indices <- vapply(temps, function(t) which.min(abs(temp_seq - t)),
	                       integer(1))

	# Extract RR values
	rr_vals <- rr_single_age[temp_indices, , drop = FALSE]

	# Apply heat/cold decomposition
	if (component != "total") {
		for (j in seq_along(age_range)) {
			mmt <- mmt_single_age[j]
			if (component == "heat") {
				rr_vals[temps <= mmt, j] <- 1
			} else if (component == "cold") {
				rr_vals[temps > mmt, j] <- 1
			}
		}
	}

	# Weighted or uniform average
	if (!is.null(sw_matrix) && !is.null(doys)) {
		avg_rr <- numeric(length(age_range))
		for (j in seq_along(age_range)) {
			w <- sw_matrix[as.character(age_range[j]), doys]
			avg_rr[j] <- weighted.mean(rr_vals[, j], w)
		}
	} else {
		avg_rr <- colMeans(rr_vals)
	}

	names(avg_rr) <- as.character(age_range)
	return(avg_rr)
}
#' Compute country-level RR curves by averaging city ERFs on a common grid
#'
#' Rigorous alternative to averaging B-spline coefficient vectors.
#' For each city: builds a city-specific B-spline basis from that city's
#' historical temperature distribution, evaluates the city ERF on the
#' common country temperature grid (uncentered log-RR), then takes a
#' population-weighted average across all cities in the country.
#' MMT is found on the averaged curve and centering is applied AFTER
#' averaging, so the country MMT reflects the composite exposure.
#'
#' This avoids the basis-mismatch error of coefficient averaging: each
#' city's coefficients are only ever multiplied by the basis built from
#' that city's own temperature distribution.
#'
#' @param coefs_cities  data.table of city coefficients for all cities in the
#'                      country (URAU_CODE, agegroup, b1-b5)
#' @param city_hist_temps Named list: city URAU_CODE -> numeric vector of
#'                        historical daily temperatures
#' @param city_pop_weights Named numeric vector: city URAU_CODE -> population
#'                         weight (need not sum to 1; normalised internally)
#' @param agelabs        Character vector of age group labels
#' @param age_midpoints  Numeric midpoints for each age group
#' @param country_varbound Numeric vector of length 2: country temperature range
#' @param varfun         Basis function type (default "bs")
#' @param vardegree      Degree of basis (default 2)
#' @param varper         Percentiles for city knot placement (default c(10,75,90))
#' @param temp_step      Temperature grid step size in °C (default 0.5)
#' @return List with: temp_seq, rr_matrix (n_temp x n_age), mmt_vec
compute_country_rr_curves <- function(coefs_cities, city_hist_temps,
                                      city_pop_weights,
                                      agelabs, age_midpoints,
                                      country_varbound,
                                      varfun = "bs", vardegree = 2,
                                      varper = c(10, 75, 90),
                                      temp_step = 0.5) {
	city_codes <- unique(coefs_cities$URAU_CODE)
	temp_seq   <- seq(country_varbound[1], country_varbound[2], by = temp_step)
	n_temp     <- length(temp_seq)

	# Normalise population weights (fall back to equal weights if missing)
	pop_w <- city_pop_weights[city_codes]
	pop_w[is.na(pop_w)] <- mean(pop_w, na.rm = TRUE)
	if (all(is.na(pop_w)) || sum(pop_w, na.rm = TRUE) == 0)
		pop_w[] <- 1
	pop_w <- pop_w / sum(pop_w, na.rm = TRUE)

	# Pre-build one B-spline basis per city on the COUNTRY temperature grid,
	# using the CITY's own historical percentiles as knots.
	# This is the only valid way to evaluate city coefficients outside their
	# original basis space.
	cat(sprintf("  Building city bases for %d cities...\n", length(city_codes)))
	city_bases <- lapply(city_codes, function(city) {
		ch <- city_hist_temps[[city]]
		if (length(ch) < 20) return(NULL)
		city_knots  <- quantile(ch, varper / 100, na.rm = TRUE)
		city_bound  <- range(ch, na.rm = TRUE)
		city_argvar <- list(fun = varfun, degree = vardegree,
		                    knots = city_knots, Bound = city_bound)
		do.call(onebasis, c(list(x = temp_seq), city_argvar))
	})
	names(city_bases) <- city_codes

	rr_matrix <- matrix(NA_real_, nrow = n_temp, ncol = length(agelabs))
	mmt_vec   <- numeric(length(agelabs))

	for (i in seq_along(agelabs)) {
		age <- agelabs[i]

		# Weighted sum of uncentered log-RR curves across cities
		log_rr_wsum  <- numeric(n_temp)
		weight_total <- 0

		for (city in city_codes) {
			basis_city <- city_bases[[city]]
			if (is.null(basis_city)) next

			coef_row <- coefs_cities[URAU_CODE == city & agegroup == age]
			if (nrow(coef_row) == 0L) next

			coefs_vec    <- as.numeric(coef_row[, .(b1, b2, b3, b4, b5)])
			log_rr_city  <- as.vector(basis_city %*% coefs_vec)
			w            <- pop_w[city]
			log_rr_wsum  <- log_rr_wsum + w * log_rr_city
			weight_total <- weight_total + w
		}

		if (weight_total == 0) {
			warning(sprintf("No valid cities for age group %s", age))
			next
		}

		log_rr_avg <- log_rr_wsum / weight_total

		# Find country MMT in 25th-99th percentile of the country temperature grid
		ind     <- temp_seq >= quantile(temp_seq, 0.25) &
		           temp_seq <= quantile(temp_seq, 0.99)
		mmt_idx <- which(ind)[which.min(log_rr_avg[ind])]
		mmt     <- temp_seq[mmt_idx]
		mmt_vec[i] <- mmt

		# Centre at country MMT, floor at RR = 1
		log_rr_centered <- log_rr_avg - log_rr_avg[mmt_idx]
		rr_matrix[, i]  <- pmax(exp(log_rr_centered), 1)

		cat(sprintf("  %s (midpoint: %.1f): MMT = %.1f°C\n",
		            age, age_midpoints[i], mmt))
	}

	return(list(
		temp_seq  = temp_seq,
		rr_matrix = rr_matrix,
		mmt_vec   = mmt_vec
	))
}
