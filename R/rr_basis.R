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
