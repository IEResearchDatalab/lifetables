################################################################################
#
# Actuarial EPV Functions
#
# Expected Present Value calculations for life annuities and life insurance.
# All functions take a life table data.table with qx columns and return
# scalar EPV values.
#
################################################################################

#' Compute EPV of a whole-life annuity-due
#'
#' Pays 1 at the beginning of each year while the insured is alive.
#' Formula: ä_x = sum_{k=0}^{n-1} v^k * k_p_x
#'
#' @param lt data.table with qx column (must be sorted by age)
#' @param qx_col Name of the qx column to use (default "qx_base")
#' @param interest_rate Annual interest rate (default 0.02)
#' @return Scalar EPV value
compute_whole_life_annuity_epv <- function(lt, qx_col = "qx_base",
                                           interest_rate = 0.02) {
	v <- 1 / (1 + interest_rate)
	n <- nrow(lt)
	px <- 1 - lt[[qx_col]]
	kpx <- cumprod(c(1, px[-n]))  # kpx[k+1] = k_p_x

	k <- 0:(n - 1)
	epv <- sum(v^k * kpx)
	return(epv)
}

#' Compute EPV of a deferred term annuity-due
#'
#' Purchased at current age, payments start after deferral period.
#' Formula: _{d|m}ä_x = sum_{k=d}^{d+m-1} v^k * k_p_x
#'
#' @param lt data.table with qx column (must be sorted by age)
#' @param qx_col Name of the qx column to use (default "qx_base")
#' @param interest_rate Annual interest rate (default 0.02)
#' @param defer Number of years to defer (default 45)
#' @param term Number of payment years (default 20)
#' @return Scalar EPV value
compute_deferred_annuity_epv <- function(lt, qx_col = "qx_base",
                                         interest_rate = 0.02,
                                         defer = 45, term = 20) {
	v <- 1 / (1 + interest_rate)
	n <- nrow(lt)
	px <- 1 - lt[[qx_col]]
	kpx <- cumprod(c(1, px[-n]))

	k_start <- defer
	k_end <- min(defer + term - 1, n - 1)
	k_range <- k_start:k_end

	epv <- sum(v^k_range * kpx[k_range + 1])  # +1 for R 1-indexing
	return(epv)
}

#' Compute EPV of a whole-life insurance (Ax)
#'
#' Pays 1 at the end of the year of death.
#' Formula: A_x = sum_{k=0}^{n-1} v^{k+1} * k_p_x * q_{x+k}
#'
#' @param lt data.table with qx column (must be sorted by age)
#' @param qx_col Name of the qx column to use (default "qx_base")
#' @param interest_rate Annual interest rate (default 0.02)
#' @return Scalar EPV value
compute_insurance_epv <- function(lt, qx_col = "qx_base",
                                  interest_rate = 0.02) {
	v <- 1 / (1 + interest_rate)
	n <- nrow(lt)
	qx <- lt[[qx_col]]
	px <- 1 - qx
	kpx <- cumprod(c(1, px[-n]))

	epv <- sum(v^(1:n) * kpx * qx)
	return(epv)
}

#' Compute relative change (%) between base and climate EPV
#'
#' @param epv_base Baseline EPV
#' @param epv_clim Climate-adjusted EPV
#' @return Percentage change
pct_delta <- function(epv_base, epv_clim) {
	100 * (epv_clim - epv_base) / epv_base
}
