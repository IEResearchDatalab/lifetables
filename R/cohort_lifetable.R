################################################################################
#
# Life Table Functions
#
# Functions for building cohort life tables with climate-adjusted mortality.
# The existing functions/lifetable.R provides a period life table function;
# this module adds cohort life table construction used by the main pipeline.
#
################################################################################

#' Build a cohort life table with climate-adjusted mortality
#'
#' Follows a cohort from starting age through the projection years,
#' merging year-specific projected mortality with climate mortality multipliers.
#'
#' @param mort_proj_dt data.table with columns: year, age, qx, mx, ax
#' @param mult_dt data.table with columns: year, age, ssp, adaptation, multiplier
#' @param ssp_val SSP code (e.g., "1", "2", "3")
#' @param adapt_lab Adaptation label (e.g., "0%", "50%", "90%")
#' @param rcp_labels Named character vector mapping ssp codes to RCP labels
#' @param cohort_start_age Starting age of the cohort
#' @param cohort_years Integer vector of calendar years
#' @param radix Starting population (default 100000)
#' @return data.table with cohort life table columns
build_cohort_lifetable <- function(mort_proj_dt, mult_dt, ssp_val, adapt_lab,
                                   rcp_labels,
                                   cohort_start_age, cohort_years,
                                   radix = 100000) {

	rcp_lab <- rcp_labels[ssp_val]

	# Cohort ages and years
	cohort_age <- cohort_start_age:(cohort_start_age + length(cohort_years) - 1)
	cohort_years_vec <- cohort_years

	# Initialize
	lt <- data.table::data.table(
		age        = cohort_age,
		year       = cohort_years_vec[1:length(cohort_age)],
		rcp        = rcp_lab,
		adaptation = adapt_lab
	)

	# Merge year-specific baseline qx and mx from projections
	lt <- merge(lt,
	            mort_proj_dt[, .(year, age, qx_base = qx, mx_base = mx, ax)],
	            by = c("year", "age"),
	            all.x = TRUE)

	# Handle missing mortality data
	if (any(is.na(lt$qx_base))) {
		missing <- lt[is.na(qx_base), .(year, age)]
		warning(sprintf("Missing mortality data for %d age-year combinations", nrow(missing)))
		lt[is.na(qx_base), qx_base := mort_proj_dt[year == max(year) & age == .BY$age, qx], by = age]
		lt[is.na(mx_base), mx_base := mort_proj_dt[year == max(year) & age == .BY$age, mx], by = age]
		lt[is.na(ax), ax := 0.5]
	}

	# Merge climate mortality multipliers
	lt <- merge(lt,
	            mult_dt[ssp == ssp_val & adaptation == adapt_lab,
	                    .(year, age, multiplier)],
	            by = c("year", "age"), all.x = TRUE)
	lt[is.na(multiplier), multiplier := 1]

	# Climate-adjusted mortality
	lt[, mx_clim := mx_base * multiplier]
	lt[, qx_clim := mx_to_qx(mx_clim, ax)]

	# Cap qx at 1
	lt[qx_base > 1, qx_base := 1]
	lt[qx_clim > 1, qx_clim := 1]

	# Compute survivorship
	lt <- lt[order(age)]

	# Baseline
	lt[, lx_base := radix]
	for (i in 2:nrow(lt)) {
		lt$lx_base[i] <- lt$lx_base[i - 1] * (1 - lt$qx_base[i - 1])
	}
	lt[, dx_base := lx_base * qx_base]

	# Climate-adjusted
	lt[, lx_clim := radix]
	for (i in 2:nrow(lt)) {
		lt$lx_clim[i] <- lt$lx_clim[i - 1] * (1 - lt$qx_clim[i - 1])
	}
	lt[, dx_clim := lx_clim * qx_clim]

	return(lt)
}
