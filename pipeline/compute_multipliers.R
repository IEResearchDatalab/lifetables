################################################################################
#
# Compute Mortality Multipliers by Age
#
# Orchestration script: loads temperature data, builds RR basis, computes
# mortality multipliers for all year × component × age combinations.
#
# Depends on: R/utils.R, R/load_data.R, R/load_coefficients.R, R/rr_basis.R
# Expects: config.R already sourced (city_name, city_code, etc. set)
#
################################################################################

library(data.table)
library(arrow)
library(dplyr)
library(dlnm)
library(splines)
library(ggplot2)
library(scales)

cat_step(1, "Loading projected temperature data")

temp_data <- load_projected_temperatures(city_code, gcmexcl = gcmexcl)
proj_data <- temp_data$proj_data
gcm_cols  <- temp_data$gcm_cols

# --- Load seasonal mortality weights ---
sw <- load_seasonal_weights(city_name_lower)

# --- Load and interpolate coefficients ---
cat_step(2, "Loading RR coefficients")

coefs <- load_city_coefficients(city_code)
coefs_single_age <- interpolate_coefs_to_single_age(
	coefs$city, coefs$all,
	agelabs, age_midpoints,
	age_range = 20:100,
	city_code = city_code
)

# Save interpolated coefficients
out_path <- sprintf("results_csv/coefs_%s.csv", city_name_lower)
fwrite(coefs_single_age, out_path)
cat(sprintf("  Saved: %s\n", out_path))

# --- Build basis parameters ---
cat_step(3, "Building basis function parameters")

hist_temps <- extract_hist_temps(proj_data, gcm_cols)
basis_params <- build_basis_params(hist_temps, varfun, vardegree, varper)

# --- Compute RR curves ---
cat_step(4, "Computing RR curves for each age group")

rr_result <- compute_rr_curves(
	coefs$city, agelabs, age_midpoints,
	basis_params$argvar, basis_params$varbound
)

# --- Interpolate RR to single-year ages ---
cat_step(5, "Interpolating RR to single-year ages")

rr_interp <- interpolate_rr_to_single_age(
	rr_result$rr_matrix, rr_result$mmt_vec,
	age_midpoints, age_range = 20:100
)

# --- Pool baseline temperatures ---
cat_step(6, sprintf("Pooling baseline temperatures (%s)", baseline_temp_label))

baseline <- pool_baseline_temperatures(
	proj_data, gcm_cols, ssp_codes, baseline_temp_period
)
# Make available for downstream scripts
baseline_temps_all <- baseline$temps

# --- Target years ---
target_years <- c(2050, 2075, 2099)
target_ssp   <- "3"  # RCP 7.0
components   <- c("heat", "cold", "total")
age_range    <- 20:100

# --- Pool target-year temperatures ---
cat_step(7, "Pooling target-year temperatures")

target_temps <- list()
target_doys  <- list()
for (yr in target_years) {
	year_data <- proj_data[ssp == target_ssp & year == yr]
	all_temps <- c()
	all_doys  <- c()
	for (gcm_col in gcm_cols) {
		all_temps <- c(all_temps, year_data[[gcm_col]])
		all_doys  <- c(all_doys, year_data$doy)
	}
	valid_yr <- !is.na(all_temps)
	target_temps[[as.character(yr)]] <- all_temps[valid_yr]
	target_doys[[as.character(yr)]]  <- all_doys[valid_yr]
	cat(sprintf("  %d: %d daily values\n", yr, sum(valid_yr)))
}

# --- Compute multipliers ---
cat_step(8, "Computing multipliers for heat / cold / total")

results_list <- list()

for (comp in components) {
	cat(sprintf("  Component: %s\n", comp))

	rr_baseline <- compute_avg_rr_by_age(
		baseline$temps, rr_result$temp_seq,
		rr_interp$rr_single_age, rr_interp$mmt_single_age,
		age_range, component = comp,
		doys = baseline$doys, sw_matrix = sw$sw_matrix
	)

	for (yr in target_years) {
		yr_str <- as.character(yr)
		avg_rr <- compute_avg_rr_by_age(
			target_temps[[yr_str]], rr_result$temp_seq,
			rr_interp$rr_single_age, rr_interp$mmt_single_age,
			age_range, component = comp,
			doys = target_doys[[yr_str]], sw_matrix = sw$sw_matrix
		)
		multiplier <- avg_rr / rr_baseline

		results_list[[length(results_list) + 1]] <- data.table(
			component  = comp,
			year       = yr,
			age        = age_range,
			avg_rr     = avg_rr,
			multiplier = multiplier
		)
	}
}

results <- rbindlist(results_list)
results[, year_label := factor(year)]
results[, component := factor(component, levels = c("heat", "total", "cold"),
                              labels = c("Heat", "Total", "Cold"))]

cat("\nMultiplier Summary:\n")
print(results[, .(min_mult = round(min(multiplier), 4),
                   max_mult = round(max(multiplier), 4),
                   mult_at_65 = round(multiplier[age == 65], 4)),
              by = .(component, year)])

# --- Save ---
cat_step(9, "Saving results")
fwrite(results, sprintf("results_csv/mortality_multiplier_by_age_%s.csv", city_name_lower))
cat(sprintf("  Saved: results_csv/mortality_multiplier_by_age_%s.csv\n", city_name_lower))

cat("\nMultiplier computation complete.\n")
