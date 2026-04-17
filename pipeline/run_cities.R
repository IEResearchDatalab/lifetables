################################################################################
#
# Run Pipeline for Multiple Cities
#
# Entry point for the climate-mortality pipeline. Loops over cities,
# computes seasonal weights, mortality multipliers, cohort life tables,
# EPVs, and generates plots for each city.
#
# Usage: Rscript pipeline/run_cities.R
#
################################################################################

# ---- Source shared function modules ----
source("R/utils.R")
source("R/load_data.R")
source("R/load_coefficients.R")
source("R/rr_basis.R")
source("R/epv.R")
source("R/cohort_lifetable.R")

# ---- Cities to process ----
cities <- c("Bucharest", "Helsinki", "Rome")

for (city_name in cities) {
	cat_header(sprintf("Processing city: %s", city_name))

	# Source config (sets city_code, nuts3_code, etc. based on city_name)
	source("pipeline/config.R")

	# Step 1: Compute seasonal mortality weights
	source("pipeline/compute_seasonal_weights.R")

	# Step 2: Compute mortality multipliers by age
	source("pipeline/compute_multipliers.R")

	# Step 3: Plot multiplier evolution
	source("pipeline/plot_multiplier_evolution.R")

	# Step 4: Compute cohort life tables and financial impact
	source("pipeline/compute_lifetables.R")

	cat(sprintf("\nFinished processing city: %s\n\n", city_name))
}

# ---- Aggregate results across cities ----
source("pipeline/aggregate_results.R")

# ---- Copy mortality projections ----
for (city in cities) {
	city_lower <- tolower(city)
	in_file  <- sprintf("data/%s_mortality_projections.csv", city_lower)
	out_file <- sprintf("results_csv/%s_mortality_projections.csv", city_lower)
	if (file.exists(in_file)) {
		file.copy(in_file, out_file, overwrite = TRUE)
	}
}

# ---- Package results ----
zip(zipfile = "results_csv.zip",
    files   = list.files("results_csv", full.names = TRUE))
zip_size <- file.info("results_csv.zip")$size / (1024 * 1024)
cat(sprintf("Created results_csv.zip (%.2f MB)\n", zip_size))

cat_header("PIPELINE COMPLETE")
