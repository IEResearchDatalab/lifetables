# Load config once to get the list of cities
city_name <- "Bucharest"  # Default city for initial config loading

source("legacy/config.R")
list_of_cities <- names(city_configs)

for (city_name in list_of_cities) {
  cat(sprintf("Processing city: %s\n", city_name))
  
  # Source the config file to set city-specific parameters
  source("legacy/config.R")

  # Plot temperatures
  source("legacy/plot_bucharest_temp_evolution.R")

  # Compute seasonal mortality weights
  #source("legacy/compute_seasonal_weights.R")
  
  # Run the mortality multiplier computation script
  source("legacy/compute_mortality_multiplier_by_age.R")
  source("legacy/plot_mortality_multiplier_evolution.R")
  
  # Download mortality projections from Eurostat and compute central death rates
  source("legacy/extract_eurostat_projections.R")

  # Run the cohort lifetable computation script
  source("legacy/compute_cohort_lifetable_financial.R")
  
  cat(sprintf("Finished processing city: %s\n\n", city_name))
}

# Aggregate results
source("legacy/aggregate_financial_impact.R")

# Zip the output directory "results_csv"
zip(zipfile = "results_csv.zip", files = list.files("results_csv", full.names = TRUE))
# Show size of the zip file
zip_size <- file.info("results_csv.zip")$size / (1024 * 1024) # size in MB
cat(sprintf("Created results_csv.zip (%.2f MB)\n", zip_size))

# Add "config_city_codes.json" to the zip file for reference
zip(zipfile = "results_csv.zip", files = "config_city_codes.json", flags = "-j")