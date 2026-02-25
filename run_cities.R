for (city_name in c("Bucharest", "Helsinki", "Rome")) {
  cat(sprintf("Processing city: %s\n", city_name))
  
  # Source the config file to set city-specific parameters
  source("config.R")

  # Plot temperatures
  #source("plot_bucharest_temp_dist_rr.R")
  source("plot_bucharest_temp_evolution.R")

  # Compute seasonal mortality weights
  source("compute_seasonal_weights.R")
  
  # Run the mortality multiplier computation script
  source("compute_mortality_multiplier_by_age.R")
  source("plot_mortality_multiplier_evolution.R")
  
  # Run the cohort lifetable computation script
  source("compute_cohort_lifetable_financial.R")
  
  cat(sprintf("Finished processing city: %s\n\n", city_name))
}

# Aggregate results
source("aggregate_financial_impact.R")

# Include the mortality projections from "data" folder
# (hardcode it here)
for (city in c("Bucharest", "Helsinki", "Rome")) {
  cat(sprintf("Processing mortality projections for city: %s\n", city))
  
  # Load the mortality projections CSV file for the city
  mortality_file <- sprintf("data/%s_mortality_projections.csv", tolower(city))
  # Copy it into the "results_csv" directory with a standardized name
  output_file <- sprintf("results_csv/%s_mortality_projections.csv", tolower(city))
  file.copy(mortality_file, output_file, overwrite = TRUE)
}

# Zip the output directory "results_csv"
zip(zipfile = "results_csv.zip", files = list.files("results_csv", full.names = TRUE))
# Show size of the zip file
zip_size <- file.info("results_csv.zip")$size / (1024 * 1024) # size in MB
cat(sprintf("Created results_csv.zip (%.2f MB)\n", zip_size))