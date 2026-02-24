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

# Zip the output directory "results_csv"
zip(zipfile = "results_csv.zip", files = list.files("results_csv", full.names = TRUE))