for (city_name in c("Bucharest", "Helsinki", "Rome")) {
  cat(sprintf("Processing city: %s\n", city_name))
  
  # Source the config file to set city-specific parameters
  source("config.R")
  
  # Run the mortality multiplier computation script
  source("compute_mortality_multiplier_by_age.R")
  
  # Run the cohort lifetable computation script
  source("compute_cohort_lifetable_financial.R")
  
  cat(sprintf("Finished processing city: %s\n\n", city_name))
}