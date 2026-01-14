
library(arrow)
library(data.table)
library(dplyr)

# Define input file
input_parquet <- "results_parquet/city_period.parquet"
output_csv <- "city_period_2050_extracted_v2.csv"

cat(sprintf("Reading from %s...\n", input_parquet))

if (!file.exists(input_parquet)) {
  stop("Input file not found!")
}

# Open dataset
ds <- open_dataset(input_parquet)

# Filter criteria
target_year <- 2050
target_age <- "all"

# Read and filter
dt <- ds %>% 
  filter(period == target_year, agegroup == target_age) %>%
  collect() %>%
  as.data.table()

# Filter for Romania (Cities starting with RO)
dt <- dt[grepl("^RO", city)]

cat(sprintf("Loaded %d rows for year %d and age group '%s' (Romania only).\n", nrow(dt), target_year, target_age))

if (nrow(dt) > 0) {
  # The data is in long format with a 'sc' column (clim, full, demo, etc.)
  # and a value column 'an_est'.
  
  # Check unique scenarios
  print("Scenarios found:")
  print(unique(dt$sc))
  
  # Pivot to wide format
  # We want columns: an_full, an_clim, an_demo
  # Assuming keys are: city, ssp, period, adapt, range
  
  # Construct formula dynamically based on available columns
  # Note: adapt and range are dictionaries in parquet, data.table handles them fine usually, 
  # but check if they need conversion to character.
  if("adapt" %in% names(dt)) dt[, adapt := as.character(adapt)]
  if("range" %in% names(dt)) dt[, range := as.character(range)]
  
  dt_wide <- dcast(dt, city + ssp + period + adapt + range ~ sc, value.var = "an_est")
  
  # Rename columns to match requested format (e.g. 'full' -> 'an_full')
  scenarios <- unique(dt$sc)
  valid_cols <- names(dt_wide)
  
  for(scen in scenarios){
    if(scen %in% valid_cols){
      new_col <- paste0("an_", scen)
      setnames(dt_wide, scen, new_col)
    }
  }
  
  # Write to CSV
  fwrite(dt_wide, output_csv)
  cat(sprintf("Data exported to %s\n", output_csv))
  
  # Preview
  print(head(dt_wide))
  
} else {
  cat("No data found for the specified criteria.\n")
}
