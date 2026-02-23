# Script to aggregate all financial_impact_summary_*.csv files into a single summary table
# Output: financial_impact_total.csv

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Find all relevant CSV files in results_csv/
files <- list.files("results_csv", pattern = "^financial_impact_summary_.*\\.csv$", full.names = TRUE)

# Prepare a list to store results
total_list <- list()

for (file in files) {
  # Extract city name from filename
  city <- str_match(basename(file), "financial_impact_summary_(.*)\\.csv")[,2]
  
  # Read the CSV
  df <- read_csv(file, show_col_types = FALSE)
  
  # Filter for adaptation == "0%"
  df0 <- df %>% filter(adaptation == "0%")
  
  # Select relevant columns
  annuity <- df0 %>% select(rcp, pct_delta_annuity)
  insurance <- df0 %>% select(rcp, pct_delta_insurance)
  
  # Pivot to wide format
  annuity_wide <- annuity %>% pivot_wider(names_from = rcp, values_from = pct_delta_annuity)
  insurance_wide <- insurance %>% pivot_wider(names_from = rcp, values_from = pct_delta_insurance)
  
  # Add Product and City columns
  annuity_wide <- annuity_wide %>% mutate(Product = "Annuity", City = city) %>% select(Product, City, everything())
  insurance_wide <- insurance_wide %>% mutate(Product = "Insurance", City = city) %>% select(Product, City, everything())
  
  # Store
  total_list[[length(total_list)+1]] <- annuity_wide
  total_list[[length(total_list)+1]] <- insurance_wide
}

# Combine all
final_df <- bind_rows(total_list)

# Arrange columns: Product, City, then RCPs
rcp_cols <- sort(setdiff(names(final_df), c("Product", "City")))
final_df <- final_df %>% select(Product, City, all_of(rcp_cols))


# Capitalize city names
final_df$City <- stringr::str_to_title(final_df$City)

# Round all numeric RCP columns to 3 decimals
for (col in rcp_cols) {
  if (is.numeric(final_df[[col]])) {
    final_df[[col]] <- round(final_df[[col]], 3)
  }
}

# Sort by Product, City
final_df <- final_df %>% arrange(Product, City)

# Write to CSV
write_csv(final_df, "results_csv/financial_impact_total.csv")

cat("Wrote results_csv/financial_impact_total.csv\n")
