################################################################################
# 
# 04_city_le_comparison.R
#
# Summarize Life Expectancy (e0) comparisons for Romanian cities.
# Input: results_csv/romania_city_lifetables_full.csv (Output from script 05)
# Output: results_csv/city_life_expectancy_summary.csv
#
################################################################################

library(data.table)

# 1. Load Data
# ----------------
input_file <- "results_csv/romania_city_lifetables_full.csv"

if(!file.exists(input_file)){
  stop(paste("Input file not found:", input_file, "\nPlease run 05_romania_city_lifetables.R first."))
}

message("Loading life table results...")
dt <- fread(input_file)

# 2. Extract Life Expectancy at Birth (age 0)
# ----------------
# We can also extract e65 or others if needed, but e0 is standard.
e0_dt <- dt[age == 0]

# 3. Calculate Differences
# ----------------
# ex_cc is the Life Expectancy in the Climate Change scenario
# ex_no_cc is the Life Expectancy in the Baseline
# Impact = ex_cc - ex_no_cc (Negative value implies loss of life)

e0_dt[, ":="(
  e0_baseline = ex_no_cc,
  e0_climate = ex_cc,
  loss_years = ex_no_cc - ex_cc, # Positive value = Loss
  pct_change = (ex_cc - ex_no_cc) / ex_no_cc * 100
)]

# 4. Select and Order Columns
# ----------------
summary_cols <- c("URAU_CODE", "LABEL", "ssp", "adapt", "period", 
                  "e0_baseline", "e0_climate", "loss_years", "pct_change")

summary_dt <- e0_dt[, ..summary_cols]
setorder(summary_dt, URAU_CODE, ssp, adapt, period)

# 5. Save Summary
# ----------------
output_file <- "results_csv/city_life_expectancy_summary.csv"
fwrite(summary_dt, output_file)

message("Summary generated successfully.")
message(sprintf("Saved %d rows to %s", nrow(summary_dt), output_file))

# 6. Preview
# ----------------
print(head(summary_dt))
