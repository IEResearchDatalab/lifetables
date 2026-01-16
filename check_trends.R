library(data.table)

# Load EU level period summary
dt <- fread("results_csv/eu_period.csv")

# Filter for SSP1 (usually SSP1-2.6) and maybe SSP3 for comparison
# Check available SSPs
print(unique(dt$ssp))

# Aggregate by period and SSP
# We need to average over GCMs first? Or is the table already aggregated?
# The table has column 'gcm'. We should probably sum over agegroups and then mean over GCMs.

# Filter for agegroup "all" to check totals
dt_all <- dt[agegroup == "all"]

# Calculate Implied Population per row (guard against divide by zero)
# Rate is per 100,000
dt_all[, implied_pop := ifelse(abs(rate_est) > 0.001, an_est / (rate_est / 100000), NA)]

# Aggregate by SSP/Period
res <- dt_all[, .(
  total_deaths = sum(an_est, na.rm=TRUE),
  # Pop is summed over cities? No, `eu_period` is already EU aggregate.
  # But we have multiple GCMs. We should take the MEAN over GCMs.
  avg_pop = mean(implied_pop, na.rm=TRUE),
  avg_rate = mean(rate_est, na.rm=TRUE),
  avg_deaths = mean(an_est, na.rm=TRUE) # Mean over GCMs, not Sum
), by = .(ssp, period, adapt)]

# Look at SSP1, Adapt 0% (to remove adaptation noise)
print("SSP1 Trends (Adapt 0%):")
print(res[ssp == 1 & adapt == "0%"][order(period)])

print("SSP3 Trends (Adapt 0%):")
print(res[ssp == 3 & adapt == "0%"][order(period)])



# Print SSP1 trends
print("SSP1 Trend:")
print(res[ssp == 1][order(period)])

print("SSP3 Trend:")
print(res[ssp == 3][order(period)])
