################################################################################
# 
# Temperature Projections for Bucharest (RO001C)
# Chart showing average temperature trends up to the last year
#
################################################################################

# Load required packages
library(arrow)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)

# Set city code for Bucharest
city_code <- "RO001C"

#------------------------------------------------------------------------------
# Read temperature data
#------------------------------------------------------------------------------

cat("Reading temperature data for Bucharest...\n")

# Read from CSV (parquet file has compatibility issues)
if (file.exists("results_csv/tsum0.csv")) {
  temp_data <- fread("results_csv/tsum0.csv") %>%
    filter(city == city_code)
} else {
  stop("Temperature data file not found!")
}

cat(sprintf("Found %s rows for Bucharest\n", format(nrow(temp_data), big.mark = ",")))

#------------------------------------------------------------------------------
# Process data for visualization
#------------------------------------------------------------------------------

# Convert to data.table for efficient processing
temp_data <- as.data.table(temp_data)

# Extract year from calperiod
temp_data[, year := as.numeric(ifelse(
  calperiod == "hist",
  2007,  # Middle of historical period (2000-2014)
  substr(calperiod, 1, 4)
))]

# Calculate mean temperature across all GCMs for each scenario and period
# Using the "full" temperature (includes climate change)
temp_summary <- temp_data[, .(
  mean_temp = mean(full, na.rm = TRUE),
  min_temp = min(full, na.rm = TRUE),
  max_temp = max(full, na.rm = TRUE),
  sd_temp = sd(full, na.rm = TRUE),
  n_models = .N
), by = .(year, ssp, perc)]

# Get mean temperature (50th percentile) and extremes
mean_by_year <- temp_summary[perc == 50, .(
  temperature = mean_temp,
  temp_min = mean_temp - sd_temp,
  temp_max = mean_temp + sd_temp,
  ssp
), by = year]

# Get additional percentiles for context
percentile_data <- temp_summary[perc %in% c(1, 25, 50, 75, 99), .(
  year, ssp, perc, temperature = mean_temp
)]

#------------------------------------------------------------------------------
# Create visualization
#------------------------------------------------------------------------------

cat("Creating temperature chart...\n")

# Define SSP labels and colors
ssp_labels <- c("1" = "SSP1-2.6 (Low emissions)", 
                "2" = "SSP2-4.5 (Medium emissions)", 
                "3" = "SSP3-7.0 (High emissions)")
ssp_colors <- c("1" = "#2E7D32", "2" = "#F57C00", "3" = "#C62828")

# Main plot: Mean temperature over time by SSP scenario
p1 <- ggplot(mean_by_year, aes(x = year, y = temperature, color = factor(ssp))) +
  # Confidence band (mean ± 1 SD across models)
  geom_ribbon(aes(ymin = temp_min, ymax = temp_max, fill = factor(ssp)), 
              alpha = 0.2, color = NA) +
  # Mean line
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  # Value labels for last point in each SSP
  geom_text(data = mean_by_year %>% group_by(ssp) %>% filter(year == max(year)),
            aes(label = sprintf("%.1f°C", temperature)),
            vjust = -1, size = 3.5, fontface = "bold", show.legend = FALSE) +
  # Value label for initial point (only one SSP since they're all equal)
  geom_text(data = mean_by_year %>% filter(ssp == 1, year == min(year)),
            aes(label = sprintf("%.1f°C", temperature)),
            vjust = -1, size = 3.5, fontface = "bold", show.legend = FALSE) +
  # Styling
  scale_color_manual(values = ssp_colors, labels = ssp_labels, name = "Scenario") +
  scale_fill_manual(values = ssp_colors, labels = ssp_labels, name = "Scenario") +
  labs(
    title = "Median Temperature Projections for Bucharest",
    subtitle = sprintf("Mean of 50th percentile (median) temperatures across %d climate models", 
                      temp_summary[perc == 50 & ssp == 1, max(n_models)]),
    x = "Year",
    y = "Temperature (°C)",
    caption = "Note: Each line shows the mean of median temperatures across climate models. Shaded area shows ±1 standard deviation.\nHistorical period: 2000-2014"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Save plot
ggsave("figures/bucharest_temperature_mean.png", p1, 
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Saved: figures/bucharest_temperature_mean.png\n")

# Additional plot: Temperature distribution (percentiles) for SSP3
cat("Creating percentile chart for SSP3...\n")

percentile_wide <- percentile_data[ssp == 3] %>%
  pivot_wider(names_from = perc, values_from = temperature, names_prefix = "p")

p2 <- ggplot(percentile_wide, aes(x = year)) +
  # Extreme range (1st to 99th percentile)
  geom_ribbon(aes(ymin = p1, ymax = p99), fill = "#C62828", alpha = 0.15) +
  # Interquartile range (25th to 75th percentile)
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#C62828", alpha = 0.3) +
  # Median line
  geom_line(aes(y = p50), color = "#C62828", linewidth = 1.5) +
  geom_point(aes(y = p50), color = "#C62828", size = 2.5) +
  # Value labels
  geom_text(aes(y = p50, label = sprintf("%.1f°C", p50)), 
            vjust = -1.2, size = 3.5, fontface = "bold", color = "#C62828") +
  labs(
    title = "Temperature Distribution for Bucharest (SSP3-7.0)",
    subtitle = "Showing percentile ranges across climate models",
    x = "Year",
    y = "Temperature (°C)",
    caption = "Dark shaded area: 25th-75th percentile (interquartile range)\nLight shaded area: 1st-99th percentile (near full range)\nLine: Median (50th percentile)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.3)
  )

ggsave("figures/bucharest_temperature_percentiles.png", p2,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Saved: figures/bucharest_temperature_percentiles.png\n")

# Display plots
print(p1)
print(p2)

#------------------------------------------------------------------------------
# Print summary statistics
#------------------------------------------------------------------------------

cat("\n=== Temperature Summary for Bucharest ===\n\n")

summary_table <- mean_by_year %>%
  group_by(ssp) %>%
  summarise(
    `Historical (2000-2014)` = temperature[year == min(year)],
    `2030s` = mean(temperature[year >= 2030 & year < 2040], na.rm = TRUE),
    `2050s` = mean(temperature[year >= 2050 & year < 2060], na.rm = TRUE),
    `2070s` = mean(temperature[year >= 2070 & year < 2080], na.rm = TRUE),
    `2090s` = mean(temperature[year >= 2090], na.rm = TRUE),
    `Total Increase` = max(temperature) - min(temperature)
  )

summary_table <- summary_table %>%
  mutate(Scenario = ssp_labels[as.character(ssp)]) %>%
  select(Scenario, everything(), -ssp)

print(summary_table, digits = 2)

cat("\n=== Key Findings ===\n\n")
for (s in 1:3) {
  scenario_data <- mean_by_year[ssp == s]
  hist_temp <- scenario_data[year == min(year), temperature]
  last_temp <- scenario_data[year == max(year), temperature]
  increase <- last_temp - hist_temp
  
  cat(sprintf("%s:\n", ssp_labels[as.character(s)]))
  cat(sprintf("  Historical: %.1f°C\n", hist_temp))
  cat(sprintf("  Latest projection: %.1f°C\n", last_temp))
  cat(sprintf("  Total increase: +%.1f°C\n\n", increase))
}

cat("Charts saved to figures/ directory\n")
