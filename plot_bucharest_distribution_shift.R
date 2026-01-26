################################################################################
# 
# Temperature Distribution Shift for Bucharest (RO001C)
# Comparing baseline (historical) vs last projected year under SSP3
#
################################################################################

# Load required packages
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)

# Set city code for Bucharest
city_code <- "RO001C"
ssp_scenario <- 3

#------------------------------------------------------------------------------
# Read and process temperature data
#------------------------------------------------------------------------------

cat("Reading temperature data for Bucharest...\n")

temp_data <- fread("results_csv/tsum0.csv") %>%
  filter(city == city_code, ssp == ssp_scenario)

cat(sprintf("Found %s rows for Bucharest SSP%d\n", 
            format(nrow(temp_data), big.mark = ","), ssp_scenario))

# Convert to data.table
temp_data <- as.data.table(temp_data)

# Get unique periods
periods <- unique(temp_data$calperiod)
cat("Available periods:", paste(periods, collapse = ", "), "\n")

# Select historical and last period
hist_period <- "hist"
last_period <- periods[periods != "hist"][length(periods[periods != "hist"])]

cat(sprintf("\nComparing:\n  Baseline: %s\n  Future: %s\n\n", hist_period, last_period))

# Filter for these two periods
comparison_data <- temp_data[calperiod %in% c(hist_period, last_period)]

# Create readable labels
comparison_data[, period_label := ifelse(
  calperiod == "hist",
  "Historical (2000-2014)",
  paste0("Future (", sub("^(\\d{4})-.*", "\\1s", calperiod), ")")
)]

#------------------------------------------------------------------------------
# Create distribution comparison visualizations
#------------------------------------------------------------------------------

cat("Creating distribution comparison charts...\n")

# Define colors
hist_color <- "#2196F3"  # Blue for historical
future_color <- "#F44336"  # Red for future

#--- Chart 1: Overlapping density plots across all percentiles ---

# Prepare data for density plot - use actual temperature values from all percentiles
# Each row represents a temperature at a specific percentile for a specific model
density_data <- comparison_data[, .(
  temperature = full,
  period = period_label
)]

# Get unique period labels for color mapping
period_labels <- unique(comparison_data$period_label)
period_colors <- setNames(c(hist_color, future_color), period_labels)

# Calculate means for each period
period_means <- density_data[, .(mean_temp = mean(temperature)), by = period]
period_means[, label := sprintf("Mean: %.1f°C", mean_temp)]
# Add horizontal offset for labels
period_means[, x_offset := c(-4.5, 4.5)]

# Calculate bin width and total counts for secondary axis
bin_width <- diff(range(density_data$temperature)) / 50
n_total <- nrow(density_data) / 2  # divided by 2 periods

p1 <- ggplot(density_data, aes(x = temperature, fill = period, color = period)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.4, position = "identity", bins = 50) +
  geom_density(alpha = 0, linewidth = 1.2) +  # Just the line, no fill
  geom_vline(data = period_means, aes(xintercept = mean_temp, color = period),
             linetype = "dashed", linewidth = 1) +
  geom_text(data = period_means, aes(x = mean_temp + x_offset, label = label, color = period),
            y = Inf, vjust = 1.5, hjust = 0.5, size = 4, fontface = "bold", show.legend = FALSE) +
  scale_y_continuous(
    name = "Density",
    sec.axis = sec_axis(~ . * n_total * bin_width, name = "Frequency (Count)")
  ) +
  scale_fill_manual(values = period_colors) +
  scale_color_manual(values = period_colors) +
  labs(
    title = "Temperature Distribution Shift for Bucharest (SSP3-7.0)",
    subtitle = sprintf("Comparing historical baseline to %s projections (dashed lines show means)", 
                      sub("^(\\d{4})-.*", "\\1s", last_period)),
    x = "Temperature (°C)",
    y = "Density",
    fill = "Period",
    color = "Period",
    caption = sprintf("Distribution of all percentile values across %d climate models\nMean shift: %.2f°C → %.2f°C (+%.2f°C)", 
                     uniqueN(comparison_data$gcm),
                     period_means[1, mean_temp],
                     period_means[2, mean_temp],
                     diff(period_means$mean_temp))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("figures/bucharest_distribution_shift_density.png", p1,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Saved: figures/bucharest_distribution_shift_density.png\n")

#--- Chart 2: Side-by-side box plots ---

# Calculate summary statistics across models for key percentiles
key_percentiles <- c(1, 25, 50, 75, 99)
box_data <- comparison_data[perc %in% key_percentiles, .(
  temp = full,
  period = period_label,
  percentile = factor(perc, levels = key_percentiles, 
                     labels = paste0(key_percentiles, "th"))
)]

p2 <- ggplot(box_data, aes(x = percentile, y = temp, fill = period)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = period_colors) +
  labs(
    title = "Temperature Distribution Shift by Percentile",
    subtitle = "Box plots show variation across climate models",
    x = "Temperature Percentile",
    y = "Temperature (°C)",
    fill = "Period",
    caption = "Each box shows the spread across climate models for a given percentile"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("figures/bucharest_distribution_shift_boxplot.png", p2,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Saved: figures/bucharest_distribution_shift_boxplot.png\n")

#--- Chart 3: Shift visualization (change in each percentile) ---

# Calculate mean shift for each percentile
shift_data <- comparison_data[, .(
  temp_mean = mean(full, na.rm = TRUE)
), by = .(calperiod, perc)]

shift_wide <- shift_data %>%
  pivot_wider(names_from = calperiod, values_from = temp_mean)

# Calculate shift and add labels - only label key percentiles
shift_wide$shift <- shift_wide[[last_period]] - shift_wide$hist
shift_wide$perc_numeric <- as.numeric(shift_wide$perc)

p3 <- ggplot(shift_wide, aes(x = perc_numeric, y = shift)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(color = future_color, linewidth = 1.2) +
  geom_point(color = future_color, size = 2, alpha = 0.6) +
  geom_text(data = shift_wide[shift_wide$perc_numeric %in% c(0, 25, 50, 75, 100), ],
            aes(label = sprintf("+%.1f°C", shift)),
            vjust = -1.2, size = 3.5, fontface = "bold") +
  scale_x_continuous(breaks = c(0, 10, 25, 50, 75, 90, 100),
                     labels = c("0th", "10th", "25th", "50th", "75th", "90th", "100th")) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Temperature Shift Across Distribution (SSP3-7.0)",
    subtitle = sprintf("Change from historical (2000-2014) to %s", 
                      sub("^(\\d{4})-.*", "\\1s", last_period)),
    x = "Temperature Percentile",
    y = "Temperature Change (°C)",
    caption = "Positive values indicate warming. Each point shows the shift for that percentile."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
  )

ggsave("figures/bucharest_distribution_shift_change.png", p3,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Saved: figures/bucharest_distribution_shift_change.png\n")

#--- Chart 4: Combined before/after showing full percentile curves ---

# Calculate mean temperature for each percentile in both periods
percentile_curves <- comparison_data[, .(
  temp_mean = mean(full, na.rm = TRUE),
  temp_sd = sd(full, na.rm = TRUE)
), by = .(period_label, perc)]

# Convert perc to numeric for plotting
percentile_curves[, perc_numeric := as.numeric(perc)]

p4 <- ggplot(percentile_curves, aes(x = perc_numeric, y = temp_mean, color = period_label)) +
  geom_ribbon(aes(ymin = temp_mean - temp_sd, ymax = temp_mean + temp_sd, 
                  fill = period_label), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.5) +
  # Add value labels at key percentiles
  geom_text(data = percentile_curves[perc_numeric %in% c(0, 50, 100)],
            aes(label = sprintf("%.1f°C", temp_mean)),
            vjust = -1, size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_x_continuous(breaks = c(0, 10, 25, 50, 75, 90, 100),
                     labels = c("0th", "10th", "25th", "50th", "75th", "90th", "100th")) +
  scale_color_manual(values = period_colors) +
  scale_fill_manual(values = period_colors) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Complete Temperature Distribution: Historical vs Future",
    subtitle = "Full percentile curves showing the entire temperature range",
    x = "Percentile",
    y = "Temperature (°C)",
    color = "Period",
    fill = "Period",
    caption = "Lines show mean across models; shaded areas show ±1 SD"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.3),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
  )

ggsave("figures/bucharest_distribution_shift_curves.png", p4,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Saved: figures/bucharest_distribution_shift_curves.png\n")

# Display plots
print(p1)
print(p2)
print(p3)
print(p4)

#------------------------------------------------------------------------------
# Print summary statistics
#------------------------------------------------------------------------------

cat("\n=== Distribution Shift Summary ===\n\n")

summary_stats <- comparison_data[, .(
  Mean = mean(full, na.rm = TRUE),
  Median = median(full, na.rm = TRUE),
  SD = sd(full, na.rm = TRUE),
  Min = min(full, na.rm = TRUE),
  Max = max(full, na.rm = TRUE),
  Q25 = quantile(full, 0.25, na.rm = TRUE),
  Q75 = quantile(full, 0.75, na.rm = TRUE)
), by = period_label]

print(summary_stats, digits = 2)

cat("\n=== Temperature Changes ===\n\n")

hist_stats <- summary_stats[period_label == "Historical (2000-2014)"]
future_stats <- summary_stats[period_label != "Historical (2000-2014)"]

cat(sprintf("Mean temperature shift: +%.2f°C (from %.1f°C to %.1f°C)\n", 
            future_stats$Mean - hist_stats$Mean, hist_stats$Mean, future_stats$Mean))
cat(sprintf("Minimum temperature shift: +%.2f°C (from %.1f°C to %.1f°C)\n", 
            future_stats$Min - hist_stats$Min, hist_stats$Min, future_stats$Min))
cat(sprintf("Maximum temperature shift: +%.2f°C (from %.1f°C to %.1f°C)\n", 
            future_stats$Max - hist_stats$Max, hist_stats$Max, future_stats$Max))
cat(sprintf("Median shift: +%.2f°C\n", future_stats$Median - hist_stats$Median))
cat(sprintf("\nChange in variability (SD): %.2f°C\n", future_stats$SD - hist_stats$SD))

cat("\n=== Extreme Temperature Changes ===\n\n")

# Get extreme percentiles
extreme_shift <- as.data.frame(shift_wide)[shift_wide$perc %in% c(1, 5, 95, 99), ]
for (i in 1:nrow(extreme_shift)) {
  cat(sprintf("%sth percentile: +%.2f°C\n", 
              extreme_shift$perc[i], extreme_shift$shift[i]))
}

cat("\nAll charts saved to figures/ directory\n")
