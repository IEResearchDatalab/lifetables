#------------------------------------------------------------------------------
# Temperature Evolution for Bucharest
# 365-day moving average with confidence intervals by SSP scenario
#------------------------------------------------------------------------------

library(data.table)
library(arrow)
library(dplyr)
library(ggplot2)
library(showtext)

#------------------------------------------------------------------------------
# IE Style Setup (matching main plot)
#------------------------------------------------------------------------------

# IE color palette
OceanBlue <- "#000066"
ElectricBlue <- "#0000db"
SeaBlue <- "#47bfff"
TechGreen <- "#6DC201"
TechBlack <- "#000024"

# Load Montserrat font
font_add("Montserrat", 
         regular = "fonts/Montserrat-Regular.ttf",
         bold = "fonts/Montserrat-Bold.ttf",
         italic = "fonts/Montserrat-Italic.ttf")
showtext_auto()

# IE theme
theme_ie <- function(base_size = 18) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = "Montserrat", color = OceanBlue),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      axis.title = element_text(face = "bold", color = OceanBlue),
      axis.text = element_text(color = OceanBlue),
      legend.title = element_text(face = "bold", color = OceanBlue),
      legend.text = element_text(color = OceanBlue),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray85")
    )
}

#------------------------------------------------------------------------------
# Load city configuration
#------------------------------------------------------------------------------
source("config.R")

window_size <- 365     # Rolling average window (days)

# SSP scenario colors
ssp_colors <- c(
  "RCP2.6" = TechGreen,
  "RCP4.5" = SeaBlue,
  "RCP7.0" = "#D32F2F",
  "RCP8.5" = "#F57C00"
)

ssp_labels <- c(
  "1" = "RCP2.6",
  "2" = "RCP4.5",
  "3" = "RCP7.0",
  "5" = "RCP8.5"
)

cat("=== Temperature Evolution Analysis ===\n")
cat(sprintf("City: %s (%s)\n", city_code, city_name))
cat(sprintf("Rolling window: %d days\n", window_size))

#------------------------------------------------------------------------------
# Load raw temperature projections
#------------------------------------------------------------------------------

cat("\nLoading raw temperature data...\n")

rcp85_paths <- c(
  "data/tmeanproj_rcp85.gz.parquet",
  "cordex_data/processed/tmeanproj_rcp85.gz.parquet"
)
rcp85_path <- rcp85_paths[file.exists(rcp85_paths)][1]

# Read parquet file - GCMs are in separate columns
temp_raw_base <- open_dataset("data/tmeanproj.gz.parquet") |>
  filter(URAU_CODE == city_code) |>
  collect() |>
  as.data.table()

if (is.na(rcp85_path) || rcp85_path == "") {
  temp_raw <- temp_raw_base
} else {
  cat(sprintf("Found RCP8.5 parquet: %s\n", rcp85_path))
  temp_raw_rcp85 <- open_dataset(rcp85_path) |>
    filter(URAU_CODE == city_code) |>
    collect() |>
    as.data.table()
  temp_raw <- rbindlist(list(temp_raw_base, temp_raw_rcp85), fill = TRUE)
}

temp_raw[, ssp := as.character(ssp)]

cat(sprintf("Loaded %s observations\n", format(nrow(temp_raw), big.mark = ",")))

# Get GCM column names (all columns starting with "tas_")
gcm_cols <- grep("^tas_", names(temp_raw), value = TRUE)
cat(sprintf("Found %d GCM models\n", length(gcm_cols)))

# Melt to long format for easier processing
temp_long <- melt(temp_raw, 
                  id.vars = c("URAU_CODE", "date", "ssp"),
                  measure.vars = gcm_cols,
                  variable.name = "gcm",
                  value.name = "tmean")

# Extract year from date
temp_long[, year := year(date)]

ssp_min_years <- temp_long[ssp != "hist", .(min_year = min(year, na.rm = TRUE)), by = ssp]
if (nrow(ssp_min_years) > 0) {
  common_start_year <- max(ssp_min_years$min_year, na.rm = TRUE)
  temp_long <- temp_long[ssp == "hist" | year >= common_start_year]
  cat(sprintf("Aligned scenario start year: %d\n", common_start_year))
}

#------------------------------------------------------------------------------
# Load ERA5 historical observations
#------------------------------------------------------------------------------

cat("Loading ERA5 observations...\n")

era5 <- open_dataset("data/era5series.gz.parquet") |>
  filter(URAU_CODE == city_code) |>
  select(date, era5landtmean) |>
  collect() |>
  as.data.table()

era5[, tmean := era5landtmean]

cat(sprintf("Loaded %s ERA5 observations\n", format(nrow(era5), big.mark = ",")))

#------------------------------------------------------------------------------
# Calculate rolling average for ERA5 (historical)
#------------------------------------------------------------------------------

cat("\nComputing rolling averages...\n")

# Sort by date
setkey(era5, date)

# Calculate rolling mean for ERA5
era5[, roll_mean := frollmean(tmean, n = window_size, align = "center", na.rm = TRUE)]

# Prepare ERA5 data for plotting (also 365-day moving average)
era5_plot <- era5[!is.na(roll_mean), .(
  date = date,
  mean = roll_mean,
  scenario = "ERA5 (Historical)"
)]

#------------------------------------------------------------------------------
# Calculate rolling average by SSP scenario (across GCM ensemble)
#------------------------------------------------------------------------------

# For each SSP, calculate daily mean and confidence intervals across GCMs
# Then apply rolling average

ssp_results <- list()

ssp_ids <- intersect(names(ssp_labels), unique(temp_long$ssp))

for (ssp_id in ssp_ids) {
  cat(sprintf("  Processing SSP%s...\n", ssp_id))
  
  ssp_data <- temp_long[ssp == ssp_id]
  
  # Calculate daily statistics across GCMs
  daily_stats <- ssp_data[, .(
    mean_temp = mean(tmean, na.rm = TRUE),
    sd_temp = sd(tmean, na.rm = TRUE),
    q05 = quantile(tmean, 0.05, na.rm = TRUE),
    q25 = quantile(tmean, 0.25, na.rm = TRUE),
    q75 = quantile(tmean, 0.75, na.rm = TRUE),
    q95 = quantile(tmean, 0.95, na.rm = TRUE),
    n_gcm = .N
  ), by = .(date)]
  
  setkey(daily_stats, date)
  
  # Apply rolling average to mean and quantiles
  daily_stats[, `:=`(
    roll_mean = frollmean(mean_temp, n = window_size, align = "center", na.rm = TRUE),
    roll_q05 = frollmean(q05, n = window_size, align = "center", na.rm = TRUE),
    roll_q25 = frollmean(q25, n = window_size, align = "center", na.rm = TRUE),
    roll_q75 = frollmean(q75, n = window_size, align = "center", na.rm = TRUE),
    roll_q95 = frollmean(q95, n = window_size, align = "center", na.rm = TRUE)
  )]
  
  ssp_results[[ssp_labels[ssp_id]]] <- daily_stats[!is.na(roll_mean), .(
    date = date,
    mean = roll_mean,
    q05 = roll_q05,
    q25 = roll_q25,
    q75 = roll_q75,
    q95 = roll_q95,
    scenario = ssp_labels[ssp_id]
  )]
}

# Combine SSP results
ssp_combined <- rbindlist(ssp_results)

cat(sprintf("  Total projection data points: %s\n", format(nrow(ssp_combined), big.mark = ",")))

#------------------------------------------------------------------------------
# Create plot
#------------------------------------------------------------------------------

cat("\nCreating visualization...\n")

# Prepare data for plotting
# ERA5 historical
era5_for_plot <- era5_plot[, .(date, mean, scenario)]

# Combined plot data
plot_data <- rbind(
  era5_for_plot[, .(date, mean, scenario)],
  ssp_combined[, .(date, mean, scenario)]
)

# Set scenario factor levels for legend order
available_labels <- unname(ssp_labels[ssp_ids])
plot_data[, scenario := factor(scenario, levels = c("ERA5 (Historical)", available_labels))]
ssp_combined[, scenario := factor(scenario, levels = available_labels)]

# Color palette including historical
all_colors <- c(
  "ERA5 (Historical)" = OceanBlue,
  ssp_colors
)

# Create the plot
p_evolution <- ggplot() +
  # 25th percentile lines (lower bound of 50% CI)
  geom_line(data = ssp_combined,
            aes(x = date, y = q25, color = scenario),
            linewidth = 0.3, linetype = "dashed", alpha = 0.6) +
  # 75th percentile lines (upper bound of 50% CI)
  geom_line(data = ssp_combined,
            aes(x = date, y = q75, color = scenario),
            linewidth = 0.3, linetype = "dashed", alpha = 0.6) +
  # Mean lines (solid, thicker)
  geom_line(data = plot_data,
            aes(x = date, y = mean, color = scenario),
            linewidth = 1) +
  # Scales
  scale_color_manual(values = all_colors, name = "Scenario",
                     breaks = c("ERA5 (Historical)", available_labels)) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(name = "Temperature (°C)") +
  labs(x = "Year") +
  theme_ie(base_size = 18)

# Save
ggsave(file.path(img_dir, paste0(city_name_lower, "_temp_evolution.png")), p_evolution,
       width = 12, height = 6, dpi = 300, bg = "white")
cat(sprintf("Saved: %s/%s_temp_evolution.png\n", img_dir, city_name_lower))

ggsave(file.path(img_dir, paste0(city_name_lower, "_temp_evolution.pdf")), p_evolution,
       width = 12, height = 6, device = cairo_pdf)
cat(sprintf("Saved: %s/%s_temp_evolution.pdf\n", img_dir, city_name_lower))

#------------------------------------------------------------------------------
# Summary statistics
#------------------------------------------------------------------------------

cat("\n=== Summary Statistics ===\n\n")

# Historical mean (last 10 years of ERA5)
hist_mean <- era5_plot[year(date) >= 2010, mean(mean, na.rm = TRUE)]
cat(sprintf("Historical mean (2010-2019): %.2f°C\n", hist_mean))

# Future means by scenario (2050s)
cat("\n2050s decade means:\n")
for (scen in c("RCP2.6", "RCP4.5", "RCP7.0")) {
  future_mean <- ssp_combined[scenario == scen & year(date) >= 2050 & year(date) < 2060, 
                               mean(mean, na.rm = TRUE)]
  cat(sprintf("  %s: %.2f°C (Δ%.2f°C)\n", scen, future_mean, future_mean - hist_mean))
}

# End of century means
cat("\n2090s decade means:\n")
for (scen in c("RCP2.6", "RCP4.5", "RCP7.0")) {
  future_mean <- ssp_combined[scenario == scen & year(date) >= 2090 & year(date) < 2100, 
                               mean(mean, na.rm = TRUE)]
  cat(sprintf("  %s: %.2f°C (Δ%.2f°C)\n", scen, future_mean, future_mean - hist_mean))
}

print(p_evolution)
