################################################################################
# 
# Temperature Distribution: Historical vs 2050 for Bucharest
# With Relative Risk curve overlay for 65-74 age group
# Style: IE Blue Template (Montserrat font, OceanBlue color scheme)
#
################################################################################

# Load required packages
library(arrow)
library(data.table)
library(ggplot2)
library(dplyr)
library(dlnm)        # For onebasis function to compute RR
library(showtext)    # For custom fonts

#------------------------------------------------------------------------------
# IE Blue Style Setup
#------------------------------------------------------------------------------

# Register Montserrat font (consistent with IE LaTeX template)
font_add("Montserrat", 
         regular = "fonts/Montserrat-Regular.ttf",
         bold = "fonts/Montserrat-Bold.ttf",
         italic = "fonts/Montserrat-Italic.ttf",
         bolditalic = "fonts/Montserrat-BoldItalic.ttf")
showtext_auto()

# IE Color palette
OceanBlue <- "#000066"
ElectricBlue <- "#0000db"
SeaBlue <- "#47bfff"
TechGreen <- "#6DC201"
TechBlack <- "#000024"

# Custom IE theme
theme_ie <- function(base_size = 12) {
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

ssp_scenario <- 3      # SSP3 (RCP 7.0 equivalent) for future projections
target_age <- "65-74"  # Age group for RR curve

# Define periods
hist_range <- histrange
future_range <- c(2050, 2059)  # 2050s decade

# Percentiles for smooth distribution
smooth_perc <- seq(0, 100, by = 2)  # 51 percentiles

cat("=== Reading Raw GCM Data ===\n")
cat(sprintf("City: %s (%s)\n", city_code, city_name))
cat(sprintf("Scenario: SSP%d\n", ssp_scenario))

#------------------------------------------------------------------------------
# Read raw daily temperature projections
#------------------------------------------------------------------------------

cat("\nLoading raw temperature data...\n")

rcp85_paths <- c(
  "data/tmeanproj_rcp85.gz.parquet",
  "cordex_data/processed/tmeanproj_rcp85.gz.parquet"
)
rcp85_path <- rcp85_paths[file.exists(rcp85_paths)][1]

raw_data_base <- open_dataset("data/tmeanproj.gz.parquet") |>
  filter(URAU_CODE == city_code) |>
  collect() |>
  as.data.table()

if (is.na(rcp85_path) || rcp85_path == "") {
  raw_data <- raw_data_base
} else {
  cat(sprintf("Found RCP8.5 parquet: %s\n", rcp85_path))
  raw_data_rcp85 <- open_dataset(rcp85_path) |>
    filter(URAU_CODE == city_code) |>
    collect() |>
    as.data.table()
  raw_data <- rbindlist(list(raw_data_base, raw_data_rcp85), fill = TRUE)
}

raw_data[, ssp := as.character(ssp)]

raw_data <- raw_data[ssp %in% c("hist", as.character(ssp_scenario))]

cat(sprintf("Loaded %s daily observations\n", format(nrow(raw_data), big.mark = ",")))

gcm_cols <- grep("^tas_", names(raw_data), value = TRUE)

#------------------------------------------------------------------------------
# Process temperature data
#------------------------------------------------------------------------------

cat("Processing temperature data...\n")

raw_data[, year := year(date)]

temp_long <- melt(raw_data, 
                  id.vars = c("date", "year", "ssp", "URAU_CODE"),
                  measure.vars = gcm_cols,
                  variable.name = "gcm",
                  value.name = "temperature")
temp_long[, gcm := gsub("tas_", "", gcm)]

ssp_min_years <- temp_long[ssp != "hist", .(min_year = min(year, na.rm = TRUE)), by = ssp]
if (nrow(ssp_min_years) > 0) {
  common_start_year <- max(ssp_min_years$min_year, na.rm = TRUE)
  temp_long <- temp_long[ssp == "hist" | year >= common_start_year]
  cat(sprintf("Aligned scenario start year: %d\n", common_start_year))
}

gcm_exclude <- c("CMCC_CM2_SR5", "TaiESM1")
temp_long <- temp_long[!gcm %in% gcm_exclude]

temp_long[, period := fifelse(
  year >= hist_range[1] & year <= hist_range[2], "Historical",
  fifelse(year >= future_range[1] & year <= future_range[2], "2050s", NA_character_)
)]

temp_long <- temp_long[!is.na(period)]

#------------------------------------------------------------------------------
# Load ERA5 observations
#------------------------------------------------------------------------------

cat("Loading ERA5 observations...\n")

era5 <- open_dataset("data/era5series.gz.parquet") |>
  filter(URAU_CODE == city_code) |>
  collect() |>
  as.data.table()

era5[, year := year(date)]
era5_hist <- era5[year >= hist_range[1] & year <= hist_range[2]]

#------------------------------------------------------------------------------
# Compute temperature percentiles
#------------------------------------------------------------------------------

cat("Computing percentiles...\n")

compute_percentiles <- function(dt, perc_vec) {
  dt[, {
    temps <- temperature[!is.na(temperature)]
    .(perc = perc_vec,
      temp = quantile(temps, perc_vec / 100, na.rm = TRUE))
  }, by = .(period, gcm)]
}

perc_data <- compute_percentiles(temp_long, smooth_perc)

era5_perc <- data.table(
  period = "ERA5 Observed",
  gcm = "ERA5",
  perc = smooth_perc,
  temp = quantile(era5_hist$era5landtmean, smooth_perc / 100, na.rm = TRUE)
)

#------------------------------------------------------------------------------
# Compute Relative Risk curve for 65-74 age group
#------------------------------------------------------------------------------

cat(sprintf("Computing RR curve for age group %s...\n", target_age))

# Load coefficients
coefs_all <- fread("data/coefs.csv")
coefs_city <- coefs_all[URAU_CODE == city_code & agegroup == target_age]

# Get temperature distribution for knots
predper_full <- c(seq(0, 1, 0.1), 2:98, seq(99, 100, 0.1))
tdist <- era5_hist[, .(perc = predper_full, 
                       tmean = quantile(era5landtmean, predper_full / 100, na.rm = TRUE))]

varknots <- tdist[perc %in% varper, tmean]
varbound <- range(tdist$tmean)

# Create temperature sequence for RR calculation
n_temp <- 200
temp_seq <- seq(min(tdist$tmean), max(tdist$tmean), length.out = n_temp)

# Build basis and compute RR
argvar <- list(fun = varfun, degree = vardegree, knots = varknots, Bound = varbound)
basis <- do.call(onebasis, c(list(x = temp_seq), argvar))

coefs <- as.numeric(coefs_city[, .(b1, b2, b3, b4, b5)])
log_rr <- basis %*% coefs

# Find MMT (minimum mortality temperature) - search within observed range
temp_in_range <- temp_seq >= varbound[1] & temp_seq <= varbound[2]
ind <- temp_in_range & temp_seq >= quantile(temp_seq[temp_in_range], 0.25) & 
       temp_seq <= quantile(temp_seq[temp_in_range], 0.99)
mmt <- temp_seq[ind][which.min(log_rr[ind])]

# Center RR at MMT
cenvec <- do.call(onebasis, c(list(x = mmt), argvar))
log_rr_centered <- log_rr - drop(cenvec %*% coefs)
rr <- pmax(exp(log_rr_centered), 1)

rr_curve <- data.table(temperature = temp_seq, RR = as.vector(rr))

cat(sprintf("MMT for %s: %.1f°C\n", target_age, mmt))

#------------------------------------------------------------------------------
# Prepare data for combined plot
#------------------------------------------------------------------------------

# Average percentiles across GCMs
avg_perc <- perc_data[, .(
  temp_mean = mean(temp),
  temp_min = min(temp),
  temp_max = max(temp)
), by = .(period, perc)]

# Calculate period means
period_means <- perc_data[, .(mean_temp = mean(temp)), by = period]
temp_shift <- period_means[period == "2050s", mean_temp] - 
              period_means[period == "Historical", mean_temp]

#------------------------------------------------------------------------------
# Create combined plot: Temperature distribution + RR curve
#------------------------------------------------------------------------------

cat("\nCreating combined visualization...\n")

# IE color scheme for periods - Historical first in legend
period_colors <- c(
  "Historical" = SeaBlue,
  "2050s" = ElectricBlue
)

# Set factor levels for legend order
# Combine data for histogram
hist_data <- perc_data[, .(period, temperature = temp)]
# Set factor levels for legend order (Historical first)
hist_data[, period := factor(period, levels = c("Historical", "2050s"))]

# Calculate histogram to get frequency counts
n_bins <- 40
temp_breaks <- seq(-30, 45, length.out = n_bins + 1)

# Get max count for y-axis scaling
hist_hist <- hist(hist_data[period == "Historical", temperature], breaks = temp_breaks, plot = FALSE)
hist_2050 <- hist(hist_data[period == "2050s", temperature], breaks = temp_breaks, plot = FALSE)
max_count <- max(c(hist_hist$counts, hist_2050$counts))
count_range <- c(0, max_count * 1.15)  # Add 15% headroom for labels

# Scale RR for secondary axis (map RR range to count range)
rr_min <- 1
rr_max <- max(rr_curve$RR)

# Transform function: RR -> count scale
rr_to_count <- function(rr) {
  (rr - rr_min) / (rr_max - rr_min) * diff(count_range) * 0.95 + count_range[1]
}
# Inverse for axis labels
count_to_rr <- function(c) {
  (c - count_range[1]) / (diff(count_range) * 0.95) * (rr_max - rr_min) + rr_min
}

rr_curve[, RR_scaled := rr_to_count(RR)]

# Prepare data for RR shading (excess risk areas)
# Cold risk: temperatures below MMT
rr_cold <- rr_curve[temperature <= mmt]
# Heat risk: temperatures above MMT  
rr_heat <- rr_curve[temperature >= mmt]

# Extend shading beyond RR curve range to plot edges
# Cold extension: from left edge (-25) to start of RR curve
cold_extension <- data.table(
  temperature = c(-25, min(rr_curve$temperature)),
  RR_scaled = rr_to_count(rr_curve[temperature == min(temperature), RR])
)
# Heat extension: from end of RR curve to right edge (40)
heat_extension <- data.table(
  temperature = c(max(rr_curve$temperature), 40),
  RR_scaled = rr_to_count(rr_curve[temperature == max(temperature), RR])
)

# Baseline RR=1 on the scaled axis
rr_baseline_scaled <- rr_to_count(1)

# Prepare labels for mean temperatures
period_means[, label := sprintf("Mean %.1f°C", mean_temp)]

# Create the combined plot
p_combined <- ggplot() +
  # Extended cold risk shading (beyond RR curve to left edge, capped at last RR value)
  geom_rect(aes(xmin = -25, xmax = min(rr_curve$temperature), 
                ymin = rr_baseline_scaled, 
                ymax = rr_to_count(rr_curve[temperature == min(temperature), RR])),
            fill = SeaBlue, alpha = 0.15) +
  # Extended heat risk shading (beyond RR curve to right edge)
  geom_rect(aes(xmin = max(rr_curve$temperature), xmax = 40,
                ymin = rr_baseline_scaled, ymax = count_range[2]),
            fill = "#D32F2F", alpha = 0.15) +
  # Shaded area for COLD excess risk (below MMT)
  geom_ribbon(data = rr_cold,
              aes(x = temperature, ymin = rr_baseline_scaled, ymax = RR_scaled),
              fill = SeaBlue, alpha = 0.15) +
  # Shaded area for HEAT excess risk (above MMT)
  geom_ribbon(data = rr_heat,
              aes(x = temperature, ymin = rr_baseline_scaled, ymax = RR_scaled),
              fill = "#D32F2F", alpha = 0.15) +
  # Temperature histograms (frequency counts) - with borders
  geom_histogram(data = hist_data[period == "Historical"],
                 aes(x = temperature, fill = "Historical"),
                 alpha = 0.4, breaks = temp_breaks, color = SeaBlue, linewidth = 0.3) +
  geom_histogram(data = hist_data[period == "2050s"],
                 aes(x = temperature, fill = "2050s"),
                 alpha = 0.4, breaks = temp_breaks, color = ElectricBlue, linewidth = 0.3) +
  # RR baseline (RR=1)
  geom_hline(yintercept = rr_baseline_scaled, color = "gray50", linetype = "solid", linewidth = 0.5) +
  # RR curve (on secondary axis scale)
  geom_line(data = rr_curve,
            aes(x = temperature, y = RR_scaled),
            color = OceanBlue, linewidth = 1.4) +
  # Risk zone labels
  annotate("text", x = -15, y = max_count * 0.4, label = "Cold\nrisk",
           color = SeaBlue, size = 4.5, fontface = "italic", family = "Montserrat", alpha = 0.8) +
  annotate("text", x = 32, y = max_count * 0.4, label = "Heat\nrisk",
           color = "#D32F2F", size = 4.5, fontface = "italic", family = "Montserrat", alpha = 0.8) +
  # Mean lines with labels
  geom_vline(data = period_means,
             aes(xintercept = mean_temp, color = period),
             linetype = "dashed", linewidth = 0.8) +
  # Mean temperature labels (vertical text) - offset from line, ending at top
  geom_text(data = period_means,
            aes(x = mean_temp + 0.8, y = count_range[2], label = label, color = period),
            angle = 90, hjust = 1, vjust = 0.5, size = 4, fontface = "bold",
            family = "Montserrat", show.legend = FALSE) +
  # MMT indicator with label (dotted line, same color as RR curve)
  geom_vline(xintercept = mmt, color = OceanBlue, linetype = "dotted", linewidth = 1) +
  geom_text(aes(x = mmt + 0.8, y = count_range[2], label = sprintf("MMT %.1f°C", mmt)),
            angle = 90, hjust = 1, vjust = 0.5, size = 4, fontface = "bold",
            color = OceanBlue, family = "Montserrat") +
  # Scales - explicitly set breaks order for legend (Historical first)
  scale_fill_manual(values = period_colors, name = "Period",
                    breaks = c("Historical", "2050s")) +
  scale_color_manual(values = period_colors, name = "Period",
                     breaks = c("Historical", "2050s")) +
  scale_y_continuous(
    name = "Number of days",
    sec.axis = sec_axis(
      transform = ~ count_to_rr(.),
      name = "Relative Risk (65-74 yrs)",
      breaks = seq(1, ceiling(rr_max * 10) / 10, 0.2)
    )
  ) +
  scale_x_continuous(name = "Temperature (°C)") +
  coord_cartesian(xlim = c(-25, 40), ylim = count_range) +
  theme_ie(base_size = 14) +
  theme(
    axis.title.y.right = element_text(color = OceanBlue),
    axis.text.y.right = element_text(color = OceanBlue)
  )

# Save
ggsave(file.path(img_dir, paste0(city_name_lower, "_temp_dist_rr.png")), p_combined,
       width = 10, height = 6, dpi = 300, bg = "white")
cat(sprintf("Saved: %s/%s_temp_dist_rr.png\n", img_dir, city_name_lower))

# Also save as PDF for LaTeX
ggsave(file.path(img_dir, paste0(city_name_lower, "_temp_dist_rr.pdf")), p_combined,
       width = 10, height = 6, device = cairo_pdf)
cat(sprintf("Saved: %s/%s_temp_dist_rr.pdf\n", img_dir, city_name_lower))

#------------------------------------------------------------------------------
# Summary statistics
#------------------------------------------------------------------------------

cat("\n=== Summary Statistics ===\n\n")

cat("Temperature distribution:\n")
cat(sprintf("  Historical mean: %.2f°C\n", period_means[period == "Historical", mean_temp]))
cat(sprintf("  2050s mean: %.2f°C\n", period_means[period == "2050s", mean_temp]))
cat(sprintf("  Shift: +%.2f°C\n", temp_shift))

cat(sprintf("\nRelative Risk (%s):\n", target_age))
cat(sprintf("  MMT: %.1f°C\n", mmt))
cat(sprintf("  Max RR: %.3f\n", max(rr_curve$RR)))

print(p_combined)
