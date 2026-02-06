################################################################################
#
# Mortality Multiplier by Age for Different Years
# 
# This script computes the mortality multiplier for single-year ages (20-100)
# interpolated from age-group RR curves, for years 2050, 2075, and 2099.
#
################################################################################

library(terra)
library(data.table)
library(arrow)
library(dplyr)
library(dlnm)
library(splines)
library(ggplot2)
library(showtext)

#------------------------------------------------------------------------------
# IE Blue Template Styling
#------------------------------------------------------------------------------

font_add("Montserrat", 
         regular = "fonts/Montserrat-Regular.ttf",
         bold = "fonts/Montserrat-Bold.ttf",
         italic = "fonts/Montserrat-Italic.ttf",
         bolditalic = "fonts/Montserrat-BoldItalic.ttf")
showtext_auto()

OceanBlue <- "#000066"
ElectricBlue <- "#0000db"
SeaBlue <- "#47bfff"
TechGreen <- "#6DC201"

theme_ie <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "Montserrat") +
    theme(
      text = element_text(family = "Montserrat", color = OceanBlue),
      plot.title = element_text(face = "bold", color = OceanBlue, size = base_size * 1.4),
      plot.subtitle = element_text(color = OceanBlue, size = base_size * 0.9),
      plot.caption = element_text(hjust = 0, size = base_size * 0.75, color = OceanBlue),
      axis.title = element_text(color = OceanBlue, face = "bold"),
      axis.text = element_text(color = OceanBlue),
      legend.title = element_text(color = OceanBlue, face = "bold"),
      legend.text = element_text(color = OceanBlue),
      panel.grid.major = element_line(color = "#e0e0e0"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

#------------------------------------------------------------------------------
# Load city configuration
#------------------------------------------------------------------------------
source("config.R")

# Target years for comparison
target_years <- c(2050, 2075, 2099)

# Single-year age range
age_range <- 20:100

# Use RCP 7.0 for this visualization (high scenario)
target_ssp <- "3"

#------------------------------------------------------------------------------
# Step 1: Load 2023 ERA5 temperature data for Bucharest (baseline)
#------------------------------------------------------------------------------

cat("Loading 2023 ERA5 temperature data...\n")

nc_files <- list.files("data/2023_temp", pattern = paste0(city_name_lower, "_2023_\\d{2}\\.nc$"),
                       full.names = TRUE)
nc_files <- sort(nc_files)

temp_2023_list <- lapply(nc_files, function(f) {
  r <- rast(f)
  vals <- global(r, "mean", na.rm = TRUE)$mean
  vals_celsius <- vals - 273.15
  month <- as.numeric(gsub(".*_(\\d{2})\\.nc$", "\\1", f))
  n_days <- nlyr(r)
  start_date <- as.Date(paste0("2023-", sprintf("%02d", month), "-01"))
  dates <- seq(start_date, by = "day", length.out = n_days)
  data.table(date = dates, tmean_2023 = vals_celsius)
})

temp_2023 <- rbindlist(temp_2023_list)
temp_2023 <- temp_2023[order(date)]

cat(sprintf("Loaded %d days of 2023 temperature data\n", nrow(temp_2023)))

#------------------------------------------------------------------------------
# Step 2: Load projected temperature data
#------------------------------------------------------------------------------

cat("\nLoading projected temperature data...\n")

proj_data <- open_dataset("data/tmeanproj.gz.parquet") %>%
  filter(URAU_CODE == city_code) %>%
  collect() %>%
  as.data.table()

proj_data[, year := year(date)]

gcm_cols <- names(proj_data)[grepl("^tas_", names(proj_data))]
gcm_cols <- gcm_cols[!gsub("tas_", "", gcm_cols) %in% gcmexcl]

cat(sprintf("Using %d GCMs\n", length(gcm_cols)))

#------------------------------------------------------------------------------
# Step 3: Load RR coefficients for all age groups
#------------------------------------------------------------------------------

cat("\nLoading RR coefficients...\n")

coefs_all <- fread("data/coefs.csv")
coefs_city <- coefs_all[URAU_CODE == city_code]

cat(sprintf("Loaded coefficients for %d age groups\n", nrow(coefs_city)))

#------------------------------------------------------------------------------
# Step 4: Define basis function parameters using historical data
#------------------------------------------------------------------------------

cat("\nDefining basis function parameters...\n")

hist_data <- proj_data[ssp == "hist"]
hist_temps <- unlist(hist_data[, ..gcm_cols], use.names = FALSE)
hist_temps <- hist_temps[!is.na(hist_temps)]

varknots <- quantile(hist_temps, varper / 100, na.rm = TRUE)
varbound <- range(hist_temps, na.rm = TRUE)

argvar <- list(fun = varfun, degree = vardegree, knots = varknots, Bound = varbound)

cat(sprintf("Historical temperature range: %.1f°C to %.1f°C\n", varbound[1], varbound[2]))

#------------------------------------------------------------------------------
# Step 5: Compute RR curves for each age group and find MMT
#------------------------------------------------------------------------------

cat("\nComputing RR curves for each age group...\n")

# Temperature sequence
temp_seq <- seq(varbound[1], varbound[2], by = 0.5)
n_temp <- length(temp_seq)

# Build basis
basis <- do.call(onebasis, c(list(x = temp_seq), argvar))

# Store RR matrix: rows = temperature, columns = age groups
rr_matrix <- matrix(NA, nrow = n_temp, ncol = length(agelabs))
mmt_vec <- numeric(length(agelabs))

for (i in seq_along(agelabs)) {
  age <- agelabs[i]
  coef_row <- coefs_city[agegroup == age]
  coefs <- as.numeric(coef_row[, .(b1, b2, b3, b4, b5)])
  
  log_rr <- basis %*% coefs
  
  # Find MMT in 25-99 percentile range
  ind <- temp_seq >= quantile(temp_seq, 0.25) & temp_seq <= quantile(temp_seq, 0.99)
  mmt <- temp_seq[ind][which.min(log_rr[ind])]
  mmt_vec[i] <- mmt
  
  # Center at MMT
  cenvec <- do.call(onebasis, c(list(x = mmt), argvar))
  log_rr_centered <- log_rr - drop(cenvec %*% coefs)
  
  rr <- pmax(exp(log_rr_centered), 1)
  rr_matrix[, i] <- as.vector(rr)
  
  cat(sprintf("  %s (midpoint: %.1f): MMT = %.1f°C\n", age, age_midpoints[i], mmt))
}

#------------------------------------------------------------------------------
# Step 6: Interpolate RR to single-year ages
#------------------------------------------------------------------------------

cat("\nInterpolating RR to single-year ages...\n")

# For each temperature, interpolate RR across ages
rr_single_age <- matrix(NA, nrow = n_temp, ncol = length(age_range))
colnames(rr_single_age) <- age_range

for (t_idx in seq_len(n_temp)) {
  rr_at_temp <- rr_matrix[t_idx, ]
  # Linear interpolation with extrapolation at boundaries
  rr_interp <- approx(x = age_midpoints, y = rr_at_temp, 
                      xout = age_range, rule = 2)$y
  rr_single_age[t_idx, ] <- rr_interp
}

# Also interpolate MMT for each single-year age
mmt_single_age <- approx(x = age_midpoints, y = mmt_vec, 
                         xout = age_range, rule = 2)$y

cat(sprintf("Interpolated to %d single-year ages (20-100)\n", length(age_range)))

#------------------------------------------------------------------------------
# Step 7: Function to compute average RR for a temperature vector at each age
#------------------------------------------------------------------------------

compute_avg_rr_by_age <- function(temps) {
  temps <- temps[!is.na(temps)]
  if (length(temps) == 0) return(rep(NA_real_, length(age_range)))
  
  # For each temperature in the vector, find the closest index in temp_seq
  temp_indices <- sapply(temps, function(t) {
    which.min(abs(temp_seq - t))
  })
  
  # Average RR across all days for each age
  avg_rr <- colMeans(rr_single_age[temp_indices, , drop = FALSE])
  return(avg_rr)
}

#------------------------------------------------------------------------------
# Step 8: Compute average RR for 2023 baseline (by age)
#------------------------------------------------------------------------------

cat("\nComputing 2023 baseline RR by age...\n")

rr_2023_by_age <- compute_avg_rr_by_age(temp_2023$tmean_2023)

cat(sprintf("Baseline RR range: %.4f to %.4f\n", min(rr_2023_by_age), max(rr_2023_by_age)))

#------------------------------------------------------------------------------
# Step 9: Compute average RR for target years (averaging across GCMs)
#------------------------------------------------------------------------------

cat("\nComputing projected RR by age for target years...\n")

results_list <- list()

for (yr in target_years) {
  cat(sprintf("  Processing year %d...\n", yr))
  
  year_data <- proj_data[ssp == target_ssp & year == yr]
  
  # Average across all GCMs for this year
  all_temps <- c()
  for (gcm_col in gcm_cols) {
    all_temps <- c(all_temps, year_data[[gcm_col]])
  }
  all_temps <- all_temps[!is.na(all_temps)]
  
  avg_rr <- compute_avg_rr_by_age(all_temps)
  
  # Compute multiplier
  multiplier <- avg_rr / rr_2023_by_age
  
  results_list[[as.character(yr)]] <- data.table(
    year = yr,
    age = age_range,
    avg_rr = avg_rr,
    multiplier = multiplier
  )
}

results <- rbindlist(results_list)
results[, year_label := paste0(year)]

cat("\nMultiplier Summary by Year:\n")
print(results[, .(min_mult = min(multiplier), max_mult = max(multiplier), 
                   mult_at_65 = multiplier[age == 65]), by = year])

#------------------------------------------------------------------------------
# Step 10: Save results
#------------------------------------------------------------------------------

cat("\nSaving results...\n")
fwrite(results, sprintf("results_csv/%s_mortality_multiplier_by_age.csv", city_name_lower))

#------------------------------------------------------------------------------
# Step 11: Create visualization
#------------------------------------------------------------------------------

cat("\nCreating visualization...\n")

# Color palette for years
year_colors <- c(
  "2050" = SeaBlue,
  "2075" = ElectricBlue,
  "2099" = OceanBlue
)

p <- ggplot(results, aes(x = age, y = multiplier, color = year_label)) +
  # Baseline reference
  geom_hline(yintercept = 1, linetype = "dashed", color = OceanBlue, alpha = 0.5) +
  # Lines
  geom_line(linewidth = 0.7) +
  # Scales
  scale_color_manual(values = year_colors, name = NULL) +
  scale_x_continuous(breaks = seq(20, 100, by = 10)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  # Labels
  labs(
    x = "Age",
    y = "Mortality Multiplier",
    title = sprintf("%s: Mortality Multiplier by Age", city_name),
    subtitle = "Under RCP 7.0, relative to 2023 baseline"
  ) +
  # Theme
  theme_ie(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -5),
    plot.margin = margin(10, 15, 10, 10)
  )

# Save PNG
ggsave(file.path(img_dir, "sample_multi.png"), p, 
       width = 6, height = 4, dpi = 300, bg = "white")

# Save PDF
ggsave(file.path(img_dir, "sample_multi.pdf"), p, 
       width = 6, height = 4, device = cairo_pdf)

# Save JPG (referenced by main.tex)
ggsave(file.path(img_dir, "sample_multi.jpg"), p, 
       width = 6, height = 4, dpi = 300, bg = "white")

cat(sprintf("Plot saved to %s/sample_multi.pdf\n", img_dir))

cat("\nDone!\n")
