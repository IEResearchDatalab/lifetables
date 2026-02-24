################################################################################
#
# Mortality Multiplier by Age — Heat / Cold / Total Decomposition
# 
# This script computes the mortality multiplier for single-year ages (20-100)
# interpolated from age-group RR curves, for years 2050, 2075, and 2099,
# decomposed into heat-only, cold-only, and total components.
#
################################################################################

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

OceanBlue    <- "#000066"
ElectricBlue <- "#0000db"
SeaBlue      <- "#47bfff"
TechGreen    <- "#6DC201"
WarmRed      <- "#D7263D"
CoolBlue     <- "#1B98E0"
NeutralGrey  <- "#7F7F7F"

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

# Components to compute
components <- c("heat", "cold", "total")

#------------------------------------------------------------------------------
# Step 1: Load projected temperature data (includes historical baseline)
#------------------------------------------------------------------------------

cat("Loading projected temperature data...\n")

proj_data <- open_dataset("data/tmeanproj.gz.parquet") %>%
  filter(URAU_CODE == city_code) %>%
  collect() %>%
  as.data.table()

proj_data[, year := year(date)]
proj_data[, doy := as.integer(format(date, "%j"))]
proj_data[doy > 365, doy := 365L]  # cap leap-year day 366

gcm_cols <- names(proj_data)[grepl("^tas_", names(proj_data))]
gcm_cols <- gcm_cols[!gsub("tas_", "", gcm_cols) %in% gcmexcl]

cat(sprintf("Using %d GCMs\n", length(gcm_cols)))

# --- Load seasonal mortality weights ---
seasonal_weights_file <- sprintf("results_csv/seasonal_weights_daily_%s.csv", city_name_lower)
if (file.exists(seasonal_weights_file)) {
  sw_dt <- fread(seasonal_weights_file)
  sw_matrix <- matrix(1 / 365, nrow = 81, ncol = 365,
                      dimnames = list(20:100, 1:365))
  for (i in seq_len(nrow(sw_dt))) {
    a <- sw_dt$age[i]; d <- sw_dt$doy[i]
    sw_matrix[as.character(a), d] <- sw_dt$weight[i]
  }
  use_seasonal_weights <- TRUE
  cat("Loaded seasonal mortality weights (age × DOY)\n")
} else {
  use_seasonal_weights <- FALSE
  cat("Seasonal weights not found — using uniform weighting\n")
}

#------------------------------------------------------------------------------
# Step 2: Load RR coefficients for all age groups
#------------------------------------------------------------------------------

cat("\nLoading RR coefficients...\n")

coefs_all <- fread("data/coefs.csv")
coefs_city <- coefs_all[URAU_CODE == city_code]

cat(sprintf("Loaded coefficients for %d age groups\n", nrow(coefs_city)))

#------------------------------------------------------------------------------
# Step 3: Define basis function parameters using historical data
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
# Step 4: Compute RR curves for each age group and find MMT
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
# Step 5: Interpolate RR to single-year ages
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
# Step 6: Function to compute average RR for a temperature vector at each age
#------------------------------------------------------------------------------

compute_avg_rr_by_age <- function(temps, component = "total", doys = NULL) {
  temps <- temps[!is.na(temps)]
  if (length(temps) == 0) return(rep(NA_real_, length(age_range)))
  
  # For each temperature in the vector, find the closest index in temp_seq
  temp_indices <- sapply(temps, function(t) {
    which.min(abs(temp_seq - t))
  })
  
  # Get RR matrix for these temperature indices
  rr_vals <- rr_single_age[temp_indices, , drop = FALSE]
  
  # Apply component filtering per age (heat/cold decomposition)
  if (component != "total") {
    for (j in seq_along(age_range)) {
      mmt <- mmt_single_age[j]
      if (component == "heat") {
        rr_vals[temps <= mmt, j] <- 1
      } else if (component == "cold") {
        rr_vals[temps > mmt, j] <- 1
      }
    }
  }
  
  # Seasonal-weighted or uniform average RR across all days for each age
  if (use_seasonal_weights && !is.null(doys)) {
    avg_rr <- numeric(length(age_range))
    for (j in seq_along(age_range)) {
      w <- sw_matrix[as.character(age_range[j]), doys]
      avg_rr[j] <- weighted.mean(rr_vals[, j], w)
    }
    return(avg_rr)
  } else {
    return(colMeans(rr_vals))
  }
}

#------------------------------------------------------------------------------
# Step 7: Pool baseline temperatures
#------------------------------------------------------------------------------

cat(sprintf("\nPooling baseline temperatures (%s)...\n", baseline_temp_label))

baseline_hist <- proj_data[ssp == "hist" & year %in% baseline_temp_period]
baseline_proj <- proj_data[ssp %in% ssp_codes & year %in% baseline_temp_period & year > 2014]

baseline_temps_all <- c(
  unlist(baseline_hist[, ..gcm_cols], use.names = FALSE),
  unlist(baseline_proj[, ..gcm_cols], use.names = FALSE)
)
baseline_doys_all <- c(
  rep(baseline_hist$doy, length(gcm_cols)),
  rep(baseline_proj$doy, length(gcm_cols))
)
valid_bl <- !is.na(baseline_temps_all)
baseline_temps_all <- baseline_temps_all[valid_bl]
baseline_doys_all  <- baseline_doys_all[valid_bl]

cat(sprintf("  Pooled %s baseline temperature values\n",
            format(length(baseline_temps_all), big.mark = ",")))

#------------------------------------------------------------------------------
# Step 8: Pool target-year temperatures
#------------------------------------------------------------------------------

cat("\nPooling target-year temperatures...\n")

target_temps <- list()
target_doys <- list()
for (yr in target_years) {
  year_data <- proj_data[ssp == target_ssp & year == yr]
  all_temps <- c()
  all_doys  <- c()
  for (gcm_col in gcm_cols) {
    all_temps <- c(all_temps, year_data[[gcm_col]])
    all_doys  <- c(all_doys,  year_data$doy)
  }
  valid_yr <- !is.na(all_temps)
  target_temps[[as.character(yr)]] <- all_temps[valid_yr]
  target_doys[[as.character(yr)]]  <- all_doys[valid_yr]
  cat(sprintf("  %d: %d daily values\n", yr, length(target_temps[[as.character(yr)]])))
}

#------------------------------------------------------------------------------
# Step 9: Compute multipliers for all 3 components x 3 target years
#------------------------------------------------------------------------------

cat("\nComputing multipliers for heat / cold / total...\n")

results_list <- list()

for (comp in components) {
  cat(sprintf("  Component: %s\n", comp))
  
  rr_baseline <- compute_avg_rr_by_age(baseline_temps_all, component = comp,
                                       doys = baseline_doys_all)
  
  for (yr in target_years) {
    avg_rr     <- compute_avg_rr_by_age(target_temps[[as.character(yr)]], component = comp,
                                        doys = target_doys[[as.character(yr)]])
    multiplier <- avg_rr / rr_baseline
    
    results_list[[length(results_list) + 1]] <- data.table(
      component  = comp,
      year       = yr,
      age        = age_range,
      avg_rr     = avg_rr,
      multiplier = multiplier
    )
  }
}

results <- rbindlist(results_list)
results[, year_label := factor(year)]
results[, component  := factor(component, levels = c("heat", "total", "cold"),
                                labels = c("Heat", "Total", "Cold"))]

cat("\nMultiplier Summary by Component and Year:\n")
print(results[, .(min_mult = round(min(multiplier), 4),
                   max_mult = round(max(multiplier), 4),
                   mult_at_65 = round(multiplier[age == 65], 4)),
              by = .(component, year)])

#------------------------------------------------------------------------------
# Step 9: Save results
#------------------------------------------------------------------------------

cat("\nSaving results...\n")
fwrite(results, sprintf("results_csv/mortality_multiplier_by_age_%s.csv", city_name_lower))

#------------------------------------------------------------------------------
# Step 11: Create decomposition visualizations
#------------------------------------------------------------------------------

cat("\nCreating decomposition visualizations...\n")

component_colors <- c(
  "Heat"  = WarmRed,
  "Total" = NeutralGrey,
  "Cold"  = CoolBlue
)

# --- Plot A: Faceted by year, lines colored by component ---

p_facet <- ggplot(results, aes(x = age, y = multiplier, color = component)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = OceanBlue, alpha = 0.4) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ year, ncol = 3) +
  scale_color_manual(values = component_colors, name = NULL) +
  scale_x_continuous(breaks = seq(20, 100, by = 20)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(
    x = "Age",
    y = "Mortality Multiplier",
    title = sprintf("%s: Mortality Multiplier Decomposition by Age", city_name),
    subtitle = sprintf("Under RCP 7.0, relative to %s baseline", baseline_temp_label)
  ) +
  theme_ie(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -5),
    strip.text = element_text(face = "bold", color = OceanBlue, size = 11),
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave(file.path(img_dir, sprintf("multiplier_decomposition_by_year_%s.png", city_name_lower)), p_facet,
       width = 9, height = 4, dpi = 300, bg = "white")
ggsave(file.path(img_dir, sprintf("multiplier_decomposition_by_year_%s.pdf", city_name_lower)), p_facet,
       width = 9, height = 4, device = cairo_pdf)
cat("  Saved multiplier_decomposition_by_year.{png,pdf}\n")

# --- Plot B: 2099 only, all three components (single clean panel) ---

p_2099 <- ggplot(results[year == 2099], aes(x = age, y = multiplier, color = component)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = OceanBlue, alpha = 0.4) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = component_colors, name = NULL) +
  scale_x_continuous(breaks = seq(20, 100, by = 10)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(
    x = "Age",
    y = "Mortality Multiplier",
    title = sprintf("%s: Heat / Cold / Total Mortality Multiplier (2099)", city_name),
    subtitle = sprintf("Under RCP 7.0, relative to %s baseline", baseline_temp_label)
  ) +
  theme_ie(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -5),
    plot.margin = margin(10, 15, 10, 10)
  )

# Save PDF

ggsave(file.path(img_dir, sprintf("multiplier_decomposition_2099_%s.png", city_name_lower)), p_2099,
       width = 6, height = 4, dpi = 300, bg = "white")
ggsave(file.path(img_dir, sprintf("multiplier_decomposition_2099_%s.pdf", city_name_lower)), p_2099,
       width = 6, height = 4, device = cairo_pdf)
cat("  Saved multiplier_decomposition_2099.{png,pdf}\n")

# Save JPG (referenced by main.tex)
ggsave(file.path(img_dir, sprintf("multiplier_decomposition_2099_%s.jpg", city_name_lower)), p_2099,
       width = 6, height = 4, dpi = 300, bg = "white")

cat(sprintf("Plot saved to %s/sample_multi_%s.pdf\n", img_dir, city_name_lower))
# --- Plot C: Heat-only by year (backward-compatible sample_multi) ---

p_heat <- ggplot(results[component == "Heat"],
                 aes(x = age, y = multiplier, color = year_label)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = OceanBlue, alpha = 0.5) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = c("2050" = SeaBlue, "2075" = ElectricBlue, "2099" = OceanBlue),
                     name = NULL) +
  scale_x_continuous(breaks = seq(20, 100, by = 10)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(
    x = "Age",
    y = "Mortality Multiplier",
    title = sprintf("%s: Heat Mortality Multiplier by Age", city_name),
    subtitle = sprintf("Under RCP 7.0, relative to %s baseline", baseline_temp_label)
  ) +
  theme_ie(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -5),
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave(file.path(img_dir, sprintf("sample_multi_%s.png", city_name_lower)), p_heat,
       width = 6, height = 4, dpi = 300, bg = "white")
ggsave(file.path(img_dir, sprintf("sample_multi_%s.pdf", city_name_lower)), p_heat,
       width = 6, height = 4, device = cairo_pdf)
ggsave(file.path(img_dir, sprintf("sample_multi_%s.jpg", city_name_lower)), p_heat,
       width = 6, height = 4, dpi = 300, bg = "white")
cat("  Saved sample_multi.{png,pdf,jpg}\n")

cat("\nDone!\n")
