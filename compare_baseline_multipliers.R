################################################################################
#
# Baseline Comparison: Total Mortality Multiplier
#
# Computes total (heat + cold) mortality multipliers under two baselines:
#   (a) 1990-2019 climatological average (30-year WMO normal from GCM data)
#   (b) 2023 single-year (GCM data for the same year as original ERA5 baseline)
#
# Purpose: demonstrate that the choice of baseline period has minimal effect
# on the resulting mortality multipliers — the climate signal is robust.
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
# Load configuration
#------------------------------------------------------------------------------
source("config.R")

target_years <- c(2050, 2075, 2099)
age_range    <- 20:100
target_ssp   <- "3"   # RCP 7.0

#------------------------------------------------------------------------------
# Step 1: Load data
#------------------------------------------------------------------------------

cat("Loading projected temperature data...\n")

proj_data <- open_dataset("data/tmeanproj.gz.parquet") %>%
  filter(URAU_CODE == city_code) %>%
  collect() %>%
  as.data.table()

proj_data[, year := year(date)]

gcm_cols <- names(proj_data)[grepl("^tas_", names(proj_data))]
gcm_cols <- gcm_cols[!gsub("tas_", "", gcm_cols) %in% gcmexcl]

cat(sprintf("Using %d GCMs\n", length(gcm_cols)))

#------------------------------------------------------------------------------
# Step 2: Load RR coefficients and build basis
#------------------------------------------------------------------------------

cat("\nBuilding RR curves...\n")

coefs_all  <- fread("data/coefs.csv")
coefs_city <- coefs_all[URAU_CODE == city_code]

hist_data  <- proj_data[ssp == "hist"]
hist_temps <- unlist(hist_data[, ..gcm_cols], use.names = FALSE)
hist_temps <- hist_temps[!is.na(hist_temps)]

varknots <- quantile(hist_temps, varper / 100, na.rm = TRUE)
varbound <- range(hist_temps, na.rm = TRUE)
argvar   <- list(fun = varfun, degree = vardegree, knots = varknots, Bound = varbound)

temp_seq <- seq(varbound[1], varbound[2], by = 0.5)
n_temp   <- length(temp_seq)
basis    <- do.call(onebasis, c(list(x = temp_seq), argvar))

rr_matrix <- matrix(NA, nrow = n_temp, ncol = length(agelabs))
mmt_vec   <- numeric(length(agelabs))

for (i in seq_along(agelabs)) {
  coef_row <- coefs_city[agegroup == agelabs[i]]
  coefs    <- as.numeric(coef_row[, .(b1, b2, b3, b4, b5)])
  log_rr   <- basis %*% coefs

  ind <- temp_seq >= quantile(temp_seq, 0.25) & temp_seq <= quantile(temp_seq, 0.99)
  mmt <- temp_seq[ind][which.min(log_rr[ind])]
  mmt_vec[i] <- mmt

  cenvec          <- do.call(onebasis, c(list(x = mmt), argvar))
  log_rr_centered <- log_rr - drop(cenvec %*% coefs)
  rr_matrix[, i]  <- pmax(exp(log_rr_centered), 1)
}

#------------------------------------------------------------------------------
# Step 3: Interpolate to single-year ages
#------------------------------------------------------------------------------

rr_single_age <- matrix(NA, nrow = n_temp, ncol = length(age_range))
colnames(rr_single_age) <- age_range

for (t_idx in seq_len(n_temp)) {
  rr_single_age[t_idx, ] <- approx(age_midpoints, rr_matrix[t_idx, ],
                                    xout = age_range, rule = 2)$y
}

#------------------------------------------------------------------------------
# Step 4: Average RR function (total component only)
#------------------------------------------------------------------------------

compute_avg_rr <- function(temps) {
  temps <- temps[!is.na(temps)]
  if (length(temps) == 0) return(rep(NA_real_, length(age_range)))
  idx <- sapply(temps, function(t) which.min(abs(temp_seq - t)))
  colMeans(rr_single_age[idx, , drop = FALSE])
}

#------------------------------------------------------------------------------
# Step 5: Pool temperatures for both baselines and target years
#------------------------------------------------------------------------------

cat("\nPooling temperatures...\n")

# Baseline A: 1990-2019 (30-year climatological average)
bl_hist_a <- proj_data[ssp == "hist" & year %in% 1990:2019]
bl_proj_a <- proj_data[ssp %in% ssp_codes & year %in% 1990:2019 & year > 2014]
temps_baseline_a <- c(
  unlist(bl_hist_a[, ..gcm_cols], use.names = FALSE),
  unlist(bl_proj_a[, ..gcm_cols], use.names = FALSE)
)
temps_baseline_a <- temps_baseline_a[!is.na(temps_baseline_a)]
cat(sprintf("  Baseline 1990-2019: %s values, mean %.2f°C\n",
            format(length(temps_baseline_a), big.mark = ","), mean(temps_baseline_a)))

# Baseline B: 2023 only (single year from GCM projections)
bl_proj_b <- proj_data[ssp %in% ssp_codes & year == 2023]
temps_baseline_b <- unlist(bl_proj_b[, ..gcm_cols], use.names = FALSE)
temps_baseline_b <- temps_baseline_b[!is.na(temps_baseline_b)]
cat(sprintf("  Baseline 2023:      %s values, mean %.2f°C\n",
            format(length(temps_baseline_b), big.mark = ","), mean(temps_baseline_b)))

# Target years
target_temps <- list()
for (yr in target_years) {
  year_data <- proj_data[ssp == target_ssp & year == yr]
  all_temps <- unlist(year_data[, ..gcm_cols], use.names = FALSE)
  target_temps[[as.character(yr)]] <- all_temps[!is.na(all_temps)]
}

#------------------------------------------------------------------------------
# Step 6: Compute total multipliers under both baselines
#------------------------------------------------------------------------------

cat("\nComputing total multipliers under both baselines...\n")

rr_base_a <- compute_avg_rr(temps_baseline_a)
rr_base_b <- compute_avg_rr(temps_baseline_b)

results_list <- list()

for (yr in target_years) {
  avg_rr <- compute_avg_rr(target_temps[[as.character(yr)]])

  results_list[[length(results_list) + 1]] <- data.table(
    baseline = "1990\u20132019", year = yr, age = age_range,
    multiplier = avg_rr / rr_base_a
  )
  results_list[[length(results_list) + 1]] <- data.table(
    baseline = "2023", year = yr, age = age_range,
    multiplier = avg_rr / rr_base_b
  )
}

results <- rbindlist(results_list)
results[, year_label := factor(year)]

#------------------------------------------------------------------------------
# Step 7: Summary comparison
#------------------------------------------------------------------------------

cat("\nMultiplier comparison at age 65:\n")
comp <- results[age == 65, .(multiplier = round(multiplier, 5)), by = .(baseline, year)]
comp_wide <- dcast(comp, year ~ baseline, value.var = "multiplier")
setnames(comp_wide, c("year", "bl_1990_2019", "bl_2023"))
comp_wide[, diff := round(bl_1990_2019 - bl_2023, 5)]
comp_wide[, pct_diff := round((bl_1990_2019 / bl_2023 - 1) * 100, 3)]
print(comp_wide)

#------------------------------------------------------------------------------
# Step 8: Save results
#------------------------------------------------------------------------------

fwrite(results, sprintf("results_csv/%s_baseline_comparison_multipliers.csv", city_name_lower))
cat(sprintf("\nSaved: results_csv/%s_baseline_comparison_multipliers.csv\n", city_name_lower))

#------------------------------------------------------------------------------
# Step 9: Create comparison chart
#------------------------------------------------------------------------------

cat("\nCreating comparison chart...\n")

baseline_colors <- c(
  "1990\u20132019" = ElectricBlue,
  "2023"           = SeaBlue
)

baseline_linetypes <- c(
  "1990\u20132019" = "solid",
  "2023"           = "dashed"
)

# --- Plot A: Faceted by year ---

p_facet <- ggplot(results, aes(x = age, y = multiplier,
                                color = baseline, linetype = baseline)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = OceanBlue, alpha = 0.4) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ year, ncol = 3) +
  scale_color_manual(values = baseline_colors, name = "Baseline") +
  scale_linetype_manual(values = baseline_linetypes, name = "Baseline") +
  scale_x_continuous(breaks = seq(20, 100, by = 20)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
  labs(
    x = "Age",
    y = "Mortality Multiplier (Total RR)",
    title = sprintf("%s: Total Mortality Multiplier \u2014 Baseline Comparison", city_name),
    subtitle = "Under RCP 7.0 — 1990\u20132019 climatological average vs 2023 single year"
  ) +
  theme_ie(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -5),
    strip.text = element_text(face = "bold", color = OceanBlue, size = 11),
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave(file.path(img_dir, "baseline_comparison_total_rr.png"), p_facet,
       width = 9, height = 4, dpi = 300, bg = "white")
ggsave(file.path(img_dir, "baseline_comparison_total_rr.pdf"), p_facet,
       width = 9, height = 4, device = cairo_pdf)
cat("  Saved baseline_comparison_total_rr.{png,pdf}\n")

# --- Plot B: 2099 only (cleaner single panel) ---

p_2099 <- ggplot(results[year == 2099],
                 aes(x = age, y = multiplier,
                     color = baseline, linetype = baseline)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = OceanBlue, alpha = 0.4) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = baseline_colors, name = "Baseline") +
  scale_linetype_manual(values = baseline_linetypes, name = "Baseline") +
  scale_x_continuous(breaks = seq(20, 100, by = 10)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
  labs(
    x = "Age",
    y = "Mortality Multiplier (Total RR)",
    title = sprintf("%s: Total Mortality Multiplier \u2014 Baseline Comparison (2099)", city_name),
    subtitle = "Under RCP 7.0 — 1990\u20132019 climatological average vs 2023 single year"
  ) +
  theme_ie(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -5),
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave(file.path(img_dir, "baseline_comparison_total_rr_2099.png"), p_2099,
       width = 6, height = 4, dpi = 300, bg = "white")
ggsave(file.path(img_dir, "baseline_comparison_total_rr_2099.pdf"), p_2099,
       width = 6, height = 4, device = cairo_pdf)
cat("  Saved baseline_comparison_total_rr_2099.{png,pdf}\n")

cat("\nDone!\n")
