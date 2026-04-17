################################################################################
#
# Mortality Multiplier Evolution Plots
#
# Computes per-GCM mortality multipliers for age groups across years and
# produces:
#   1. Evolution over time for age 65-74 under 3 RCPs (with GCM uncertainty)
#   2. Combined panel: (a) multiplier by age + (b) evolution over time
#   3. Evolution by age groups (65-74, 75-84, 85+) under RCP 4.5
#
# Outputs:
#   results_csv/  — 5 CSV files with per-GCM and summary data
#   plots/        — 3 plot pairs (PDF + PNG) with caption .txt files
#
################################################################################

library(data.table)
library(arrow)
library(dplyr)
library(dlnm)
library(splines)
library(ggplot2)
library(patchwork)
library(showtext)

#------------------------------------------------------------------------------
# IE Blue Template Styling
#------------------------------------------------------------------------------

font_add("Montserrat",
         regular = "fonts/Montserrat-Regular.ttf",
         bold    = "fonts/Montserrat-Bold.ttf",
         italic  = "fonts/Montserrat-Italic.ttf",
         bolditalic = "fonts/Montserrat-BoldItalic.ttf")
showtext_auto()

OceanBlue    <- "#000066"
ElectricBlue <- "#0000db"
SeaBlue      <- "#47bfff"
TechGreen    <- "#6DC201"

theme_ie <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "Montserrat") +
    theme(
      text         = element_text(family = "Montserrat", color = OceanBlue),
      plot.title   = element_text(face = "bold", color = OceanBlue,
                                  size = base_size * 1.4),
      plot.subtitle = element_text(color = OceanBlue, size = base_size * 0.9),
      plot.caption = element_text(hjust = 0, size = base_size * 0.75,
                                  color = OceanBlue),
      axis.title   = element_text(color = OceanBlue, face = "bold"),
      axis.text    = element_text(color = OceanBlue),
      legend.title = element_text(color = OceanBlue, face = "bold"),
      legend.text  = element_text(color = OceanBlue),
      panel.grid.major = element_line(color = "#e0e0e0"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

#------------------------------------------------------------------------------
# Load city configuration
#------------------------------------------------------------------------------
source("legacy/config.R")

proj_years   <- (cohort_start_year + 1):cohort_end_year
target_years <- c(2050, 2075, 2099)
focus_groups <- c("65-74", "75-84", "85+")

if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)

cat(sprintf("=== Mortality Multiplier Evolution Plots ===\n"))
cat(sprintf("City: %s | Baseline: %s | Cohort: %d-%d\n",
            city_name, baseline_temp_label, cohort_start_year, cohort_end_year))

#------------------------------------------------------------------------------
# Step 1: Load projected temperature data
#------------------------------------------------------------------------------

cat("\nStep 1: Loading projected temperature data...\n")

proj_data <- open_dataset("data/tmeanproj.gz.parquet") %>%
  filter(URAU_CODE == city_code) %>%
  collect() %>%
  as.data.table()

proj_data[, year := year(date)]

gcm_cols <- names(proj_data)[grepl("^tas_", names(proj_data))]
gcm_cols <- gcm_cols[!gsub("tas_", "", gcm_cols) %in% gcmexcl]
gcm_names <- gsub("tas_", "", gcm_cols)

cat(sprintf("  Using %d GCMs\n", length(gcm_cols)))

#------------------------------------------------------------------------------
# Step 2: Load RR coefficients and build basis
#------------------------------------------------------------------------------

cat("\nStep 2: Loading RR coefficients and building basis...\n")

coefs_all  <- fread("data/coefs.csv")
coefs_city <- coefs_all[URAU_CODE == city_code]

hist_data  <- proj_data[ssp == "hist"]
hist_temps <- unlist(hist_data[, ..gcm_cols], use.names = FALSE)
hist_temps <- hist_temps[!is.na(hist_temps)]

varknots <- quantile(hist_temps, varper / 100, na.rm = TRUE)
varbound <- range(hist_temps, na.rm = TRUE)
argvar   <- list(fun = varfun, degree = vardegree, knots = varknots,
                 Bound = varbound)

temp_seq <- seq(varbound[1], varbound[2], by = 0.5)
n_temp   <- length(temp_seq)
basis    <- do.call(onebasis, c(list(x = temp_seq), argvar))

#------------------------------------------------------------------------------
# Step 3: Compute RR curves per age group (centred at MMT, RR >= 1)
#------------------------------------------------------------------------------

cat("\nStep 3: Computing RR curves per age group...\n")

rr_matrix <- matrix(NA, nrow = n_temp, ncol = length(agelabs))
mmt_vec   <- numeric(length(agelabs))

for (i in seq_along(agelabs)) {
  ag       <- agelabs[i]
  coef_row <- coefs_city[agegroup == ag]
  coefs    <- as.numeric(coef_row[, .(b1, b2, b3, b4, b5)])

  log_rr <- basis %*% coefs
  ind    <- temp_seq >= quantile(temp_seq, 0.25) &
            temp_seq <= quantile(temp_seq, 0.99)
  mmt    <- temp_seq[ind][which.min(log_rr[ind])]
  mmt_vec[i] <- mmt

  cenvec         <- do.call(onebasis, c(list(x = mmt), argvar))
  log_rr_centred <- log_rr - drop(cenvec %*% coefs)
  rr_matrix[, i] <- as.vector(pmax(exp(log_rr_centred), 1))

  cat(sprintf("  %s: MMT = %.1f°C\n", ag, mmt))
}
names(mmt_vec) <- agelabs

#------------------------------------------------------------------------------
# Step 4: Interpolate RR to single-year ages
#------------------------------------------------------------------------------

cat("\nStep 4: Interpolating to single-year ages...\n")

age_range     <- 20:100
rr_single_age <- matrix(NA, nrow = n_temp, ncol = length(age_range))
colnames(rr_single_age) <- age_range

for (t_idx in seq_len(n_temp)) {
  rr_single_age[t_idx, ] <- approx(age_midpoints, rr_matrix[t_idx, ],
                                    xout = age_range, rule = 2)$y
}

mmt_single_age <- approx(age_midpoints, mmt_vec, xout = age_range, rule = 2)$y
names(mmt_single_age) <- age_range

# Helper: average RR for a temperature vector, per single-year age
compute_avg_rr <- function(temps) {
  temps <- temps[!is.na(temps)]
  if (length(temps) == 0) return(rep(NA_real_, length(age_range)))
  idx <- sapply(temps, function(t) which.min(abs(temp_seq - t)))
  rr_vals <- rr_single_age[idx, , drop = FALSE]
  # Component filtering
  if (rr_component != "total") {
    for (j in seq_along(age_range)) {
      mmt <- mmt_single_age[j]
      if (rr_component == "heat") {
        rr_vals[temps <= mmt, j] <- 1
      } else if (rr_component == "cold") {
        rr_vals[temps > mmt, j] <- 1
      }
    }
  }
  colMeans(rr_vals)
}

# Helper: average RR for a temperature vector, per age-GROUP
compute_avg_rr_group <- function(temps) {
  temps <- temps[!is.na(temps)]
  if (length(temps) == 0) return(rep(NA_real_, length(agelabs)))
  idx <- sapply(temps, function(t) which.min(abs(temp_seq - t)))
  rr_vals <- rr_matrix[idx, , drop = FALSE]
  # Component filtering
  if (rr_component != "total") {
    for (j in seq_along(agelabs)) {
      mmt <- mmt_vec[j]
      if (rr_component == "heat") {
        rr_vals[temps <= mmt, j] <- 1
      } else if (rr_component == "cold") {
        rr_vals[temps > mmt, j] <- 1
      }
    }
  }
  colMeans(rr_vals)
}

#------------------------------------------------------------------------------
# Step 5: Compute baseline RR
#------------------------------------------------------------------------------

cat("\nStep 5: Computing baseline RR...\n")

baseline_hist <- proj_data[ssp == "hist" & year %in% baseline_temp_period]
baseline_proj <- proj_data[ssp %in% ssp_codes &
                             year %in% baseline_temp_period & year > 2014]

baseline_temps_all <- c(
  unlist(baseline_hist[, ..gcm_cols], use.names = FALSE),
  unlist(baseline_proj[, ..gcm_cols], use.names = FALSE)
)
baseline_temps_all <- baseline_temps_all[!is.na(baseline_temps_all)]

rr_baseline_by_age   <- compute_avg_rr(baseline_temps_all)
rr_baseline_by_group <- compute_avg_rr_group(baseline_temps_all)

cat(sprintf("  Baseline RR (single-age) range: %.4f – %.4f\n",
            min(rr_baseline_by_age), max(rr_baseline_by_age)))

#------------------------------------------------------------------------------
# Step 6: Per-GCM multipliers for focus age group (65-74) across all years/SSPs
#------------------------------------------------------------------------------

cat("\nStep 6: Computing per-GCM multipliers (65-74)...\n")

# Index for 65-74 group
focus_idx <- which(agelabs == "65-74")

gcm_results <- list()

for (ssp_val in ssp_codes) {
  ssp_data <- proj_data[ssp == ssp_val]

  for (yr in proj_years) {
    year_data <- ssp_data[year == yr]
    if (nrow(year_data) == 0) next

    for (gcm_col in gcm_cols) {
      temps <- year_data[[gcm_col]]
      temps <- temps[!is.na(temps)]
      if (length(temps) == 0) next

      # Compute avg RR for 65-74 only (fast: single column)
      idx   <- sapply(temps, function(t) which.min(abs(temp_seq - t)))
      rr_col <- rr_matrix[idx, focus_idx]
      # Apply heat/cold component filtering
      if (rr_component == "heat") {
        rr_col[temps <= mmt_vec[focus_idx]] <- 1
      } else if (rr_component == "cold") {
        rr_col[temps > mmt_vec[focus_idx]] <- 1
      }
      avg_rr <- mean(rr_col)
      mult   <- avg_rr / rr_baseline_by_group[focus_idx]

      gcm_results[[length(gcm_results) + 1]] <- data.table(
        year = yr, ssp = ssp_val,
        gcm  = gsub("tas_", "", gcm_col),
        avg_rr = avg_rr, multiplier = mult
      )
    }
  }
}

gcm_dt <- rbindlist(gcm_results)

cat(sprintf("  Computed %s per-GCM records\n",
            format(nrow(gcm_dt), big.mark = ",")))

#------------------------------------------------------------------------------
# Step 7: Summary statistics across GCMs
#------------------------------------------------------------------------------

cat("\nStep 7: Computing summary statistics...\n")

summary_dt <- gcm_dt[, .(
  multiplier_mean   = mean(multiplier),
  multiplier_median = median(multiplier),
  multiplier_q10    = quantile(multiplier, 0.10),
  multiplier_q25    = quantile(multiplier, 0.25),
  multiplier_q75    = quantile(multiplier, 0.75),
  multiplier_q90    = quantile(multiplier, 0.90),
  n_gcm             = .N
), by = .(year, ssp)]

summary_dt[, rcp_label := rcp_labels[ssp]]

#------------------------------------------------------------------------------
# Step 8: Per-RCP combined (65-74) — median + p10/p90 for overlay plots
#------------------------------------------------------------------------------

rcp_combined_dt <- summary_dt[, .(
  ssp       = ssp,
  ssp_label = rcp_label,
  year      = year,
  median    = multiplier_median,
  p10       = multiplier_q10,
  p90       = multiplier_q90
)]

#------------------------------------------------------------------------------
# Step 9: Per-age-group multipliers (65-74, 75-84, 85+) under RCP 4.5
#------------------------------------------------------------------------------

cat("\nStep 9: Computing per-age-group multipliers...\n")

age_group_results <- list()

for (ag in focus_groups) {
  ag_idx   <- which(agelabs == ag)
  ssp_data <- proj_data[ssp == "2"]  # RCP 4.5

  for (yr in proj_years) {
    year_data <- ssp_data[year == yr]
    if (nrow(year_data) == 0) next

    gcm_mults <- numeric(0)
    for (gcm_col in gcm_cols) {
      temps <- year_data[[gcm_col]]
      temps <- temps[!is.na(temps)]
      if (length(temps) == 0) next
      idx    <- sapply(temps, function(t) which.min(abs(temp_seq - t)))
      rr_col <- rr_matrix[idx, ag_idx]
      # Apply heat/cold component filtering
      if (rr_component == "heat") {
        rr_col[temps <= mmt_vec[ag_idx]] <- 1
      } else if (rr_component == "cold") {
        rr_col[temps > mmt_vec[ag_idx]] <- 1
      }
      avg_rr <- mean(rr_col)
      gcm_mults <- c(gcm_mults, avg_rr / rr_baseline_by_group[ag_idx])
    }

    age_group_results[[length(age_group_results) + 1]] <- data.table(
      age_group        = ag,
      year             = yr,
      multiplier_mean  = mean(gcm_mults),
      multiplier_median = median(gcm_mults),
      multiplier_q10   = quantile(gcm_mults, 0.10),
      multiplier_q90   = quantile(gcm_mults, 0.90)
    )
  }
}

age_group_dt <- rbindlist(age_group_results)

#------------------------------------------------------------------------------
# Step 10: By-age combined (single-year ages, target years, RCP 4.5, pooled)
#------------------------------------------------------------------------------

cat("\nStep 10: Computing by-age combined for target years...\n")

by_age_results <- list()

for (yr in target_years) {
  year_data <- proj_data[ssp == "2" & year == yr]
  all_temps <- c()
  for (gcm_col in gcm_cols) all_temps <- c(all_temps, year_data[[gcm_col]])
  all_temps <- all_temps[!is.na(all_temps)]

  avg_rr     <- compute_avg_rr(all_temps)
  multiplier <- avg_rr / rr_baseline_by_age

  by_age_results[[as.character(yr)]] <- data.table(
    year       = yr,
    age        = age_range,
    multiplier = multiplier,
    year_label = as.character(yr)
  )
}

by_age_dt <- rbindlist(by_age_results)

#------------------------------------------------------------------------------
# Step 11: Save all CSVs
#------------------------------------------------------------------------------

cat("\nStep 11: Saving CSVs...\n")

fwrite(gcm_dt,
       sprintf("results_csv/mortality_multiplier_by_gcm_%s.csv", city_name_lower))
fwrite(summary_dt,
       sprintf("results_csv/mortality_multiplier_summary_%s.csv", city_name_lower))
fwrite(rcp_combined_dt,
       sprintf("results_csv/mortality_multiplier_by_rcp_combined_%s.csv", city_name_lower))
fwrite(age_group_dt,
       sprintf("results_csv/mortality_multiplier_by_age_group_%s.csv", city_name_lower))
fwrite(by_age_dt,
       sprintf("results_csv/mortality_multiplier_by_age_combined_%s.csv", city_name_lower))

cat("  Done.\n")

################################################################################
# PLOTS
################################################################################

rcp_colors <- c("RCP 2.6" = SeaBlue,
                "RCP 4.5" = ElectricBlue,
                "RCP 7.0" = OceanBlue)

ag_colors  <- c("65-74" = SeaBlue,
                "75-84" = ElectricBlue,
                "85+"   = OceanBlue)

year_colors <- c("2050" = SeaBlue, "2075" = ElectricBlue, "2099" = OceanBlue)

#------------------------------------------------------------------------------
# Plot 1: Evolution over time for 65-74 under 3 RCPs
#------------------------------------------------------------------------------

cat("\nPlot 1: Multiplier evolution (65-74, 3 RCPs)...\n")

plot_evol <- summary_dt[, rcp_label := factor(rcp_label,
                                               levels = c("RCP 2.6", "RCP 4.5", "RCP 7.0"))]

p1 <- ggplot(plot_evol, aes(x = year, color = rcp_label, fill = rcp_label)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = OceanBlue, alpha = 0.5) +
  geom_ribbon(aes(ymin = multiplier_q10, ymax = multiplier_q90),
              alpha = 0.12, colour = NA) +
  geom_line(aes(y = multiplier_median), linewidth = 0.7) +
  scale_color_manual(values = rcp_colors, name = NULL) +
  scale_fill_manual(values = rcp_colors, name = NULL) +
  scale_x_continuous(breaks = seq(2020, 2100, 10)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(
    x = "Year", y = "Mortality Multiplier",
    title = sprintf("%s: Mortality Multiplier (65\u201374)", city_name),
    subtitle = sprintf("%s RR — relative to %s baseline",
                        tools::toTitleCase(rr_component), baseline_temp_label)
  ) +
  theme_ie(base_size = 11) +
  theme(legend.position = "bottom", legend.margin = margin(t = -5),
        plot.margin = margin(10, 15, 10, 10))

ggsave("plots/mortality_multiplier_evolution_%s.png" |> sprintf(city_name_lower),
       p1, width = 7, height = 4.5, dpi = 300, bg = "white")
ggsave("plots/mortality_multiplier_evolution_%s.pdf" |> sprintf(city_name_lower),
       p1, width = 7, height = 4.5, device = cairo_pdf)

writeLines(sprintf(
  "Projected %s mortality multiplier for %s (age group 65--74) under three
Representative Concentration Pathways (RCP 2.6, 4.5, and 7.0), relative to the
%s baseline. Solid lines represent the median across %d CMIP6 climate models;
shaded bands indicate the 10th and 90th percentile range. The horizontal dashed
line at 1.0 marks the baseline level. The mortality multiplier is computed
as the ratio of projected annual mean %s relative risk (RR) to the %s baseline RR,
using temperature-mortality exposure-response functions from the Multi-Country
Multi-City (MCC) Collaborative Research Network.",
  rr_component, city_name, baseline_temp_label, length(gcm_cols),
  rr_component, baseline_temp_label
), sprintf("plots/mortality_multiplier_evolution_%s_caption.txt", city_name_lower))

cat("  Saved.\n")

#------------------------------------------------------------------------------
# Plot 2: Combined (a) by-age + (b) evolution
#------------------------------------------------------------------------------

cat("\nPlot 2: Combined panel...\n")

pa <- ggplot(by_age_dt, aes(x = age, y = multiplier, color = year_label)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = OceanBlue, alpha = 0.5) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = year_colors, name = NULL) +
  scale_x_continuous(breaks = seq(20, 100, 10)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(x = "Age", y = "Mortality Multiplier", tag = "(a)",
       title = "Multiplier by age (RCP 4.5)") +
  theme_ie(base_size = 10) +
  theme(legend.position = "bottom", legend.margin = margin(t = -5))

# Panel (b): evolution for 65-74, 3 RCPs
pb <- ggplot(plot_evol, aes(x = year, color = rcp_label, fill = rcp_label)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = OceanBlue, alpha = 0.5) +
  geom_ribbon(aes(ymin = multiplier_q10, ymax = multiplier_q90),
              alpha = 0.12, colour = NA) +
  geom_line(aes(y = multiplier_median), linewidth = 0.7) +
  scale_color_manual(values = rcp_colors, name = NULL) +
  scale_fill_manual(values = rcp_colors, name = NULL) +
  scale_x_continuous(breaks = seq(2020, 2100, 10)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(x = "Year", y = "Mortality Multiplier", tag = "(b)",
       title = "Multiplier over time (65\u201374)") +
  theme_ie(base_size = 10) +
  theme(legend.position = "bottom", legend.margin = margin(t = -5))

p2 <- pa | pb

ggsave("plots/mortality_multiplier_combined_%s.png" |> sprintf(city_name_lower),
       p2, width = 12, height = 5, dpi = 300, bg = "white")
ggsave("plots/mortality_multiplier_combined_%s.pdf" |> sprintf(city_name_lower),
       p2, width = 12, height = 5, device = cairo_pdf)

writeLines(sprintf(
  "Figure X. Projected mortality multiplier for %s under climate change scenarios,
relative to the %s baseline (GCM climatological average). (a) Mortality multiplier by age under
the RCP 4.5 scenario for years 2050, 2075, and 2099. The multiplier increases with age,
reflecting greater vulnerability of older populations to temperature-related mortality.
(b) Mortality multiplier over time (%d-%d) for the 65-74 age group under three
Representative Concentration Pathways (RCP 2.6, 4.5, and 7.0). Shaded bands represent
the 10th and 90th percentile range across %d CMIP6 climate models. The mortality multiplier
is computed as the ratio of projected annual mean relative risk (RR) to the %s
baseline RR, using temperature-mortality exposure-response functions from the
Multi-Country Multi-City (MCC) Collaborative Research Network.",
  city_name, baseline_temp_label,
  min(proj_years), max(proj_years), length(gcm_cols),
  baseline_temp_label
), sprintf("plots/mortality_multiplier_combined_%s_caption.txt", city_name_lower))

cat("  Saved.\n")

#------------------------------------------------------------------------------
# Plot 3: Evolution by age groups (65-74, 75-84, 85+) under RCP 4.5
#------------------------------------------------------------------------------

cat("\nPlot 3: Evolution by age groups (RCP 4.5)...\n")

age_group_dt[, age_group := factor(age_group, levels = focus_groups)]

p3 <- ggplot(age_group_dt,
             aes(x = year, color = age_group, fill = age_group)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = OceanBlue, alpha = 0.5) +
  geom_ribbon(aes(ymin = multiplier_q10, ymax = multiplier_q90),
              alpha = 0.12, colour = NA) +
  geom_line(aes(y = multiplier_median), linewidth = 0.7) +
  scale_color_manual(values = ag_colors, name = "Age group") +
  scale_fill_manual(values = ag_colors, name = "Age group") +
  scale_x_continuous(breaks = seq(2020, 2100, 10)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(
    x = "Year", y = "Mortality Multiplier",
    title = sprintf("%s: Mortality Multiplier by Age Group", city_name),
    subtitle = sprintf("Under RCP 4.5, %s RR — relative to %s baseline",
                        rr_component,
                        baseline_temp_label)
  ) +
  theme_ie(base_size = 11) +
  theme(legend.position = "bottom", legend.margin = margin(t = -5),
        plot.margin = margin(10, 15, 10, 10))

ggsave("plots/mortality_multiplier_age_groups_%s.png" |> sprintf(city_name_lower),
       p3, width = 7, height = 4.5, dpi = 300, bg = "white")
ggsave("plots/mortality_multiplier_age_groups_%s.pdf" |> sprintf(city_name_lower),
       p3, width = 7, height = 4.5, device = cairo_pdf)

writeLines(sprintf(
  "Projected mortality multiplier for %s by age group (65--74, 75--84, and 85+)
under RCP 4.5, relative to the %s baseline. Solid lines represent the median across
%d CMIP6 climate models; shaded bands indicate the 10th and 90th percentile range.
The horizontal dashed line at 1.0 marks the baseline level. Older age groups
show slightly higher mortality multipliers, reflecting increased vulnerability to
temperature-related mortality with age. The mortality multiplier is computed as the
ratio of projected annual mean relative risk (RR) to the %s baseline RR, using
temperature-mortality exposure-response functions from the Multi-Country Multi-City
(MCC) Collaborative Research Network.",
  city_name, baseline_temp_label, length(gcm_cols),
  baseline_temp_label
), sprintf("plots/mortality_multiplier_age_groups_%s_caption.txt", city_name_lower))

cat("  Saved.\n")

cat("\n=== All plots and CSVs generated. ===\n")
