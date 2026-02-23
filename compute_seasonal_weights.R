# ==============================================================================
# Seasonal Mortality Weights from Eurostat Weekly Deaths
# ==============================================================================
#
# Downloads weekly deaths by 5-year age group from Eurostat (demo_r_mweek3)
# for the target NUTS 3 region (Bucharest), averages over a reference period,
# then applies PCLM ungrouping to obtain single-year age × week-of-year
# seasonal mortality weights.
#
# Output: results_csv/bucharest_seasonal_weights.csv
#   Columns: age, week, weight  (weight sums to 1 within each age)
#
# The weights capture when during the year baseline deaths concentrate,
# so that the temperature–mortality RR average is mortality-weighted
# rather than uniform across the year.
#
# Method:
#   1. Download demo_r_mweek3 for RO321, sex = T (total)
#   2. Exclude COVID years (2020–2021)
#   3. Average weekly deaths over 2015–2019 by age group
#   4. Apply PCLM per week (52 fits) to ungroup 5-year ages → single-year ages
#   5. Normalise within each age so weights sum to 1 over 52 weeks
#   6. Expand to 365 daily weights (each day gets its week's weight / 7)
#
# ==============================================================================

library(eurostat)
library(data.table)
library(ungroup)     # for pclm()
library(ggplot2)
library(dplyr)

source("config.R")

# ==============================================================================
# STEP 1: Download Eurostat weekly deaths
# ==============================================================================

cat("Step 1: Downloading Eurostat demo_r_mweek3 for", nuts3_code, "...\n")

raw <- get_eurostat("demo_r_mweek3",
                    filters = list(geo = nuts3_code, sex = "T"),
                    time_format = "raw")

dt <- as.data.table(raw)
dt[, year := as.integer(substr(as.character(time), 1, 4))]
dt[, week := as.integer(sub(".*W", "", as.character(time)))]

cat(sprintf("  Downloaded %d rows\n", nrow(dt)))
cat(sprintf("  Available years: %s\n",
            paste(sort(unique(dt[!is.na(values) & age == "TOTAL", year])),
                  collapse = ", ")))

# ==============================================================================
# STEP 2: Select reference period (pre-COVID overlap with baseline)
# ==============================================================================

# The baseline_temp_period is 1990-2019, but Eurostat weekly deaths for RO321
# are only available from 2015. We use 2015-2019 (5 pre-COVID years).
ref_years <- 2015:2019

cat(sprintf("\nStep 2: Reference period for seasonal weights: %d–%d\n",
            min(ref_years), max(ref_years)))

# Exclude W53 (only some years have it) and keep standard 52 weeks
dt_ref <- dt[year %in% ref_years & week <= 52 & !is.na(values) &
               age != "TOTAL" & age != "UNK"]

cat(sprintf("  %d records in reference period\n", nrow(dt_ref)))

# ==============================================================================
# STEP 3: Parse age groups and compute weekly averages
# ==============================================================================

# Map Eurostat age labels to lower bounds and widths
age_map <- data.table(
  age_code  = c("Y_LT5", "Y5-9", "Y10-14", "Y15-19", "Y20-24", "Y25-29",
                "Y30-34", "Y35-39", "Y40-44", "Y45-49", "Y50-54", "Y55-59",
                "Y60-64", "Y65-69", "Y70-74", "Y75-79", "Y80-84", "Y85-89",
                "Y_GE90"),
  age_start = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55,
                60, 65, 70, 75, 80, 85, 90),
  age_width = c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 11)
  # Last group 90+ → we extend to age 100, so width = 11
)

dt_ref <- merge(dt_ref, age_map, by.x = "age", by.y = "age_code", all.x = TRUE)

# Average weekly deaths across reference years, by week × age group
avg_deaths <- dt_ref[, .(mean_deaths = mean(values, na.rm = TRUE)),
                     by = .(week, age_start, age_width)]

# Sort
setorder(avg_deaths, week, age_start)

cat(sprintf("  %d week × age-group combinations\n", nrow(avg_deaths)))
cat("  Total mean weekly deaths (all ages): ",
    round(avg_deaths[, sum(mean_deaths) / 52], 1), "per week\n")

# ==============================================================================
# STEP 4: PCLM ungrouping – one fit per week
# ==============================================================================

cat("\nStep 4: Applying PCLM ungrouping (52 weekly fits)...\n")

# Output age range: single years 0–100
out_ages <- 0:100
n_ages <- length(out_ages)

# Container for fitted single-year deaths by week
pclm_results <- vector("list", 52)

for (w in 1:52) {
  wk_data <- avg_deaths[week == w]
  setorder(wk_data, age_start)
  
  # Input vectors for PCLM
  x_lower <- wk_data$age_start
  y_counts <- wk_data$mean_deaths
  nlast <- wk_data$age_width[nrow(wk_data)]  # width of 90+ group
  
  # Replace zeros with a tiny positive value (PCLM requirement)
  y_counts[y_counts == 0] <- 0.001
  
  # Fit PCLM
  fit <- suppressWarnings(
    pclm(x = x_lower, y = y_counts, nlast = nlast, out.step = 1)
  )
  
  pclm_results[[w]] <- data.table(
    week = w,
    age  = 0:(length(fit$fitted) - 1),
    deaths_pclm = as.numeric(fit$fitted)
  )
  
  if (w %% 13 == 0) cat(sprintf("  Week %d done\n", w))
}

pclm_all <- rbindlist(pclm_results)

cat(sprintf("  PCLM output: %d rows (%d weeks × %d ages)\n",
            nrow(pclm_all), 52, length(unique(pclm_all$age))))

# Restrict to ages 20–100 (matching our cohort age range)
pclm_all <- pclm_all[age >= 20 & age <= 100]

# ==============================================================================
# STEP 5: Compute normalised seasonal weights
# ==============================================================================

cat("\nStep 5: Computing normalised seasonal weights...\n")

# Within each age, normalise so weights sum to 1 over 52 weeks
pclm_all[, total_deaths := sum(deaths_pclm), by = age]
pclm_all[, weight := deaths_pclm / total_deaths]

# Verify normalisation
check <- pclm_all[, .(sum_wt = sum(weight)), by = age]
cat(sprintf("  Weight sums: min = %.6f, max = %.6f (should be 1.0)\n",
            min(check$sum_wt), max(check$sum_wt)))

# ==============================================================================
# STEP 6: Expand to 365 daily weights
# ==============================================================================

cat("\nStep 6: Expanding to 365 daily weights...\n")

# Map day-of-year (1–365) to ISO week (1–52)
doy_week <- pmin(ceiling(1:365 / 7), 52L)

# Build daily grid by looking up weekly weight for each age × doy
daily_weights <- CJ(age = 20:100, doy = 1:365)
daily_weights[, week := doy_week[doy]]

# Merge weekly weights
daily_weights <- merge(daily_weights,
                       pclm_all[, .(age, week, weight)],
                       by = c("age", "week"),
                       all.x = TRUE)

# Re-normalise so daily weights sum to 1 within each age (365 days)
daily_weights[, weight := weight / sum(weight), by = age]

# Keep only needed columns, sorted
setorder(daily_weights, age, doy)
daily_weights <- daily_weights[, .(age, doy, week, weight)]

cat(sprintf("  Daily weights: %d rows (%d ages × 365 days)\n",
            nrow(daily_weights), length(unique(daily_weights$age))))

# Verify
check2 <- daily_weights[, .(sum_wt = sum(weight)), by = age]
cat(sprintf("  Daily weight sums: min = %.6f, max = %.6f\n",
            min(check2$sum_wt), max(check2$sum_wt)))

# ==============================================================================
# STEP 7: Save outputs
# ==============================================================================

cat("\nStep 7: Saving outputs...\n")

# Weekly weights (compact)
fwrite(pclm_all[, .(age, week, deaths_pclm, weight)],
       "results_csv/bucharest_seasonal_weights_weekly.csv")

# Daily weights (for pipeline integration)
fwrite(daily_weights,
       "results_csv/bucharest_seasonal_weights_daily.csv")

cat("  Saved: results_csv/bucharest_seasonal_weights_weekly.csv\n")
cat("  Saved: results_csv/bucharest_seasonal_weights_daily.csv\n")

# ==============================================================================
# STEP 8: Diagnostic plots
# ==============================================================================

cat("\nStep 8: Creating diagnostic plots...\n")

# --- Plot A: Seasonal profile for selected ages ---

plot_ages <- c(20, 40, 60, 70, 80, 90, 100)
plot_dt <- pclm_all[age %in% plot_ages]
plot_dt[, age_label := factor(paste0("Age ", age), 
                               levels = paste0("Age ", plot_ages))]

# Convert week to approximate month for x-axis
plot_dt[, month_approx := week * 7 / 30.44]

p1 <- ggplot(plot_dt, aes(x = week, y = weight, colour = age_label)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(
    breaks = c(1, 5, 9, 14, 18, 22, 27, 31, 35, 40, 44, 48),
    labels = month.abb
  ) +
  scale_colour_viridis_d(name = "Age") +
  labs(
    title = paste0(city_name, ": Seasonal Mortality Profile by Age"),
    subtitle = paste0("PCLM-ungrouped weekly weights (", 
                      min(ref_years), "–", max(ref_years), " average)"),
    x = "Week of year",
    y = "Share of annual deaths"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

ggsave(file.path(img_dir, "seasonal_weights_profile.png"),
       p1, width = 10, height = 5.5, dpi = 300)
ggsave(file.path(img_dir, "seasonal_weights_profile.pdf"),
       p1, width = 10, height = 5.5)

cat("  Saved: img/seasonal_weights_profile.{png,pdf}\n")

# --- Plot B: Heatmap of weekly weights by age ---

p2 <- ggplot(pclm_all, aes(x = week, y = age, fill = weight)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Weight", option = "inferno") +
  scale_x_continuous(
    breaks = c(1, 5, 9, 14, 18, 22, 27, 31, 35, 40, 44, 48),
    labels = month.abb
  ) +
  scale_y_continuous(breaks = seq(20, 100, 10)) +
  labs(
    title = paste0(city_name, ": Seasonal Mortality Weight Surface"),
    subtitle = paste0("Week × single-year age (PCLM, ",
                      min(ref_years), "–", max(ref_years), ")"),
    x = "Week of year",
    y = "Age"
  ) +
  theme_minimal(base_size = 12) +
  coord_cartesian(expand = FALSE)

ggsave(file.path(img_dir, "seasonal_weights_heatmap.png"),
       p2, width = 10, height = 6, dpi = 300)
ggsave(file.path(img_dir, "seasonal_weights_heatmap.pdf"),
       p2, width = 10, height = 6)

cat("  Saved: img/seasonal_weights_heatmap.{png,pdf}\n")

# --- Plot C: Winter/summer ratio by age ---

# Compare Dec-Feb (weeks 49-52, 1-8) vs Jun-Aug (weeks 23-35)
winter_weeks <- c(49:52, 1:8)
summer_weeks <- 23:35

ws_ratio <- pclm_all[, .(
  winter = sum(weight[week %in% winter_weeks]),
  summer = sum(weight[week %in% summer_weeks])
), by = age]
ws_ratio[, ratio := winter / summer]

p3 <- ggplot(ws_ratio, aes(x = age, y = ratio)) +
  geom_line(linewidth = 0.8, colour = "#1B98E0") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  labs(
    title = paste0(city_name, ": Winter-to-Summer Mortality Ratio by Age"),
    subtitle = paste0("Weeks 49–52 & 1–8 vs weeks 23–35 (",
                      min(ref_years), "–", max(ref_years), ")"),
    x = "Age", y = "Winter / Summer deaths ratio"
  ) +
  scale_x_continuous(breaks = seq(20, 100, 10)) +
  theme_minimal(base_size = 12)

ggsave(file.path(img_dir, "seasonal_winter_summer_ratio.png"),
       p3, width = 8, height = 5, dpi = 300)
ggsave(file.path(img_dir, "seasonal_winter_summer_ratio.pdf"),
       p3, width = 8, height = 5)

cat("  Saved: img/seasonal_winter_summer_ratio.{png,pdf}\n")

# ==============================================================================
# Summary statistics
# ==============================================================================

cat("\n=== Summary ===\n")
cat(sprintf("Reference years: %s\n", paste(ref_years, collapse = ", ")))
cat(sprintf("Age range: %d–%d (single years via PCLM)\n", 
            min(pclm_all$age), max(pclm_all$age)))
cat(sprintf("Weeks: 1–52\n"))

# Show peak week and weight for key ages
for (a in c(20, 45, 65, 80, 100)) {
  peak <- pclm_all[age == a][which.max(weight)]
  trough <- pclm_all[age == a][which.min(weight)]
  cat(sprintf("  Age %3d: peak week %2d (w=%.4f), trough week %2d (w=%.4f), ratio = %.2f\n",
              a, peak$week, peak$weight, trough$week, trough$weight,
              peak$weight / trough$weight))
}

cat("\nDone. Seasonal weights ready for pipeline integration.\n")
