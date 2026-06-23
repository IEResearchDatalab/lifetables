library(data.table)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
mx_to_qx <- function(mx, ax = 0.5) {
  mx / (1 + (1 - ax) * mx)
}

qx_to_mx <- function(qx, ax = 0.5) {
  qx / (1 - (1 - ax) * qx)
}

whole_life_annuity <- function(lt, qx_col = "qx", i = 0.02) {
  v <- 1 / (1 + i)
  n <- nrow(lt)
  px <- 1 - lt[[qx_col]]
  kpx <- cumprod(c(1, px[-n]))
  k <- 0:(n - 1)
  sum(v^k * kpx)
}

whole_life_insurance <- function(lt, qx_col = "qx", i = 0.02) {
  v <- 1 / (1 + i)
  n <- nrow(lt)
  qx <- lt[[qx_col]]
  px <- 1 - qx
  kpx <- cumprod(c(1, px[-n]))
  sum(v^(1:n) * kpx * qx)
}

pct_delta <- function(base, clim) {
  100 * (clim - base) / base
}

# ---------------------------------------------------------------------------
# Build cohort life table from mortality projections + country multiplier matrix
# ---------------------------------------------------------------------------
build_cohort_lt_country <- function(start_age, start_year, mort_dt, mult_dt,
                                    omega = 100) {
  ages  <- start_age:omega
  years <- start_year:(start_year + omega - start_age)
  lt <- data.table(age = ages, year = years)

  max_mort_year <- max(mort_dt$year)
  lt <- merge(lt, mort_dt[, .(year, age, qx, mx, ax)], by = c("year", "age"), all.x = TRUE)

  if (any(is.na(lt$qx))) {
    for (i in which(is.na(lt$qx))) {
      a <- lt$age[i]
      fb <- mort_dt[year == max_mort_year & age == a]
      if (nrow(fb) > 0) {
        lt$qx[i] <- fb$qx[1]; lt$mx[i] <- fb$mx[1]; lt$ax[i] <- fb$ax[1]
      } else {
        lt$qx[i] <- 1; lt$mx[i] <- 10; lt$ax[i] <- 0.5
      }
    }
  }

  # Country multiplier matrix: columns = years from 2025 onward, rows = ages
  mult_years <- as.character(years[years >= 2025])
  lt[, multiplier := 1.0]
  for (yr in mult_years) {
    yr_int <- as.integer(yr)
    if (yr_int %in% as.integer(names(mult_dt))) {
      idx <- which(lt$year == yr_int)
      for (i in seq_along(idx)) {
        a <- lt$age[idx[i]]
        mult_row <- mult_dt[age == a]
        if (nrow(mult_row) > 0) {
          lt$multiplier[idx[i]] <- mult_row[[yr]]
        }
      }
    }
  }

  lt <- lt[order(age)]
  lt[, mx_clim := mx * multiplier]
  lt[, qx_clim := mx_to_qx(mx_clim, ax)]
  lt[qx > 1, qx := 1]
  lt[qx_clim > 1, qx_clim := 1]

  # Terminate life table: everyone dies by the final age
  lt[age == omega, `:=`(qx = 1, qx_clim = 1, mx = 10, mx_clim = 10)]

  return(lt)
}

# ===========================================================================
# PART 1: EPV change by entry age — Austria & Romania
#          RCP 7.0, cohort entering 2025, i = 1%
#          Using country-level multiplier matrices
# ===========================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("PART 1: EPV changes by entry age (country-level)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

entry_ages <- seq(30, 70, by = 5)
entry_year <- 2025
interest   <- 0.01

countries <- list(
  Austria = list(mort_city = "vienna",  mult_file = "austria_rcp70.csv"),
  Romania = list(mort_city = "bucharest", mult_file = "romania_rcp70.csv")
)

epv_results <- list()

for (cn in names(countries)) {
  cfg <- countries[[cn]]
  cat("Processing:", cn, "\n")

  mort_path <- sprintf("results_csv/mortality_projections_%s.csv", cfg$mort_city)
  mult_path <- sprintf("results_csv/country_multiplier_matrices/%s", cfg$mult_file)

  mort <- fread(mort_path)
  mort <- mort[age >= 0]
  setkey(mort, year, age)

  mult <- fread(mult_path)
  setkey(mult, age)

  for (x in entry_ages) {
    lt <- build_cohort_lt_country(x, entry_year, mort, mult)

    ann_base   <- whole_life_annuity(lt, "qx", interest)
    ann_clim   <- whole_life_annuity(lt, "qx_clim", interest)
    ins_base   <- whole_life_insurance(lt, "qx", interest)
    ins_clim   <- whole_life_insurance(lt, "qx_clim", interest)

    epv_results[[length(epv_results) + 1]] <- data.table(
      country      = cn,
      entry_age    = x,
      entry_year   = entry_year,
      interest     = interest,
      rcp          = "RCP 7.0",
      annuity_base     = ann_base,
      annuity_clim     = ann_clim,
      pct_delta_annuity = pct_delta(ann_base, ann_clim),
      insurance_base    = ins_base,
      insurance_clim    = ins_clim,
      pct_delta_insurance = pct_delta(ins_base, ins_clim)
    )
  }
}

epv_table <- rbindlist(epv_results)
cat("\nEPV change table (RCP 7.0, i = 1%, entry year = 2025):\n")
print(epv_table[, .(country, entry_age,
                     annuity_base  = round(annuity_base, 4),
                     pct_delta_annuity = round(pct_delta_annuity, 4),
                     insurance_base = round(insurance_base, 4),
                     pct_delta_insurance = round(pct_delta_insurance, 4))], row.names = FALSE)

fwrite(epv_table, "results_csv/epv_change_by_age_rcp70_i1.csv")
cat("\nSaved: results_csv/epv_change_by_age_rcp70_i1.csv\n")

# ===========================================================================
# PART 2: RR curve for Austria — country-level, 0.1 deg C, -40 to 40 deg C
# ===========================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("PART 2: Austria country-level RR curve\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Use existing country-level ERF curve data and extrapolate
erf <- fread("results_csv/erf_curves_country.csv")
erf_at <- erf[country_code == "AT"]

cat(sprintf("Loaded %d rows for AT\n", nrow(erf_at)))
cat(sprintf("Temperature range: [%.1f, %.1f] C\n", min(erf_at$temp), max(erf_at$temp)))

agelabs <- c("20-44", "45-64", "65-74", "75-84", "85+")
age_midpoints <- c(32.5, 55, 70, 80, 92.5)
age_range <- 20:100

# Get sorted temperatures from ERF data
erf_at <- erf_at[order(temp)]
temp_vals <- erf_at$temp

# Build RR matrix per age group
rr_ag <- as.matrix(erf_at[, ..agelabs])
colnames(rr_ag) <- NULL

# Find MMT per age group (temp where RR == 1, or minimal)
mmt_vec <- numeric(5)
for (i in 1:5) {
  # MMT is the temperature where RR is minimum (>= 1)
  rr_col <- rr_ag[, i]
  # Find where RR is minimum
  mmt_idx <- which.min(rr_col)
  mmt_vec[i] <- temp_vals[mmt_idx]
  cat(sprintf("  %s: MMT = %.1f C (RR min = %.4f)\n",
              agelabs[i], mmt_vec[i], rr_col[mmt_idx]))
}

# Extrapolate: fit log-linear tails beyond the data range
# For cold tail (left): use the coldest 10% of data to fit exponential
# For heat tail (right): use the hottest 10% of data to fit exponential
target_seq <- seq(-40, 40, by = 0.1)
n_target <- length(target_seq)

rr_extrap <- matrix(NA, nrow = n_target, ncol = 5)
colnames(rr_extrap) <- agelabs

cat("\nExtrapolating RR to [-40, 40] C at 0.1 C resolution...\n")

for (i in 1:5) {
  rr_col <- rr_ag[, i]
  log_rr <- log(rr_col)

  # Cold tail: fit linear model on log(RR) for coldest 5 points
  n_cold <- 5
  cold_idx <- 1:n_cold
  cold_lm <- lm(log_rr[cold_idx] ~ temp_vals[cold_idx])
  cat(sprintf("  %s cold tail slope: %.4f\n", agelabs[i], coef(cold_lm)[2]))

  # Heat tail: fit linear model on log(RR) for hottest 5 points
  n_hot <- 5
  n_all <- length(temp_vals)
  hot_idx <- (n_all - n_hot + 1):n_all
  hot_lm <- lm(log_rr[hot_idx] ~ temp_vals[hot_idx])
  cat(sprintf("  %s heat tail slope: %.4f\n", agelabs[i], coef(hot_lm)[2]))

  t_min <- min(temp_vals)
  t_max <- max(temp_vals)

  for (j in 1:n_target) {
    t <- target_seq[j]
    if (t < t_min) {
      # Cold extrapolation
      log_rr_t <- cold_lm$coef[1] + cold_lm$coef[2] * t
      rr_extrap[j, i] <- exp(log_rr_t)
    } else if (t > t_max) {
      # Heat extrapolation
      log_rr_t <- hot_lm$coef[1] + hot_lm$coef[2] * t
      rr_extrap[j, i] <- exp(log_rr_t)
    } else {
      # Interpolate from existing data
      rr_extrap[j, i] <- approx(temp_vals, rr_col, xout = t, rule = 2)$y
    }
  }
  # Floor at 1
  rr_extrap[, i] <- pmax(rr_extrap[, i], 1)
}

rr_table <- as.data.table(rr_extrap)
setnames(rr_table, agelabs)
rr_table[, temperature := target_seq]

# Mark extrapolated points
t_min <- min(temp_vals)
t_max <- max(temp_vals)
rr_table[, extrapolated := fifelse(temperature < t_min | temperature > t_max, 1L, 0L)]

setcolorder(rr_table, "temperature")

cat(sprintf("\nFinal RR table: %d rows x %d cols\n", nrow(rr_table), ncol(rr_table)))
cat(sprintf("Temperature range: [%.1f, %.1f] C\n", min(rr_table$temperature), max(rr_table$temperature)))
cat("\nSample rows:\n")
print(rr_table[temperature %in% seq(-40, 40, by = 10)])

fwrite(rr_table, "results_csv/rr_curve_austria_fine.csv")
cat("\nSaved: results_csv/rr_curve_austria_fine.csv\n")

# ===========================================================================
# PART 3: Generate plots
# ===========================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("PART 3: Generating plots\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

library(ggplot2)
library(scales)

# --- Plot 1: RR curves per age group ---
rr_long <- melt(rr_table, id.vars = c("temperature", "extrapolated"),
                variable.name = "age_group", value.name = "RR")

p_rr <- ggplot(rr_long, aes(x = temperature, y = RR, color = age_group,
                             linetype = factor(extrapolated))) +
  geom_line(linewidth = 0.8) +
  scale_linetype_manual(values = c("0" = "solid", "1" = "dashed"),
                        labels = c("0" = "Interpolated", "1" = "Extrapolated"),
                        name = NULL) +
  scale_color_manual(values = c("20-44" = "#1f77b4", "45-64" = "#ff7f0e",
                                 "65-74" = "#2ca02c", "75-84" = "#d62728",
                                 "85+"   = "#9467bd"),
                     name = "Age group") +
  labs(x = "Temperature (°C)", y = "Relative Risk (RR)",
       title = "Temperature–Mortality Relative Risk — Austria") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.box = "vertical")

ggsave("img/rr_curve_austria.png", p_rr, width = 10, height = 6, dpi = 150)
cat("Saved: img/rr_curve_austria.png\n")

# --- Plot 2: EPV % change bar chart ---
epv_plot <- melt(epv_table, id.vars = c("country", "entry_age"),
                  measure.vars = c("pct_delta_annuity", "pct_delta_insurance"),
                  variable.name = "product", value.name = "pct_delta")
epv_plot[, product := fifelse(product == "pct_delta_annuity", "Annuity", "Insurance")]

p_epv <- ggplot(epv_plot, aes(x = entry_age, y = pct_delta, fill = country)) +
  geom_col(position = position_dodge(width = 3), width = 2.5) +
  scale_x_continuous(breaks = seq(30, 70, by = 5)) +
  scale_fill_manual(values = c("Austria" = "#1f77b4", "Romania" = "#ff7f0e"),
                    name = NULL) +
  facet_wrap(~ product, ncol = 1, scales = "free_y") +
  labs(x = "Entry age", y = "EPV change (%)",
       title = "EPV Change Under RCP 7.0 (i = 1%, entry year 2025)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("img/epv_change_bar.png", p_epv, width = 10, height = 7, dpi = 150)
cat("Saved: img/epv_change_bar.png\n")

cat("\nAll done.\n")