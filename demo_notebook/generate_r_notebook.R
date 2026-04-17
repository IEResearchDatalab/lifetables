# Script to generate the R demo notebook as proper .ipynb JSON
# Run: Rscript generate_r_notebook.R

library(jsonlite)

cells <- list()

add_md <- function(src) {
  cells[[length(cells) + 1]] <<- list(
    cell_type = "markdown",
    metadata = list(),
    source = strsplit(src, "\n", fixed = TRUE)[[1]]
  )
}

add_code <- function(src) {
  cells[[length(cells) + 1]] <<- list(
    cell_type = "code",
    execution_count = NULL,
    metadata = list(vscode = list(languageId = "r")),
    outputs = list(),
    source = strsplit(src, "\n", fixed = TRUE)[[1]]
  )
}

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 1: Title
# ═══════════════════════════════════════════════════════════════════════════════
add_md('# Demo: Full Pipeline (R)

This notebook replicates the climate-mortality pipeline end-to-end in R.

1. Load temperature distributions and RR coefficients.
2. Build B-spline basis and evaluate RR curves by age.
3. Compute annual mean RR and mortality multipliers.
4. Apply multipliers to Eurostat baseline mortality.
5. Build a cohort life table and compute EPVs.

**Data:** Upload `results_csv.zip` (same ZIP as the Python demo).')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 2: Setup
# ═══════════════════════════════════════════════════════════════════════════════
add_code('# 0. Setup
suppressPackageStartupMessages({
  library(splines)
  library(data.table)
  library(ggplot2)
})

# Wide aspect ratio for Colab/Jupyter
options(repr.plot.width = 12, repr.plot.height = 5)

# On Colab: upload results_csv.zip via the file browser,
# or place it in /content before running.
if (file.exists("/content") && !dir.exists("results_csv")) {
  unzip("results_csv.zip")
}

BASE <- "results_csv"
cat("Files:", paste(list.files(BASE), collapse = ", "), "\\n")')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 3: Parameters
# ═══════════════════════════════════════════════════════════════════════════════
add_code('# 1. Parameters
city_name  <- "Bucharest"   # Bucharest | Helsinki | Rome
rcp        <- 4.5           # 2.6 | 4.5 | 7.0

city_lower <- tolower(city_name)
rcp_str    <- gsub("\\\\.", "", as.character(rcp))
rcp_label  <- paste0("RCP", rcp_str)

city_lookup <- list(
  Bucharest = list(city_code = "RO001C", nuts3 = "RO321"),
  Helsinki  = list(city_code = "FI001C", nuts3 = "FI1B1"),
  Rome      = list(city_code = "IT001C", nuts3 = "ITI43")
)

city_code <- city_lookup[[city_name]]$city_code
ssp_map   <- c("2.6" = "SSP1", "4.5" = "SSP2", "7.0" = "SSP3")
ssp_val   <- ssp_map[as.character(rcp)]

# Cohort / financial parameters
cohort_start_age  <- 20L
cohort_start_year <- 2019L
cohort_end_year   <- 2099L
interest_rate     <- 0.02
v                 <- 1 / (1 + interest_rate)

# B-spline specification (Masselot et al. 2025)
vardegree <- 2L
varper    <- c(10, 75, 90)

# Annuity: deferred 45 years, term 20 (payments ages 65-84)
defer <- 45L
term  <- 20L

cat(sprintf("City: %s (%s) | RCP %.1f (%s) | i = %.0f%%\\n",
            city_name, city_code, rcp, ssp_val, interest_rate * 100))')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 4: Section header
# ═══════════════════════════════════════════════════════════════════════════════
add_md('## 2. Temperature Distributions')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 5: Load and plot temperature distributions
# ═══════════════════════════════════════════════════════════════════════════════
add_code('dist_bl  <- fread(file.path(BASE, sprintf("temp_distribution_baseline_%s.csv", city_lower)))
dist_pr  <- fread(file.path(BASE, sprintf("temp_distribution_projection_%s.csv", city_lower)))

# Filter to selected RCP
dist_pr <- dist_pr[rcp == rcp_label]
dist_pr[, rcp := NULL]

# Merge on temp_bin
dist <- merge(dist_bl, dist_pr, by = "temp_bin", all = TRUE)
for (j in names(dist)) set(dist, which(is.na(dist[[j]])), j, 0)
setnames(dist, c("n_days", "proportion"), c("n_days_baseline", "proportion_baseline"))
setorder(dist, temp_bin)

# Pool 2050-2059 for a representative future decade
cols_2050 <- paste0("n_days_", 2050:2059)
dist[, n_days_2050s := rowSums(.SD), .SDcols = cols_2050]
dist[, prop_2050s := n_days_2050s / sum(n_days_2050s)]

# Plot
df_hist <- rbind(
  data.table(temp = dist$temp_bin, prop = dist$proportion_baseline, period = "Baseline"),
  data.table(temp = dist$temp_bin, prop = dist$prop_2050s,
             period = sprintf("RCP %.1f (2050s)", rcp))
)
ggplot(df_hist, aes(x = temp, y = prop, fill = period)) +
  geom_col(position = "dodge", width = 0.8, alpha = 0.7) +
  scale_fill_manual(values = c("Baseline" = "steelblue",
                                setNames("tomato", sprintf("RCP %.1f (2050s)", rcp)))) +
  labs(x = "Temperature (C)", y = "Proportion",
       title = sprintf("Temperature Distribution - %s", city_name), fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 6: Section header
# ═══════════════════════════════════════════════════════════════════════════════
add_md('## 3. Exposure-Response Functions (RR Curves)')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 7: Load coefficients and define basis
# ═══════════════════════════════════════════════════════════════════════════════
add_code('coefs_city <- fread(file.path(BASE, sprintf("coefs_%s.csv", city_lower)))
coefs_city <- coefs_city[URAU_CODE == city_code]
coef_cols  <- sort(grep("^b\\\\d+$", names(coefs_city), value = TRUE))
all_ages   <- sort(unique(coefs_city$agegroup))   # single-year ages 20..100

cat(sprintf("%d age rows, %d spline coefficients each\\n",
            length(all_ages), length(coef_cols)))

# Basis parameters from baseline temperature range
temps_bl  <- dist$temp_bin[dist$n_days_baseline > 0]
tmin      <- min(temps_bl)
tmax      <- max(temps_bl)
varknots  <- quantile(rep(dist$temp_bin, dist$n_days_baseline), varper / 100)

cat(sprintf("Bounds: [%.0f, %.0f] | Knots: %s\\n",
            tmin, tmax, paste(round(varknots, 1), collapse = ", ")))')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 8: Build RR curves
# ═══════════════════════════════════════════════════════════════════════════════
add_code('# Build B-spline basis on a fine in-range grid
temp_grid <- seq(tmin, tmax, by = 0.1)
basis_grid <- bs(temp_grid, degree = vardegree,
                 knots = varknots, Boundary.knots = c(tmin, tmax))

# Central range for stable MMT search (25th-99th percentile)
q_range <- quantile(temp_grid, c(0.25, 0.99))
ind_cen <- temp_grid >= q_range[1] & temp_grid <= q_range[2]

# Masselot-style log-linear tail extrapolation
rr_extrap <- function(temp_eval, temp_in, rr_in, tmin, tmax) {
  o     <- order(temp_in)
  t_s   <- temp_in[o]
  lr    <- log(pmax(rr_in[o], 1e-12))
  out   <- approx(t_s, lr, xout = temp_eval, rule = 2)$y
  sl_l  <- (lr[2] - lr[1]) / (t_s[2] - t_s[1])
  sl_r  <- (lr[length(lr)] - lr[length(lr) - 1]) /
           (t_s[length(t_s)] - t_s[length(t_s) - 1])
  left  <- temp_eval < tmin
  right <- temp_eval > tmax
  out[left]  <- lr[1]          + sl_l * (temp_eval[left]  - t_s[1])
  out[right] <- lr[length(lr)] + sl_r * (temp_eval[right] - t_s[length(t_s)])
  pmax(exp(out), 1)
}

# Compute RR curve for each single-year age
n_grid    <- length(temp_grid)
rr_matrix <- matrix(NA_real_, nrow = n_grid, ncol = length(all_ages))
mmt_vec   <- numeric(length(all_ages))

for (i in seq_along(all_ages)) {
  age  <- all_ages[i]
  bvec <- as.numeric(coefs_city[agegroup == age, ..coef_cols])
  log_rr <- basis_grid %*% bvec
  mmt <- temp_grid[ind_cen][which.min(log_rr[ind_cen])]
  mmt_vec[i] <- mmt
  cen <- bs(mmt, degree = vardegree, knots = varknots,
            Boundary.knots = c(tmin, tmax)) %*% bvec
  rr  <- pmax(exp(log_rr - drop(cen)), 1)
  rr_matrix[, i] <- rr
}
names(mmt_vec) <- all_ages
colnames(rr_matrix) <- all_ages

cat(sprintf("Built RR curves for %d ages, grid length %d\\n",
            length(all_ages), n_grid))')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 9: Plot RR curves on temperature distribution
# ═══════════════════════════════════════════════════════════════════════════════
add_code('plot_ages <- c(20, 40, 60, 70, 80, 100)
temp_bins <- dist$temp_bin

# Build RR data for selected ages
df_rr <- rbindlist(lapply(plot_ages, function(a) {
  rr_vals <- rr_extrap(temp_bins, temp_grid, rr_matrix[, as.character(a)], tmin, tmax)
  data.table(temp = temp_bins, rr = rr_vals, age = paste("Age", a))
}))
df_rr[, age := factor(age, levels = paste("Age", plot_ages))]

# Histogram data (long)
df_h <- rbind(
  data.table(temp = temp_bins, prop = dist$proportion_baseline, period = "Baseline"),
  data.table(temp = temp_bins, prop = dist$prop_2050s,
             period = sprintf("RCP %.1f", rcp))
)

# Scale RR curves onto the proportion axis so RR=1 sits at y=0 (bar baseline)
max_prop <- max(df_h$prop, na.rm = TRUE)
max_rr   <- max(df_rr$rr, na.rm = TRUE)
rr_scale <- max_prop / (max_rr - 1)
df_rr[, rr_scaled := (rr - 1) * rr_scale]

ggplot() +
  geom_col(data = df_h, aes(x = temp, y = prop, fill = period),
           position = "dodge", width = 0.8, alpha = 0.4) +
  geom_line(data = df_rr, aes(x = temp, y = rr_scaled, colour = age), linewidth = 0.7) +
  scale_fill_manual(values = c("Baseline" = "steelblue",
                                setNames("tomato", sprintf("RCP %.1f", rcp)))) +
  scale_colour_viridis_d(end = 0.9) +
  scale_y_continuous(
    name = "Proportion",
    sec.axis = sec_axis(~ . / rr_scale + 1, name = "Relative Risk")
  ) +
  labs(x = "Temperature (C)",
       title = sprintf("%s - RR Curves on Temperature Distribution", city_name),
       fill = NULL, colour = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 10: Section header - Multiplier
# ═══════════════════════════════════════════════════════════════════════════════
add_md('## 4. Annual Mean RR and Mortality Multiplier

$$\\overline{RR}_{x,t}=\\sum_T H_t(T)\\,RR_x(T), \\qquad \\widetilde{M}_{x,t}=\\frac{\\overline{RR}_{x,t}}{\\overline{RR}_{x,\\text{baseline}}}$$')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 11: Compute mean RR and multipliers
# ═══════════════════════════════════════════════════════════════════════════════
add_code('ls_years <- 2019:2099

# RR evaluated at histogram bins (with tail extrapolation) for each age
rr_at_bins <- matrix(NA_real_, nrow = length(temp_bins), ncol = length(all_ages))
colnames(rr_at_bins) <- all_ages
for (i in seq_along(all_ages)) {
  rr_at_bins[, i] <- rr_extrap(temp_bins, temp_grid,
                                rr_matrix[, i], tmin, tmax)
}

# Baseline weights
w_bl <- dist$n_days_baseline
w_bl_sum <- sum(w_bl)
rr_baseline <- drop(crossprod(rr_at_bins, w_bl)) / w_bl_sum

# Mean RR matrix: rows = years, cols = ages
rr_table <- matrix(NA_real_, nrow = length(ls_years), ncol = length(all_ages),
                   dimnames = list(ls_years, all_ages))

for (yr in ls_years) {
  col_yr <- paste0("n_days_", yr)
  if (!col_yr %in% names(dist)) next
  w_yr <- dist[[col_yr]]
  s    <- sum(w_yr)
  if (s <= 0) next
  rr_table[as.character(yr), ] <- drop(crossprod(rr_at_bins, w_yr)) / s
}

# Mortality multiplier = RR_year / RR_baseline
mort_table <- sweep(rr_table, 2, rr_baseline, "/")

cat(sprintf("RR table: %d years x %d ages\\n", nrow(rr_table), ncol(rr_table)))
cat(sprintf("Multiplier range: %.4f - %.4f\\n",
            min(mort_table, na.rm = TRUE), max(mort_table, na.rm = TRUE)))')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 12: Plot mean RR by age
# ═══════════════════════════════════════════════════════════════════════════════
add_code('show_years <- c(2030, 2050, 2075, 2099)

df_rr_yr <- rbind(
  data.table(age = all_ages, rr = rr_baseline, year = "Baseline"),
  rbindlist(lapply(show_years, function(yr) {
    data.table(age = all_ages, rr = rr_table[as.character(yr), ],
               year = sprintf("RCP %.1f (%d)", rcp, yr))
  }))
)
df_rr_yr[, year := factor(year, levels = unique(year))]

ggplot(df_rr_yr, aes(x = age, y = rr, colour = year, linetype = year)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  scale_colour_manual(values = c("Baseline" = "grey40",
    setNames(c("steelblue", "goldenrod", "darkorange", "firebrick"),
             sprintf("RCP %.1f (%d)", rcp, show_years)))) +
  scale_linetype_manual(values = c("Baseline" = "dashed",
    setNames(rep("solid", 4),
             sprintf("RCP %.1f (%d)", rcp, show_years)))) +
  labs(x = "Age", y = "Average RR",
       title = sprintf("%s - Mean RR by Age", city_name),
       colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 13: Plot multiplier across years
# ═══════════════════════════════════════════════════════════════════════════════
add_code('show_ages <- c(40, 60, 80)
yrs <- as.integer(rownames(mort_table))

df_mult <- rbindlist(lapply(show_ages, function(a) {
  data.table(year = yrs, multiplier = mort_table[, as.character(a)],
             age = paste("Age", a))
}))
df_mult[, age := factor(age, levels = paste("Age", show_ages))]

ggplot(df_mult, aes(x = year, y = multiplier, colour = age)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_colour_manual(values = c("Age 40" = "steelblue",
                                  "Age 60" = "darkorange",
                                  "Age 80" = "firebrick")) +
  labs(x = "Year", y = "Mortality Multiplier",
       title = sprintf("%s - Mortality Multiplier", city_name),
       colour = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 14: Section header - Life table
# ═══════════════════════════════════════════════════════════════════════════════
add_md('## 5. Cohort Life Table

$$m^{\\text{clim}}_{x,t} = m^{\\text{ref}}_{x,t}\\,\\widetilde{M}_{x,t}, \\qquad q^{\\text{clim}}_{x,t} = \\frac{m^{\\text{clim}}_{x,t}}{1 + (1-a_x)\\,m^{\\text{clim}}_{x,t}}$$')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 15: Load mortality projections
# ═══════════════════════════════════════════════════════════════════════════════
add_code('mort_proj <- fread(file.path(BASE, sprintf("%s_mortality_projections.csv", city_lower)))
mort_proj <- mort_proj[age >= 20]
setkey(mort_proj, year, age)

cat(sprintf("Eurostat projections: %d records, years %d-%d, ages %d-%d\\n",
            nrow(mort_proj), min(mort_proj$year), max(mort_proj$year),
            min(mort_proj$age), max(mort_proj$age)))')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 16: Build cohort life table
# ═══════════════════════════════════════════════════════════════════════════════
add_code('mx_to_qx <- function(mx, ax) mx / (1 + (1 - ax) * mx)

build_cohort_lt <- function(mort_proj, mort_table, radix = 1e5) {
  cohort_years <- cohort_start_year:cohort_end_year
  cohort_ages  <- cohort_start_age + (cohort_years - cohort_start_year)

  lt <- data.table(year = cohort_years, age = cohort_ages)

  # Merge baseline mortality
  lt <- merge(lt, mort_proj[, .(year, age, qx, mx, ax)],
              by = c("year", "age"), all.x = TRUE)
  setnames(lt, c("qx", "mx"), c("qx_base", "mx_base"))

  # Fallback for years beyond projection horizon
  max_yr <- max(mort_proj$year)
  if (any(is.na(lt$qx_base))) {
    fb <- mort_proj[year == max_yr, .(age, qx_f = qx, mx_f = mx, ax_f = ax)]
    lt <- merge(lt, fb, by = "age", all.x = TRUE)
    lt[is.na(qx_base), c("qx_base", "mx_base", "ax") :=
         .(qx_f, mx_f, ax_f)]
    lt[, c("qx_f", "mx_f", "ax_f") := NULL]
  }
  lt[is.na(ax), ax := 0.5]

  # Merge climate multiplier
  lt[, multiplier := mort_table[
    cbind(as.character(year), as.character(age))
  ]]
  lt[is.na(multiplier), multiplier := 1]

  # Climate-adjusted mortality
  lt[, mx_clim := mx_base * multiplier]
  lt[, qx_clim := pmin(mx_to_qx(mx_clim, ax), 1)]
  lt[, qx_base := pmin(qx_base, 1)]

  # Survivorship
  setorder(lt, year)
  n <- nrow(lt)
  lx_b <- lx_c <- numeric(n)
  lx_b[1] <- lx_c[1] <- radix
  for (i in 2:n) {
    lx_b[i] <- lx_b[i - 1] * (1 - lt$qx_base[i - 1])
    lx_c[i] <- lx_c[i - 1] * (1 - lt$qx_clim[i - 1])
  }
  lt[, c("lx_base", "dx_base", "lx_clim", "dx_clim") :=
       .(lx_b, lx_b * qx_base, lx_c, lx_c * qx_clim)]
  lt
}

lt <- build_cohort_lt(mort_proj, mort_table)
cat(sprintf("Cohort life table: %d rows, years %d-%d, ages %d-%d\\n",
            nrow(lt), min(lt$year), max(lt$year), min(lt$age), max(lt$age)))
head(lt)')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 17: Section header - EPV
# ═══════════════════════════════════════════════════════════════════════════════
add_md('## 6. Actuarial Quantities (EPV)

**Deferred term annuity-due** (payments ages 65-84):
$${}_{45|20}\\ddot{a}_{20} = \\sum_{k=45}^{64} v^k\\;{}_kp_{20}$$

**Whole-life insurance:**
$$A_{20} = \\sum_{k \\ge 0} v^{k+1}\\;{}_kp_{20}\\;q_{20+k}$$')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 18: Compute EPVs
# ═══════════════════════════════════════════════════════════════════════════════
add_code('kpx_from_qx <- function(qx) {
  qx <- pmin(pmax(qx, 0), 1)
  c(1, cumprod(1 - qx[-length(qx)]))
}

epv_annuity <- function(lt, qx_col) {
  qx  <- lt[[qx_col]]
  kpx <- kpx_from_qx(qx)
  n   <- length(qx)
  k_start <- defer
  k_end   <- min(defer + term - 1, n - 1)
  k <- k_start:k_end
  sum(v^k * kpx[k + 1])
}

epv_insurance <- function(lt, qx_col) {
  qx  <- lt[[qx_col]]
  kpx <- kpx_from_qx(qx)
  n   <- length(qx)
  k   <- 0:(n - 1)
  sum(v^(k + 1) * kpx * qx)
}

ann_base <- epv_annuity(lt, "qx_base")
ann_clim <- epv_annuity(lt, "qx_clim")
ins_base <- epv_insurance(lt, "qx_base")
ins_clim <- epv_insurance(lt, "qx_clim")

epv_summary <- data.table(
  rcp            = sprintf("RCP %.1f", rcp),
  i              = interest_rate,
  annuity_base   = round(ann_base, 4),
  annuity_clim   = round(ann_clim, 4),
  pct_annuity    = round(100 * (ann_clim - ann_base) / ann_base, 4),
  insurance_base = round(ins_base, 5),
  insurance_clim = round(ins_clim, 5),
  pct_insurance  = round(100 * (ins_clim - ins_base) / ins_base, 4)
)

cat(sprintf("\\nEPV Summary - %s under RCP %.1f\\n\\n", city_name, rcp))
print(epv_summary)')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 19: Section header - Financial impact table
# ═══════════════════════════════════════════════════════════════════════════════
add_md('## 7. Financial Impact Table (all RCPs, LaTeX Table 2)

Pre-computed results covering all 3 RCPs x 3 adaptation scenarios.')

# ═══════════════════════════════════════════════════════════════════════════════
# CELL 20: Financial impact table
# ═══════════════════════════════════════════════════════════════════════════════
add_code('fi <- fread(file.path(BASE, sprintf("financial_impact_summary_%s.csv", city_lower)))

tbl <- fi[, .(
  RCP        = rcp,
  Adaptation = adaptation,
  Annuity    = sprintf("%+.2f%%", pct_delta_annuity),
  Insurance  = sprintf("%+.2f%%", pct_delta_insurance)
)]

cat(sprintf("Financial Impact - %s (all scenarios)\\n\\n", city_name))
print(tbl)')

# ═══════════════════════════════════════════════════════════════════════════════
# Build notebook object and write
# ═══════════════════════════════════════════════════════════════════════════════

# Fix source: each line should end with \n except last
for (i in seq_along(cells)) {
  src <- cells[[i]]$source
  if (length(src) > 1) {
    src[-length(src)] <- paste0(src[-length(src)], "\n")
  }
  cells[[i]]$source <- src
}

nb <- list(
  cells = cells,
  metadata = list(
    kernelspec = list(
      display_name = "R",
      language = "R",
      name = "ir"
    ),
    language_info = list(
      name = "R"
    )
  ),
  nbformat = 4L,
  nbformat_minor = 4L
)

json <- toJSON(nb, auto_unbox = TRUE, pretty = TRUE, null = "null")
writeLines(json, "demo_notebook/demo_r.ipynb")
cat("Wrote demo_notebook/demo_r.ipynb\n")
