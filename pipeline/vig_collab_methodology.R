################################################################################
#
# VIG CLIMATE RISK — COLLABORATOR'S METHODOLOGY WITH OUR DATA
# Bucharest (RO) & Vienna (AT) — SSP3-7.0 / RCP 7.0
#
# Division of expertise
# ---------------------
#   Data & multipliers : Eurostat EUROPOP2023 city projections +
#                        EURO-CORDEX RCP 7.0 ensemble mortality multipliers
#   Actuarial methodology: adopted from collaborator's Excel
#                          (RO001C_SSP3_0adapt_2095-Carmen.xlsx)
#
# Collaborator's methodology
# --------------------------
#   i       = 1%
#   Table   = period life table, snapshot at target_year
#   Product = whole-life insurance issued at age 30, annual net premiums
#               P = A_30 / a_30  (equivalence principle)
#   Reserve = prospective net-premium reserve at age 65:
#               35V = A_65 - P * a_65
#   Annuity = whole-life annuity-due at age 65: a_65
#   Climate = qx_cc = qx_nc * M(age, target_year)  for ages 20-100
#             qx_cc = qx_nc                          for ages  0-19
#   Scenarios:
#     nc        -- baseline (no climate)
#     cc_unadj  -- CC mortality, premium fixed at P_nc  (legacy pricing)
#     cc_adj    -- CC mortality, premium recalculated    (repriced)
#
# Outputs
# -------
#   results_csv/vig_collab_results.csv  -- EPV / reserve table
#   plots/vig_c1_epv_curves.png         -- a_x and A_x curves, ages 30-85
#   plots/vig_c2_reserve_decomp.png     -- reserve change decomposition
#
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(showtext)
})

# -- IE Science and Technology colour palette (optimised for dark backgrounds) -
IE_GREEN  <- "#6DC201"  # IE lime green  (primary brand)
IE_CYAN   <- "#00C2FF"  # IE electric blue
IE_AMBER  <- "#FFB300"  # warm amber
IE_CORAL  <- "#FF6B6B"  # coral-red
IE_TEXT   <- "#FFFFFF"  # white text (dark background)
IE_MID    <- "#AAAAAA"  # mid-grey (subtitles, captions, vlines)
IE_GRID   <- "#555555"  # subtle dark grid lines

ft_theme <- function() {
  theme_minimal(base_family = "montserrat") +
    theme(
      plot.background   = element_rect(fill = "transparent", colour = NA),
      panel.background  = element_rect(fill = "transparent", colour = NA),
      panel.grid.major  = element_line(colour = IE_GRID, linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      axis.text         = element_text(colour = IE_TEXT, size = 20),
      axis.title        = element_text(colour = IE_TEXT, size = 22, face = "bold"),
      plot.title        = element_text(colour = IE_TEXT, size = 30, face = "bold",
                                       margin = margin(b = 8)),
      plot.subtitle     = element_text(colour = IE_MID, size = 18),
      plot.caption      = element_text(colour = IE_MID, size = 14),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.text       = element_text(colour = IE_TEXT, size = 18),
      legend.title      = element_text(colour = IE_TEXT, size = 20, face = "bold"),
      strip.background  = element_rect(fill = "transparent", colour = NA),
      strip.text        = element_text(colour = IE_TEXT, size = 20, face = "bold"),
      plot.margin       = margin(15, 15, 15, 15)
    )
}

font_add_google("Montserrat", "montserrat")
showtext_auto()

# -- Parameters (from collaborator's spreadsheet) ------------------------------
i_rate      <- 0.01   # discount rate
v           <- 1 / (1 + i_rate)
omega       <- 100    # limiting age
RADIX       <- 1e6   # life-table radix
target_year <- 2095   # period life table year
mult_year   <- 2095   # multiplier matrix column to use

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Build commutation table (Dx, Nx, Cx, Mx -> a_x, A_x) from a qx vector.
# qx_vec: numeric vector of length omega+1, entry k = q_{k-1} (0-indexed age).
build_commutation <- function(qx_vec) {
  ages <- 0:omega
  n    <- length(ages)

  lx <- numeric(n);  dx <- numeric(n)
  lx[1] <- RADIX
  for (k in seq_len(n - 1)) {
    dx[k]     <- lx[k] * qx_vec[k]
    lx[k + 1] <- lx[k] - dx[k]
  }
  dx[n] <- lx[n]

  vx <- v ^ ages
  Dx <- lx * vx
  Cx <- dx * v ^ (ages + 1)
  Nx <- rev(cumsum(rev(Dx)))
  Mx <- rev(cumsum(rev(Cx)))

  data.table(age = ages, lx = lx, dx = dx,
             Dx = Dx, Nx = Nx, Cx = Cx, Mx = Mx,
             ax = Nx / Dx,   # a_x : whole-life annuity-due
             Ax = Mx / Dx)   # A_x : whole-life insurance
}

# Compute net-premium reserve metrics for one nc/cc pair.
compute_reserves <- function(ct_nc, ct_cc, label_cc = "cc") {
  epv <- function(ct, age_) ct[age == age_, .(ax, Ax)]

  nc30 <- epv(ct_nc, 30);  nc65 <- epv(ct_nc, 65)
  cc30 <- epv(ct_cc, 30);  cc65 <- epv(ct_cc, 65)

  P_nc <- nc30$Ax / nc30$ax   # equivalence principle premium (baseline)
  P_cc <- cc30$Ax / cc30$ax   # repriced premium (climate)

  data.table(
    scenario = c("nc",
                 paste0(label_cc, "_unadj"),
                 paste0(label_cc, "_adj")),
    a30 = c(nc30$ax, cc30$ax, cc30$ax),
    A30 = c(nc30$Ax, cc30$Ax, cc30$Ax),
    P   = c(P_nc,    P_nc,    P_cc),
    a65 = c(nc65$ax, cc65$ax, cc65$ax),
    A65 = c(nc65$Ax, cc65$Ax, cc65$Ax),
    V35 = c(nc65$Ax - P_nc * nc65$ax,   # 35V baseline
            cc65$Ax - P_nc * cc65$ax,   # 35V unadjusted premium
            cc65$Ax - P_cc * cc65$ax)   # 35V adjusted premium
  )
}

# Extract period qx vector (ages 0:omega) from a city projection data.table.
get_period_qx <- function(proj, yr) {
  lt <- proj[year == yr, .(age, qx)]
  setkey(lt, age)
  qx_vec <- lt[J(0:omega), qx]
  qx_vec[is.na(qx_vec)] <- 0
  qx_vec[omega + 1]     <- 1   # absorbing state at omega
  names(qx_vec)         <- as.character(0:omega)
  qx_vec
}

# Apply multiplier matrix to qx (ages 20-100 only, per collaborator protocol).
apply_multiplier <- function(qx_nc, mult_dt) {
  qx_cc <- qx_nc
  for (rw in seq_len(nrow(mult_dt))) {
    idx <- mult_dt$age[rw] + 1   # 0-indexed age -> 1-indexed position
    if (idx >= 1 && idx <= length(qx_cc))
      qx_cc[idx] <- min(qx_nc[idx] * mult_dt$mult[rw], 1)
  }
  qx_cc
}

# Cohort EPV helper: % change in äx and Ax for one entry-age cohort
compute_cohort_pct_epv <- function(proj, mult_mat, entry_age,
                                    cohort_year = 2025) {
  ages  <- entry_age:omega
  years <- cohort_year + seq_along(ages) - 1L
  n     <- length(ages)
  yr_avail <- as.integer(setdiff(names(mult_mat), "age"))

  qx_nc   <- numeric(n)
  mult_vec <- numeric(n)
  for (k in seq_len(n)) {
    ag <- ages[k];  yr <- years[k]
    row <- proj[year == yr & age == ag, qx]
    qx_nc[k] <- if (length(row) == 0 || is.na(row[1])) 0.999 else row[1]
    yr_use   <- yr_avail[which.min(abs(yr_avail - yr))]
    m_row    <- mult_mat[age == ag, get(as.character(yr_use))]
    mult_vec[k] <- if (length(m_row) == 0 || is.na(m_row[1])) 1 else m_row[1]
  }
  qx_nc[n] <- 1.0
  qx_cc    <- pmin(qx_nc * mult_vec, 1.0);  qx_cc[n] <- 1.0

  kpx_nc <- cumprod(c(1, (1 - qx_nc[-n])))
  kpx_cc <- cumprod(c(1, (1 - qx_cc[-n])))
  k_idx  <- 0:(n - 1)

  ann_nc <- sum(v^k_idx * kpx_nc)
  ann_cc <- sum(v^k_idx * kpx_cc)
  ins_nc <- sum(v^(k_idx + 1) * kpx_nc * qx_nc)
  ins_cc <- sum(v^(k_idx + 1) * kpx_cc * qx_cc)

  list(pct_da = 100 * (ann_cc - ann_nc) / ann_nc,
       pct_dA = 100 * (ins_cc - ins_nc) / ins_nc)
}

# ==============================================================================
# 1.  LOAD DATA
# ==============================================================================

proj_ro <- fread("results_csv/mortality_projections_bucharest.csv")
proj_at <- fread("results_csv/mortality_projections_vienna.csv")

mult_ro_mat <- fread("results_csv/country_multiplier_matrices/romania_rcp70.csv")
mult_at_mat <- fread("results_csv/country_multiplier_matrices/austria_rcp70.csv")
mult_ro <- mult_ro_mat[, .(age, mult = get(as.character(mult_year)))]
mult_at <- mult_at_mat[, .(age, mult = get(as.character(mult_year)))]

# ==============================================================================
# 2.  COMPUTE
# ==============================================================================

results_list <- list()
ct_list      <- list()

for (ctry in c("Romania", "Austria")) {
  proj <- if (ctry == "Romania") proj_ro else proj_at
  mult <- if (ctry == "Romania") mult_ro  else mult_at

  qx_nc <- get_period_qx(proj, target_year)
  qx_cc <- apply_multiplier(qx_nc, mult)

  ct_nc <- build_commutation(qx_nc)
  ct_cc <- build_commutation(qx_cc)

  res <- compute_reserves(ct_nc, ct_cc)
  res[, country := ctry]

  results_list[[ctry]] <- res
  ct_list[[paste0(ctry, "_nc")]] <- ct_nc
  ct_list[[paste0(ctry, "_cc")]] <- ct_cc
}

results <- rbindlist(results_list)

# ==============================================================================
# 3.  PRINT RESULTS
# ==============================================================================

cat("\n", strrep("=", 68), "\n")
cat("OUR DATA + COLLABORATOR'S METHODOLOGY  (i=1%, period table yr=2095)\n")
cat(strrep("=", 68), "\n\n")

for (ctry in c("Romania", "Austria")) {
  d  <- results[country == ctry]
  nc <- d[scenario == "nc"]
  cu <- d[scenario == "cc_unadj"]

  cat(sprintf("-- %s ---------------------------------------\n", ctry))
  cat(sprintf("  %-14s  %7s  %6s  %7s  %6s  %7s\n",
              "scenario", "a30", "A30", "a65", "A65", "35V"))
  cat(sprintf("  %-14s  %7s  %6s  %7s  %6s  %7s\n",
              strrep("-", 14), strrep("-", 7), strrep("-", 6),
              strrep("-", 7), strrep("-", 6), strrep("-", 7)))
  for (rw in seq_len(nrow(d)))
    cat(sprintf("  %-14s  %7.3f  %6.4f  %7.3f  %6.4f  %7.4f\n",
                d$scenario[rw], d$a30[rw], d$A30[rw],
                d$a65[rw], d$A65[rw], d$V35[rw]))

  cat(sprintf("\n  Climate impact (cc_unadj vs nc):\n"))
  cat(sprintf("    Delta_a65 = %+.4f  (%+.3f%%)\n",
              cu$a65 - nc$a65, 100 * (cu$a65 - nc$a65) / nc$a65))
  cat(sprintf("    Delta_35V = %+.5f\n", cu$V35 - nc$V35))
  cat(sprintf("    Per EUR100k/yr pension : EUR%+.0f\n",
              (cu$a65 - nc$a65) * 1e5))
  cat(sprintf("    Per EUR100M face value : EUR%+.0f\n\n",
              (cu$V35 - nc$V35) * 1e8))
}

# -- Divergence from collaborator (data-source gap) ----------------------------
mult_ro_65 <- mult_ro[age == 65, mult]

cat(strrep("=", 68), "\n")
cat("DIVERGENCE FROM COLLABORATOR (Romania, two data gaps)\n")
cat(strrep("=", 68), "\n\n")
cat(sprintf("  q(65) yr 2095 -- ours (EUROPOP2023):  %.5f\n",
            proj_ro[year == 2095 & age == 65, qx]))
cat(sprintf("  q(65) yr 2095 -- collaborator:        0.01177   (%.1fx higher)\n",
            0.01177 / proj_ro[year == 2095 & age == 65, qx]))
cat(sprintf("  M(65, 2095)   -- ours (EURO-CORDEX):  +%.2f%%\n",
            (mult_ro_65 - 1) * 100))
cat(sprintf("  M(65, 2095)   -- collaborator:        +5.53%%   (%.1fx higher)\n\n",
            0.0553 / (mult_ro_65 - 1)))
cat("  Both gaps inflate the collaborator's figures in the same direction.\n")
cat("  Ours = EURO-CORDEX ensemble-mean central estimate.\n")
cat("  Theirs = adverse/tail scenario appropriate for Solvency II SCR.\n\n")

# -- Save CSV ------------------------------------------------------------------
fwrite(results, "results_csv/vig_collab_results.csv")
cat("Saved results_csv/vig_collab_results.csv\n")

# ==============================================================================
# 4.  PLOTS
# ==============================================================================

# -- C1: EPV curves by age (30-85), both countries ----------------------------
epv_long <- rbindlist(list(
  ct_list[["Romania_nc"]][, .(age, ax, Ax, country = "Romania", scenario = "Baseline")],
  ct_list[["Romania_cc"]][, .(age, ax, Ax, country = "Romania", scenario = "Climate (RCP 7.0)")],
  ct_list[["Austria_nc"]][, .(age, ax, Ax, country = "Austria", scenario = "Baseline")],
  ct_list[["Austria_cc"]][, .(age, ax, Ax, country = "Austria", scenario = "Climate (RCP 7.0)")]
))

colour_map <- c(
  "Baseline.Austria"          = IE_CYAN,
  "Climate (RCP 7.0).Austria" = IE_GREEN,
  "Baseline.Romania"          = IE_AMBER,
  "Climate (RCP 7.0).Romania" = IE_CORAL
)
label_map <- c(
  "Baseline.Austria"          = "Austria - Baseline",
  "Climate (RCP 7.0).Austria" = "Austria - Climate",
  "Baseline.Romania"          = "Romania - Baseline",
  "Climate (RCP 7.0).Romania" = "Romania - Climate"
)

age_range <- epv_long[age >= 30 & age <= 85]
vline_lbl <- max(age_range$ax) * 0.92

p_ann <- ggplot(age_range,
    aes(x = age, y = ax,
        colour   = interaction(scenario, country),
        linetype = country)) +
  geom_line(linewidth = 1.5) +
  geom_vline(xintercept = 65, linetype = "dashed", colour = IE_MID, linewidth = 0.4) +
  annotate("text", x = 66, y = vline_lbl,
           label = "Age 65", colour = IE_MID, size = 6, family = "montserrat", hjust = 0) +
  scale_colour_manual(values = colour_map, labels = label_map, name = NULL) +
  scale_linetype_manual(values = c("Austria" = "solid", "Romania" = "dashed"),
                        guide = "none") +
  labs(title    = "Whole-life annuity-due",
       subtitle = sprintf("Period table yr %d | i = %.0f%% | RCP 7.0 multiplier yr %d",
                          target_year, i_rate * 100, mult_year),
       x = "Age", y = "a_x (per EUR 1/year pension)",
       caption = "Eurostat EUROPOP2023 city projections + EURO-CORDEX RCP 7.0") +
  ft_theme()

p_ins <- ggplot(age_range,
    aes(x = age, y = Ax,
        colour   = interaction(scenario, country),
        linetype = country)) +
  geom_line(linewidth = 1.5) +
  geom_vline(xintercept = 65, linetype = "dashed", colour = IE_MID, linewidth = 0.4) +
  scale_colour_manual(values = colour_map, labels = label_map, name = NULL) +
  scale_linetype_manual(values = c("Austria" = "solid", "Romania" = "dashed"),
                        guide = "none") +
  labs(title    = "Whole-life insurance",
       subtitle = sprintf("Period table yr %d | i = %.0f%% | RCP 7.0 multiplier yr %d",
                          target_year, i_rate * 100, mult_year),
       x = "Age", y = "A_x (per EUR 1 sum assured)",
       caption = "Eurostat EUROPOP2023 city projections + EURO-CORDEX RCP 7.0") +
  ft_theme()

p_c1 <- (p_ann / p_ins) +
  plot_annotation(
    title = "Actuarial EPVs under Climate Change",
    subtitle = "Our data (Eurostat + EURO-CORDEX) with collaborator's actuarial framework",
    theme = theme(plot.background = element_rect(fill = "transparent", colour = NA),
                  plot.title    = element_text(family = "montserrat", colour = IE_TEXT,
                                               size = 32, face = "bold",
                                               margin = margin(b = 8)),
                  plot.subtitle = element_text(family = "montserrat",
                                               colour = IE_MID, size = 18))
  )

ggsave("plots/vig_c1_epv_curves.png", p_c1, width = 10, height = 10, dpi = 200, bg = "transparent")
cat("Saved plots/vig_c1_epv_curves.png\n")

# -- C2: Reserve change decomposition at age 65 --------------------------------
decomp <- rbindlist(lapply(c("Romania", "Austria"), function(ctry) {
  d   <- results[country == ctry]
  nc  <- d[scenario == "nc"]
  cu  <- d[scenario == "cc_unadj"]
  caj <- d[scenario == "cc_adj"]
  dA65 <- cu$A65 - nc$A65
  da65 <- cu$a65 - nc$a65
  data.table(
    country   = ctry,
    component = factor(
      c("Delta A65\n(higher mortality)", "P x (-Delta a65)\n(shorter payout)",
        "Net Delta 35V\n(unadj prem)", "Net Delta 35V\n(adj prem)"),
      levels = c("Delta A65\n(higher mortality)", "P x (-Delta a65)\n(shorter payout)",
                 "Net Delta 35V\n(unadj prem)", "Net Delta 35V\n(adj prem)")
    ),
    value    = c(dA65, -nc$P * da65, cu$V35 - nc$V35, caj$V35 - nc$V35),
    is_total = c(FALSE, FALSE, TRUE, TRUE)
  )
}))
decomp[, eur_per_M := value * 1e6]

p_c2 <- ggplot(decomp,
    aes(x = component, y = eur_per_M,
        fill = interaction(is_total, eur_per_M >= 0))) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, colour = IE_MID, linewidth = 0.5) +
  geom_text(aes(label = sprintf("%+.0f", eur_per_M),
                vjust = ifelse(eur_per_M >= 0, -0.4, 1.3)),
            size = 5, family = "montserrat", colour = IE_TEXT) +
  facet_wrap(~ country) +
  scale_fill_manual(
    values = c("FALSE.FALSE" = IE_CORAL, "FALSE.TRUE" = IE_CYAN,
               "TRUE.FALSE"  = IE_CORAL, "TRUE.TRUE"  = IE_GREEN),
    guide = "none"
  ) +
  labs(
    title    = "Reserve change per EUR 1M face value -- decomposition at age 65",
    subtitle = paste0("Whole-life insurance, issued age 30 | Period table yr ",
                      target_year, " | i = ", i_rate * 100, "%"),
    x = NULL, y = "EUR per EUR 1M face value",
    caption = "unadj: legacy premium retained | adj: premium recalculated at CC mortality"
  ) +
  ft_theme() +
  theme(axis.text.x = element_text(size = 16))

ggsave("plots/vig_c2_reserve_decomp.png", p_c2, width = 10, height = 6, dpi = 200, bg = "transparent")
cat("Saved plots/vig_c2_reserve_decomp.png\n")

# -- C3: % change in EPVs across ages (30-85) ---------------------------------
pct_dt <- rbindlist(lapply(c("Austria", "Romania"), function(ctry) {
  nc <- ct_list[[paste0(ctry, "_nc")]][age >= 30 & age <= 85]
  cc <- ct_list[[paste0(ctry, "_cc")]][age >= 30 & age <= 85]
  data.table(
    country = ctry,
    age     = nc$age,
    pct_da  = 100 * (cc$ax - nc$ax) / nc$ax,
    pct_dA  = 100 * (cc$Ax - nc$Ax) / nc$Ax
  )
}))

pct_long <- melt(pct_dt, id.vars = c("country", "age"),
                  measure.vars = c("pct_da", "pct_dA"),
                  variable.name = "product", value.name = "pct_change")
pct_long[, product_label := fifelse(product == "pct_da",
                                     "Annuity (\u00e4x)", "Insurance (Ax)")]

country_cols <- c("Austria" = IE_CYAN, "Romania" = IE_AMBER)

p_c3 <- ggplot(pct_long, aes(x = age, y = pct_change, colour = country)) +
  geom_line(linewidth = 1.5) +
  geom_hline(yintercept = 0, colour = IE_MID, linewidth = 0.6, linetype = "dashed") +
  facet_wrap(~ product_label, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = country_cols, name = NULL) +
  scale_y_continuous(labels = function(x) sprintf("%+.2f%%", x)) +
  labs(
    title    = "Climate change alters the cost of life products",
    subtitle = paste0(
      "% change in EPV under RCP 7.0 vs baseline | period table yr ",
      target_year, " | i = ", i_rate * 100, "%"),
    x = "Age", y = "EPV change (%)",
    caption = paste0(
      "Higher mortality raises insurance payouts (\u0394Ax > 0) ",
      "but reduces annuity liabilities (\u0394\u00e4x < 0).\n",
      "Eurostat EUROPOP2023 + EURO-CORDEX RCP 7.0")
  ) +
  ft_theme() +
  theme(
    legend.position = "top",
    panel.spacing   = unit(1.5, "lines")
  )

ggsave("plots/vig_c3_epv_pct_change.png", p_c3, width = 12, height = 7, dpi = 200, bg = "transparent")
cat("Saved plots/vig_c3_epv_pct_change.png\n")

# -- C4: Mortality multiplier at age 65, 2025-2100 ----------------------------
yr_cols   <- setdiff(names(mult_at_mat), "age")
mult65_dt <- rbind(
  data.table(country = "Austria",
             year    = as.integer(yr_cols),
             mult    = unlist(mult_at_mat[age == 65, yr_cols, with = FALSE])),
  data.table(country = "Romania",
             year    = as.integer(yr_cols),
             mult    = unlist(mult_ro_mat[age == 65, yr_cols, with = FALSE]))
)
mult65_dt <- mult65_dt[year >= 2025]
mult65_dt[, pct_excess := (mult - 1) * 100]

country_cols <- c("Austria" = IE_CYAN, "Romania" = IE_AMBER)

p_c4 <- ggplot(mult65_dt, aes(x = year, y = pct_excess, colour = country)) +
  geom_line(linewidth = 1.8) +
  geom_hline(yintercept = 0, colour = IE_MID, linewidth = 0.5, linetype = "dashed") +
  scale_colour_manual(values = country_cols, name = NULL) +
  scale_x_continuous(breaks = seq(2025, 2099, 15)) +
  scale_y_continuous(labels = function(x) sprintf("%+.1f%%", x)) +
  labs(
    title    = "Rising mortality pressure at age 65 under RCP 7.0",
    subtitle = "Excess mortality multiplier vs baseline (1990\u20132019) | EURO-CORDEX",
    x = NULL, y = "Excess mortality (%)",
    caption = paste0(
      "EURO-CORDEX RCP 7.0. Country multipliers from population-weighted city ERF curves.\n",
      "Austria proxy: Vienna; Romania proxy: Bucharest.")
  ) +
  ft_theme() +
  theme(legend.position = "top")

ggsave("plots/vig_c4_mult65_trajectory.png", p_c4, width = 10, height = 6, dpi = 200, bg = "transparent")
cat("Saved plots/vig_c4_mult65_trajectory.png\n")

# -- C5: EPV % change by entry age (cohort entering 2025) ---------------------
ENTRY_AGES   <- c(30, 40, 50, 60, 65, 70)
COHORT_YEARS <- c(2025, 2050, 2075)

bar_list <- list()
for (ctry in c("Austria", "Romania")) {
  proj_c <- if (ctry == "Austria") proj_at else proj_ro
  mult_c <- if (ctry == "Austria") mult_at_mat else mult_ro_mat
  for (cy in COHORT_YEARS) {
    for (ea in ENTRY_AGES) {
      res <- compute_cohort_pct_epv(proj_c, mult_c, ea, cohort_year = cy)
      bar_list[[length(bar_list) + 1]] <- data.table(
        country     = ctry,
        cohort_year = cy,
        entry_age   = ea,
        pct_da      = res$pct_da,
        pct_dA      = res$pct_dA
      )
    }
  }
}
bar_dt   <- rbindlist(bar_list)
bar_long <- melt(bar_dt, id.vars = c("country", "cohort_year", "entry_age"),
                  measure.vars = c("pct_da", "pct_dA"),
                  variable.name = "product", value.name = "pct_change")
bar_long[, product_label := fifelse(product == "pct_da",
                                     "Annuity (\u00e4x)", "Insurance (Ax)")]
bar_long[, entry_age_f   := factor(entry_age)]
bar_long[, cohort_label  := paste0("Entry: ", cohort_year)]
bar_long[, cohort_label  := factor(cohort_label,
                                    levels = paste0("Entry: ", COHORT_YEARS))]

for (cy in COHORT_YEARS) {
  dat <- bar_long[cohort_year == cy]
  p_c5 <- ggplot(dat, aes(x = entry_age_f, y = pct_change, fill = country)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.9) +
    geom_hline(yintercept = 0, colour = IE_MID, linewidth = 0.5) +
    facet_wrap(~ product_label, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = country_cols, name = NULL) +
    scale_y_continuous(labels = function(x) sprintf("%+.2f%%", x)) +
    labs(
      title    = "Climate change alters the cost of life products",
      subtitle = paste0(
        "% change in EPV (RCP 7.0 vs baseline) | cohort entering ", cy,
        " | i = ", i_rate * 100, "%"),
      x = "Entry age", y = "EPV change (%)",
      caption = paste0(
        "\u0394Ax > 0: climate raises insurance payouts. ",
        "\u0394\u00e4x < 0: shorter payout horizon reduces annuity cost.\n",
        "Eurostat EUROPOP2023 + EURO-CORDEX RCP 7.0")
    ) +
    ft_theme() +
    theme(
      legend.position = "top",
      panel.spacing   = unit(1.5, "lines")
    )
  fname <- sprintf("plots/vig_c5_epv_bar_%d.png", cy)
  ggsave(fname, p_c5, width = 12, height = 7, dpi = 200, bg = "transparent")
  cat(sprintf("Saved %s\n", fname))
}
