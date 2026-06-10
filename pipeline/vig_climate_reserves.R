################################################################################
#
# VIG CLIMATE RISK ANALYSIS
# Impact of RCP 7.0 on Life Insurance and Annuity Reserves
# Austria & Romania — Vienna Insurance Group
#
# Methodology
# -----------
# 1. Build cohort life tables for multiple entry ages (30, 40, 50, 60, 65)
#    using Eurostat projected qx (Vienna / Bucharest as country proxies)
#    and applying the RCP 7.0 country-level mortality multiplier.
#
# 2. Compute actuarial EPVs for each product × country × entry age:
#      äx  = whole-life annuity-due (pays 1 at BOY while alive)
#      Ax  = whole-life insurance   (pays 1 at EOY of death)
#      v   = 1/(1+i),  i = 2%
#
# 3. Scale % changes to VIG's illustrative country-level life reserves.
#
# Outputs
# -------
#   results_csv/vig_epv_summary.csv   — table of EPVs and % changes
#   plots/vig_01_multiplier.png       — multiplier time-series
#   plots/vig_02_epv_pct_change.png   — % ΔEPVs by product × entry age
#   plots/vig_03_reserve_impact.png   — absolute reserve impact waterfall
#   plots/vig_04_heatmap.png          — heatmap: Δäx% by age × year
#   plots/vig_05_dashboard.png        — FT editorial one-pager
#
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(showtext)
  library(tidyr)
  library(dplyr)
})

# ── Fonts ─────────────────────────────────────────────────────────────────────

font_add("Montserrat",
         regular    = "fonts/Montserrat-Regular.ttf",
         bold       = "fonts/Montserrat-Bold.ttf",
         italic     = "fonts/Montserrat-Italic.ttf",
         bolditalic = "fonts/Montserrat-BoldItalic.ttf")
showtext_auto()
showtext_opts(dpi = 300)

# ── FT colour palette ─────────────────────────────────────────────────────────
FT_BG      <- "#FFF1E5"   # FT page background
FT_BORDER  <- "#C9C4BF"   # axes / light lines
FT_TEXT    <- "#33302E"   # primary text
FT_SUBTEXT <- "#66605A"   # secondary text / captions
FT_RED     <- "#990F3D"   # Romania / insurance product
FT_BLUE    <- "#0F5499"   # Austria / annuity product
FT_TEAL    <- "#0D7680"   # accent
FT_AMBER   <- "#F2A900"   # highlight
FT_PINK    <- "#BF5747"   # warm highlight

COUNTRY_COLORS <- c("Austria" = FT_BLUE, "Romania" = FT_RED)
PRODUCT_COLORS <- c("Annuity (äx)" = FT_TEAL, "Insurance (Ax)" = FT_PINK)

# ── Theme ─────────────────────────────────────────────────────────────────────
theme_ft <- function(base_size = 11, bg = FT_BG) {
  theme_minimal(base_size = base_size, base_family = "Montserrat") +
    theme(
      plot.background   = element_rect(fill = bg, color = NA),
      panel.background  = element_rect(fill = bg, color = NA),
      panel.grid.major  = element_line(color = FT_BORDER, linewidth = 0.35),
      panel.grid.minor  = element_blank(),
      axis.line.x       = element_line(color = FT_BORDER, linewidth = 0.5),
      axis.ticks        = element_line(color = FT_BORDER),
      text              = element_text(color = FT_TEXT),
      plot.title        = element_text(face = "bold", size = base_size * 1.4,
                                       color = FT_TEXT, margin = margin(b = 4)),
      plot.subtitle     = element_text(size = base_size * 0.9, color = FT_SUBTEXT,
                                       lineheight = 1.3, margin = margin(b = 10)),
      plot.caption      = element_text(size = base_size * 0.72, color = FT_SUBTEXT,
                                       hjust = 0, margin = margin(t = 8)),
      axis.title        = element_text(face = "bold", size = base_size * 0.85,
                                       color = FT_TEXT),
      axis.text         = element_text(size = base_size * 0.82, color = FT_TEXT),
      legend.background = element_rect(fill = bg, color = NA),
      legend.key        = element_rect(fill = bg, color = NA),
      legend.title      = element_text(face = "bold", size = base_size * 0.85),
      legend.text       = element_text(size = base_size * 0.82),
      strip.text        = element_text(face = "bold", size = base_size * 0.88,
                                       color = FT_TEXT),
      plot.margin       = margin(12, 16, 10, 12)
    )
}

save_fig <- function(p, name, width = 12, height = 7.5) {
  dir.create("plots", showWarnings = FALSE)
  png_path <- file.path("plots", paste0(name, ".png"))
  ggsave(png_path, p, width = width, height = height, dpi = 300,
         bg = FT_BG)
  cat(sprintf("  Saved: %s\n", png_path))
  invisible(p)
}

# ── Parameters ────────────────────────────────────────────────────────────────

INTEREST_RATE  <- 0.02
V              <- 1 / (1 + INTEREST_RATE)
ENTRY_AGES     <- c(30, 40, 50, 60, 65)
COHORT_YEAR    <- 2025        # cohort starts (first year multiplier available)
MAX_AGE        <- 110         # omega (limiting age; beyond this lx → 0)
TARGET_YEARS   <- c(2050, 2075, 2099)

# Illustrative VIG reserves (€ billion) — labelled as illustrative
VIG_AT_LIFE_RESERVES <- 5.2   # Austria life technical provisions (€B)
VIG_RO_LIFE_RESERVES <- 0.5   # Romania life technical provisions (€B)
ANNUITY_SHARE        <- 0.60  # fraction of life provisions in annuities
INSURANCE_SHARE      <- 0.40  # fraction in life insurance

# ── Load Data ─────────────────────────────────────────────────────────────────
cat("Loading data...\n")

# Country multiplier matrices (rows = age 20-100, cols = year 2025-2099)
mult_at <- fread("results_csv/country_multiplier_matrices/austria_rcp70.csv")
mult_ro <- fread("results_csv/country_multiplier_matrices/romania_rcp70.csv")

# City mortality projections (proxy for country-level qx)
proj_at <- fread("results_csv/mortality_projections_vienna.csv")   # Vienna → AT
proj_ro <- fread("results_csv/mortality_projections_bucharest.csv") # Bucharest → RO

cat(sprintf("  AT multiplier: ages %d-%d, years %d-%d\n",
            min(mult_at$age), max(mult_at$age),
            as.integer(names(mult_at)[2]), as.integer(tail(names(mult_at), 1))))
cat(sprintf("  AT mortality proj: ages %d-%d, years %d-%d\n",
            min(proj_at$age), max(proj_at$age),
            min(proj_at$year), max(proj_at$year)))

# ── Helper: build qx lookup ───────────────────────────────────────────────────
#  Returns a named vector qx[age] for a given year, clamping at max observed.
build_qx_lookup <- function(proj, yr_query) {
  yr_avail <- sort(unique(proj$year))
  yr_use   <- max(yr_avail[yr_avail <= yr_query], na.rm = TRUE)
  if (is.infinite(yr_use)) yr_use <- min(yr_avail)
  sub <- proj[year == yr_use, .(age, qx)]
  setNames(sub$qx, sub$age)
}

# ── Helper: get multiplier for (age, year) ────────────────────────────────────
get_multiplier <- function(mult_dt, age_vals, year_vals) {
  # mult_dt: data.table with 'age' and year columns as character
  # Returns vector of multipliers, one per (age, year) pair
  yr_cols <- as.integer(setdiff(names(mult_dt), "age"))
  yr_min <- min(yr_cols); yr_max <- max(yr_cols)

  mapply(function(a, y) {
    y_use <- min(max(y, yr_min), yr_max)
    y_col <- as.character(y_use)
    a_row <- which(mult_dt$age == a)
    if (length(a_row) == 0) return(1.0)
    mult_dt[[y_col]][a_row]
  }, age_vals, year_vals)
}

# ── Core: compute EPVs for a cohort ──────────────────────────────────────────
compute_epv <- function(proj, mult_dt, entry_age, cohort_year = 2025,
                        max_age = MAX_AGE, v = V) {

  ages  <- entry_age:max_age
  years <- cohort_year + (0:(max_age - entry_age))
  n     <- length(ages)

  # Base qx
  qx_base <- numeric(n)
  for (k in seq_len(n)) {
    qx_tbl <- build_qx_lookup(proj, years[k])
    ag <- ages[k]
    qx_base[k] <- if (!is.null(qx_tbl[as.character(ag)])) {
      val <- qx_tbl[as.character(ag)]
      if (is.na(val)) 0.999 else val
    } else 0.999
  }
  # Force qx = 1 at omega
  qx_base[n] <- 1.0

  # Climate qx
  mult_vec <- get_multiplier(mult_dt, ages, years)
  qx_clim  <- pmin(qx_base * mult_vec, 1.0)
  qx_clim[n] <- 1.0

  # Survival probabilities  k_p_x (start at 1 for k=0)
  kpx_base <- cumprod(c(1, (1 - qx_base[-n])))
  kpx_clim <- cumprod(c(1, (1 - qx_clim[-n])))

  k <- 0:(n - 1)

  # Whole-life annuity-due:  äx = Σ v^k * k_p_x
  annuity_base <- sum(v^k * kpx_base)
  annuity_clim <- sum(v^k * kpx_clim)

  # Whole-life insurance:  Ax = Σ v^(k+1) * k_p_x * q_{x+k}
  insurance_base <- sum(v^(k + 1) * kpx_base * qx_base)
  insurance_clim <- sum(v^(k + 1) * kpx_clim * qx_clim)

  list(
    annuity_base    = annuity_base,
    annuity_clim    = annuity_clim,
    insurance_base  = insurance_base,
    insurance_clim  = insurance_clim,
    pct_da          = 100 * (annuity_clim   - annuity_base)   / annuity_base,
    pct_dA          = 100 * (insurance_clim - insurance_base) / insurance_base
  )
}

# ── Core: annual EPV path (for time-series) ──────────────────────────────────
compute_epv_annual <- function(proj, mult_dt, entry_age,
                               years_seq = 2025:2099,
                               max_age = MAX_AGE, v = V) {
  # Computes EPVs for a person who enters in each year in years_seq
  # (i.e. same entry age, but the climate year changes)
  rows <- lapply(years_seq, function(yr) {
    res <- compute_epv(proj, mult_dt, entry_age, cohort_year = yr,
                       max_age = max_age, v = v)
    as.list(c(year = yr, entry_age = entry_age, unlist(res)))
  })
  rbindlist(rows)
}

# ── 1. Run EPV computations ───────────────────────────────────────────────────
cat("\nComputing EPVs...\n")

countries <- list(
  Austria = list(proj = proj_at, mult = mult_at, reserves = VIG_AT_LIFE_RESERVES),
  Romania = list(proj = proj_ro, mult = mult_ro, reserves = VIG_RO_LIFE_RESERVES)
)

epv_list <- list()
for (cname in names(countries)) {
  cat(sprintf("  Processing %s...\n", cname))
  cfg <- countries[[cname]]
  for (ea in ENTRY_AGES) {
    res <- compute_epv(cfg$proj, cfg$mult, ea, cohort_year = COHORT_YEAR)
    epv_list[[length(epv_list) + 1]] <- data.table(
      country        = cname,
      entry_age      = ea,
      annuity_base   = res$annuity_base,
      annuity_clim   = res$annuity_clim,
      insurance_base = res$insurance_base,
      insurance_clim = res$insurance_clim,
      pct_da         = res$pct_da,
      pct_dA         = res$pct_dA
    )
  }
}
epv_dt <- rbindlist(epv_list)

# ── 2. Time-series for age 50 (illustrative cohort) ──────────────────────────
cat("  Computing EPV annual time-series for age 50...\n")

ts_list <- list()
for (cname in names(countries)) {
  cfg <- countries[[cname]]
  for (ea in c(40, 65)) {
    ts <- compute_epv_annual(cfg$proj, cfg$mult, ea,
                             years_seq = seq(2025, 2095, by = 5))
    ts[, country   := cname]
    ts_list[[length(ts_list) + 1]] <- ts
  }
}
ts_dt <- rbindlist(ts_list)

# ── 3. Multiplier trajectory (direct from multiplier matrices) ────────────────
mult_long <- rbind(
  melt(mult_at, id.vars = "age", variable.name = "year_chr",
       value.name = "multiplier")[, country := "Austria"],
  melt(mult_ro, id.vars = "age", variable.name = "year_chr",
       value.name = "multiplier")[, country := "Romania"]
)
mult_long[, year := as.integer(as.character(year_chr))]
mult_long[, pct_excess := (multiplier - 1) * 100]

# Aggregate to selected age groups
mult_long[, age_group := fcase(
  age %between% c(30, 44), "Age 30–44",
  age %between% c(45, 59), "Age 45–59",
  age %between% c(60, 74), "Age 60–74",
  age >= 75,               "Age 75+"
)]

mult_agg <- mult_long[!is.na(age_group),
  .(pct_excess = mean(pct_excess)), by = .(country, year, age_group)]

# ── 4. Save summary CSV ───────────────────────────────────────────────────────
fwrite(epv_dt, "results_csv/vig_epv_summary.csv")
cat("  Saved: results_csv/vig_epv_summary.csv\n")

# ── Print headline numbers ─────────────────────────────────────────────────────
cat("\n--- Headline EPV Results (RCP 7.0, entry 2025) ---\n")
print(epv_dt[, .(country, entry_age,
                  annuity_base   = round(annuity_base,   3),
                  annuity_clim   = round(annuity_clim,   3),
                  pct_da         = round(pct_da,         3),
                  insurance_base = round(insurance_base, 4),
                  insurance_clim = round(insurance_clim, 4),
                  pct_dA         = round(pct_dA,         3))])

################################################################################
# PLOTS
################################################################################
cat("\nGenerating plots...\n")

# ── Fig 1: Multiplier trajectories ────────────────────────────────────────────
fig1_data <- mult_agg[year >= 2025]
fig1_data[, age_group := factor(age_group,
  levels = c("Age 30–44", "Age 45–59", "Age 60–74", "Age 75+"))]

p1 <- ggplot(fig1_data, aes(year, pct_excess,
                             color = country, linetype = age_group)) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_hline(yintercept = 0, color = FT_BORDER, linewidth = 0.5) +
  scale_color_manual(values = COUNTRY_COLORS, name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted"),
                        name = "Age group") +
  scale_x_continuous(breaks = seq(2025, 2099, 15)) +
  scale_y_continuous(labels = function(x) paste0("+", round(x, 1), "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Rising mortality pressure under RCP 7.0",
    subtitle = paste0(
      "Excess mortality multiplier vs baseline (1990–2019), by age group and country.\n",
      "A multiplier of +2% means 2% more deaths per year relative to historical norms."),
    x = NULL, y = "Excess mortality (%)",
    caption = paste0(
      "Source: EURO-CORDEX climate projections; Masselot et al. ERF methodology. ",
      "Country multipliers derived from population-weighted city ERF curves.\n",
      "RCP 7.0 ≡ SSP3-7.0. Baseline: 1990–2019. Austria proxy: Vienna; Romania proxy: Bucharest.")
  ) +
  theme_ft() +
  theme(legend.position = "right")

save_fig(p1, "vig_01_multiplier", width = 12, height = 6.5)

# ── Fig 2: % change in äx and Ax by entry age ─────────────────────────────────
epv_long <- melt(epv_dt,
  id.vars = c("country", "entry_age"),
  measure.vars = c("pct_da", "pct_dA"),
  variable.name = "product", value.name = "pct_change")

epv_long[, product_label := fifelse(product == "pct_da",
                                     "Annuity (äx)", "Insurance (Ax)")]
epv_long[, entry_age_f := factor(entry_age)]
epv_long[, direction   := fifelse(pct_change > 0, "increase", "decrease")]

p2 <- ggplot(epv_long, aes(x = entry_age_f, y = pct_change,
                            fill = country)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65,
           alpha = 0.9) +
  geom_hline(yintercept = 0, color = FT_TEXT, linewidth = 0.5) +
  facet_wrap(~product_label, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = COUNTRY_COLORS, name = NULL) +
  scale_y_continuous(labels = function(x) sprintf("%+.2f%%", x)) +
  labs(
    title    = "Climate change alters the cost of life products",
    subtitle = paste0(
      "Percentage change in expected present value (EPV) under RCP 7.0 vs baseline,\n",
      "for a whole-life annuity (äx) and whole-life insurance (Ax). Discount rate i = 2%. Cohort entry: 2025."),
    x = "Entry age", y = "EPV change (%)",
    caption = paste0(
      "Higher mortality raises insurance payouts (positive ΔAx) but reduces annuity ",
      "liabilities (negative Δäx) for insurers.\n",
      "Illustrative product: single-premium whole-life. Values based on projected national life tables.")
  ) +
  theme_ft() +
  theme(legend.position = "top",
        panel.spacing = unit(1.2, "lines"))

save_fig(p2, "vig_02_epv_pct_change", width = 12, height = 7)

# ── Fig 3: Reserve impact waterfall ──────────────────────────────────────────
# Compute absolute reserve impact scaled to VIG portfolio
reserve_dt <- epv_dt[entry_age == 50]   # representative age 50

reserve_impact <- data.table(
  country    = c("Austria", "Austria", "Romania", "Romania"),
  product    = c("Annuity (äx)", "Insurance (Ax)", "Annuity (äx)", "Insurance (Ax)"),
  reserves_b = c(
    VIG_AT_LIFE_RESERVES * ANNUITY_SHARE,
    VIG_AT_LIFE_RESERVES * INSURANCE_SHARE,
    VIG_RO_LIFE_RESERVES * ANNUITY_SHARE,
    VIG_RO_LIFE_RESERVES * INSURANCE_SHARE
  )
)

reserve_impact <- merge(reserve_impact, reserve_dt[, .(country, pct_da, pct_dA)],
                        by = "country")
reserve_impact[, pct_chg := fifelse(product == "Annuity (äx)", pct_da, pct_dA)]
reserve_impact[, delta_b  := reserves_b * pct_chg / 100]
reserve_impact[, label     := sprintf("%+.0fM€", delta_b * 1000)]
reserve_impact[, product   := factor(product,
  levels = c("Annuity (äx)", "Insurance (Ax)"))]

# Aggregate for headline numbers
headline <- reserve_impact[, .(
  delta_annuity    = sum(delta_b[product == "Annuity (äx)"]),
  delta_insurance  = sum(delta_b[product == "Insurance (Ax)"])
), by = country]
headline[, net_delta := delta_annuity + delta_insurance]
headline[, label := sprintf("Net: %+.0fM€", net_delta * 1000)]

p3 <- ggplot(reserve_impact,
             aes(x = product, y = delta_b * 1000,
                 fill = country)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.60,
           alpha = 0.9) +
  geom_hline(yintercept = 0, color = FT_TEXT, linewidth = 0.5) +
  geom_text(aes(label = label,
                y = delta_b * 1000 + sign(delta_b) * 1.5),
            position = position_dodge(width = 0.7),
            family = "Montserrat", size = 3, fontface = "bold",
            color = FT_TEXT) +
  scale_fill_manual(values = COUNTRY_COLORS, name = NULL) +
  scale_y_continuous(labels = function(x) sprintf("%+.0fM€", x)) +
  labs(
    title    = "Reserve shock from a 3°C warming scenario",
    subtitle = paste0(
      "Estimated change in VIG's life technical provisions (€ millions) under RCP 7.0 by 2099,\n",
      "for a representative insured aged 50 entering in 2025. Illustrative portfolio sizes."),
    x = NULL, y = "Reserve change (€M)",
    caption = paste0(
      "Illustrative VIG portfolio assumptions: Austria €5.2B life reserves (60% annuity / 40% insurance); ",
      "Romania €0.5B (same split).\n",
      "Actual impact depends on portfolio mix, policy terms, and reinsurance structure.")
  ) +
  theme_ft() +
  theme(legend.position = "top")

save_fig(p3, "vig_03_reserve_impact", width = 11, height = 7)

# ── Fig 4: Heat-map of Δäx% across ages × cohort entry years ──────────────────
cat("  Computing EPV grid for heatmap...\n")

heatmap_ages  <- c(30, 35, 40, 45, 50, 55, 60, 65, 70)
heatmap_years <- c(2025, 2035, 2045, 2055, 2065, 2075, 2085, 2095)

hm_list <- list()
for (cname in names(countries)) {
  cfg <- countries[[cname]]
  for (ea in heatmap_ages) {
    for (yr in heatmap_years) {
      # Only compute if entry year ≤ 2099 and age ≤ MAX_AGE
      if (yr + (MAX_AGE - ea) > 2099 || ea > 100) next
      res <- compute_epv(cfg$proj, cfg$mult, ea, cohort_year = yr)
      hm_list[[length(hm_list) + 1]] <- data.table(
        country   = cname,
        entry_age = ea,
        year      = yr,
        pct_da    = res$pct_da,
        pct_dA    = res$pct_dA
      )
    }
  }
}
hm_dt <- rbindlist(hm_list)
hm_dt[, entry_age_f := factor(entry_age)]
hm_dt[, year_f      := factor(year)]

p4_at <- ggplot(hm_dt[country == "Austria"],
                aes(year_f, entry_age_f, fill = pct_da)) +
  geom_tile(color = FT_BG, linewidth = 0.5) +
  geom_text(aes(label = sprintf("%+.2f", pct_da)),
            family = "Montserrat", size = 2.8, color = FT_TEXT) +
  scale_fill_gradient2(low = FT_BLUE, mid = FT_BG, high = FT_RED,
                       midpoint = 0, name = "Δäx (%)") +
  labs(title = "Austria", x = "Cohort entry year", y = "Entry age") +
  theme_ft(base_size = 10) + theme(legend.position = "right")

p4_ro <- ggplot(hm_dt[country == "Romania"],
                aes(year_f, entry_age_f, fill = pct_da)) +
  geom_tile(color = FT_BG, linewidth = 0.5) +
  geom_text(aes(label = sprintf("%+.2f", pct_da)),
            family = "Montserrat", size = 2.8, color = FT_TEXT) +
  scale_fill_gradient2(low = FT_BLUE, mid = FT_BG, high = FT_RED,
                       midpoint = 0, name = "Δäx (%)") +
  labs(title = "Romania", x = "Cohort entry year", y = "Entry age") +
  theme_ft(base_size = 10) + theme(legend.position = "right")

p4 <- (p4_at / p4_ro) +
  plot_annotation(
    title    = "Annuity liability (äx) shrinks as climate warms — but non-uniformly",
    subtitle = paste0(
      "Percentage change in whole-life annuity EPV (RCP 7.0 vs baseline) by entry age and cohort start year.\n",
      "Negative values = lower annuity liability for insurer; positive = higher. Discount rate i = 2%."),
    caption  = paste0(
      "Cells show % change in äx. Blue = liability reduction (favourable for annuity provider); ",
      "Red = liability increase.\n",
      "Cohort enters in the given year at the given age and is followed until age 110."),
    theme    = theme_ft()
  )

save_fig(p4, "vig_04_heatmap_annuity", width = 12, height = 9)

# ── Fig 5: FT editorial one-pager dashboard ───────────────────────────────────
# Top row: multiplier (AT+RO, age 65) | Key numbers panel
# Bottom row: EPV % change bars

# Key statistics box
stats_at_50 <- epv_dt[country == "Austria" & entry_age == 50]
stats_ro_50 <- epv_dt[country == "Romania"  & entry_age == 50]
stats_at_65 <- epv_dt[country == "Austria"  & entry_age == 65]
stats_ro_65 <- epv_dt[country == "Romania"  & entry_age == 65]

# Multiplier at 2099, age 65
mult_2099_at65 <- mult_at[age == 65, `2099`]
mult_2099_ro65 <- mult_ro[age == 65, `2099`]

stats_text <- data.table(
  metric  = c(
    "Mortality multiplier\nat age 65 (2099)",
    "Δäx (age 50 entry,\n2025 cohort)",
    "ΔAx (age 50 entry,\n2025 cohort)",
    "Net reserve shock\n(illustrative, €M)"
  ),
  austria = c(
    sprintf("+%.1f%%", (mult_2099_at65 - 1) * 100),
    sprintf("%+.3f%%", stats_at_50$pct_da),
    sprintf("%+.3f%%", stats_at_50$pct_dA),
    sprintf("%+.0f", (stats_at_50$pct_da / 100 * VIG_AT_LIFE_RESERVES * ANNUITY_SHARE +
                      stats_at_50$pct_dA / 100 * VIG_AT_LIFE_RESERVES * INSURANCE_SHARE) * 1000)
  ),
  romania = c(
    sprintf("+%.1f%%", (mult_2099_ro65 - 1) * 100),
    sprintf("%+.3f%%", stats_ro_50$pct_da),
    sprintf("%+.3f%%", stats_ro_50$pct_dA),
    sprintf("%+.0f", (stats_ro_50$pct_da / 100 * VIG_RO_LIFE_RESERVES * ANNUITY_SHARE +
                      stats_ro_50$pct_dA / 100 * VIG_RO_LIFE_RESERVES * INSURANCE_SHARE) * 1000)
  )
)

p_stats <- ggplot(stats_text) +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0.5, ymax = 4.5,
           fill = FT_BLUE, alpha = 0.08) +
  annotate("rect", xmin = 1.5, xmax = 2.5, ymin = 0.5, ymax = 4.5,
           fill = FT_RED, alpha = 0.08) +
  annotate("text", x = 1, y = 4.75, label = "Austria",
           hjust = 0.5, fontface = "bold", family = "Montserrat",
           color = FT_BLUE, size = 4.5) +
  annotate("text", x = 2, y = 4.75, label = "Romania",
           hjust = 0.5, fontface = "bold", family = "Montserrat",
           color = FT_RED, size = 4.5) +
  geom_text(data = stats_text,
            aes(x = 1, y = 5 - as.numeric(factor(metric)),
                label = austria),
            hjust = 0.5, size = 4.5, fontface = "bold",
            family = "Montserrat", color = FT_BLUE) +
  geom_text(data = stats_text,
            aes(x = 2, y = 5 - as.numeric(factor(metric)),
                label = romania),
            hjust = 0.5, size = 4.5, fontface = "bold",
            family = "Montserrat", color = FT_RED) +
  geom_text(data = stats_text,
            aes(x = 0, y = 5 - as.numeric(factor(metric)),
                label = metric),
            hjust = 0, size = 3.5, family = "Montserrat", color = FT_SUBTEXT) +
  geom_hline(yintercept = c(1.5, 2.5, 3.5),
             color = FT_BORDER, linewidth = 0.3, linetype = "dashed") +
  scale_x_continuous(limits = c(-0.1, 2.6)) +
  scale_y_continuous(limits = c(0.2, 5.2)) +
  labs(title = "Key metrics at a glance", subtitle = "RCP 7.0, entry age 50, 2025 cohort") +
  theme_ft() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank(), axis.line = element_blank(),
        axis.ticks = element_blank())

# Multiplier panel (age 65 only, for clarity)
p_mult_dash <- ggplot(mult_long[age == 65 & year >= 2025],
                      aes(year, pct_excess, color = country)) +
  geom_line(linewidth = 1.1, alpha = 0.9) +
  geom_hline(yintercept = 0, color = FT_BORDER, linewidth = 0.4) +
  scale_color_manual(values = COUNTRY_COLORS, name = NULL) +
  scale_x_continuous(breaks = c(2025, 2050, 2075, 2099)) +
  scale_y_continuous(labels = function(x) paste0("+", round(x, 1), "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Mortality multiplier (age 65)", x = NULL,
       y = "Excess mortality (%)") +
  theme_ft(base_size = 10) +
  theme(legend.position = "top", legend.key.width = unit(1.5, "lines"))

# Combined EPV bars for dashboard
p_bars_dash <- ggplot(epv_long,
                      aes(x = entry_age_f, y = pct_change, fill = country)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.9) +
  geom_hline(yintercept = 0, color = FT_TEXT, linewidth = 0.4) +
  facet_wrap(~product_label, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = COUNTRY_COLORS, name = NULL) +
  scale_y_continuous(labels = function(x) sprintf("%+.2f%%", x)) +
  labs(title = "EPV change by entry age", x = "Entry age", y = "% change") +
  theme_ft(base_size = 10) +
  theme(legend.position = "none", strip.text = element_text(size = 9))

# Final dashboard
dash <- (p_mult_dash | p_stats) / p_bars_dash +
  plot_layout(heights = c(1.2, 1)) +
  plot_annotation(
    title   = "VIG Climate Risk Dashboard — RCP 7.0 Scenario",
    subtitle = paste0(
      "Impact of climate-driven mortality change on life annuity (äx) and ",
      "whole-life insurance (Ax) expected present values.\n",
      "Austria and Romania. Discount rate i = 2%. Cohort entry year: 2025."),
    caption = paste0(
      "Source: EURO-CORDEX / ISIMIP3 climate projections; Masselot et al. exposure-response methodology. ",
      "Life table proxies: Vienna (AT), Bucharest (RO).\n",
      "Illustrative reserve figures. Not for commercial use. Prepared for analytical purposes only."),
    theme   = theme_ft(base_size = 12)
  )

save_fig(dash, "vig_05_dashboard", width = 14, height = 10)

cat("\nAll figures saved to plots/vig_*.png\n")
cat("Summary table saved to results_csv/vig_epv_summary.csv\n")

# ── Final console summary ──────────────────────────────────────────────────────
cat("\n", strrep("=", 70), "\n")
cat("VIG CLIMATE RISK SUMMARY — RCP 7.0 vs Baseline\n")
cat(strrep("=", 70), "\n\n")

for (cn in c("Austria", "Romania")) {
  cfg_r <- countries[[cn]]
  cat(sprintf("  %s (proxy: %s)\n", cn,
              ifelse(cn == "Austria", "Vienna", "Bucharest")))
  cat(sprintf("    Illustrative life reserves: €%.1fB\n", cfg_r$reserves))
  cat(sprintf("    Multiplier at age 65 in 2099: +%.1f%%\n",
              (get_multiplier(cfg_r$mult, 65, 2099) - 1) * 100))
  sub <- epv_dt[country == cn]
  cat(sprintf("    Δäx range (ages %s): %+.3f%% to %+.3f%%\n",
              paste(ENTRY_AGES, collapse = "/"),
              min(sub$pct_da), max(sub$pct_da)))
  cat(sprintf("    ΔAx range (ages %s): %+.3f%% to %+.3f%%\n",
              paste(ENTRY_AGES, collapse = "/"),
              min(sub$pct_dA), max(sub$pct_dA)))
  net_50 <- with(sub[entry_age == 50],
                 (pct_da / 100 * cfg_r$reserves * ANNUITY_SHARE +
                  pct_dA / 100 * cfg_r$reserves * INSURANCE_SHARE) * 1000)
  cat(sprintf("    Net reserve impact (age-50 proxy, €M): %+.0f\n\n", net_50))
}
