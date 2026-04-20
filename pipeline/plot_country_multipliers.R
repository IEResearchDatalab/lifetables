################################################################################
#
# Country-Level Mortality Multiplier Visualisations
#
# Produces four publication-quality figures that mirror the city-level
# plot_multiplier_evolution.R but at country scale:
#
#   Fig 1  — Europe choropleth map: total multiplier at 2050/SSP3-7.0, age 65
#   Fig 2  — Exposure-response function (ERF) curves, selected countries
#   Fig 3  — Heat vs cold trade-off scatter, all countries, 2050 SSP3
#   Fig 4  — Time-series ribbons (median + 80% CI), all SSPs, age 65-74
#
# All figures are saved as PDF + PNG under plots/.
#
# Requires (run compute_country_multipliers.R first):
#   results_csv/mortality_multiplier_by_age_country.csv
#   results_csv/multiplier_ts_country.csv
#   results_csv/erf_curves_country.csv
#   results_csv/mmt_country.csv
#   data/CNTR_RG_20M_2020_4326.geojson
#
################################################################################

library(data.table)
library(ggplot2)
library(patchwork)
library(sf)
library(scales)
library(showtext)

if (!requireNamespace("ggrepel", quietly = TRUE)) {
  install.packages("ggrepel", repos = "https://cloud.r-project.org")
}
library(ggrepel)
library(shadowtext)

# ── Brand fonts & palette ────────────────────────────────────────────────────

font_add("Montserrat",
         regular    = "fonts/Montserrat-Regular.ttf",
         bold       = "fonts/Montserrat-Bold.ttf",
         italic     = "fonts/Montserrat-Italic.ttf",
         bolditalic = "fonts/Montserrat-BoldItalic.ttf")
showtext_auto()

OceanBlue    <- "#000066"
ElectricBlue <- "#0000db"
SeaBlue      <- "#47bfff"
TechGreen    <- "#6DC201"

# Scenario palette (colourblind-safe)
ssp_palette <- c(
  "SSP1-2.6" = "#4575b4",
  "SSP2-4.5" = "#f4a582",
  "SSP3-7.0" = "#d73027"
)

# Age-group palette
age_palette <- c(
  "20-44" = "#bdd7e7",
  "45-64" = "#6baed6",
  "65-74" = "#2171b5",
  "75-84" = "#08519c",
  "85+"   = "#08306b"
)

theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "Montserrat") +
    theme(
      text              = element_text(color = OceanBlue),
      plot.title        = element_text(face = "bold", size = base_size * 1.35,
                                       color = OceanBlue, margin = margin(b = 4)),
      plot.subtitle     = element_text(size = base_size * 0.95, color = OceanBlue,
                                       margin = margin(b = 8)),
      plot.caption      = element_text(hjust = 0, size = base_size * 0.72,
                                       color = "#666666", margin = margin(t = 6)),
      axis.title        = element_text(face = "bold", color = OceanBlue,
                                       size = base_size * 0.88),
      axis.text         = element_text(color = OceanBlue, size = base_size * 0.82),
      legend.title      = element_text(face = "bold", color = OceanBlue,
                                       size = base_size * 0.88),
      legend.text       = element_text(color = OceanBlue, size = base_size * 0.82),
      panel.grid.major  = element_line(color = "#e8e8e8", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      legend.position   = "bottom",
      strip.text        = element_text(face = "bold", color = OceanBlue,
                                       size = base_size * 0.88)
    )
}

save_fig <- function(p, name, width = 10, height = 7) {
  path_pdf <- file.path("plots", paste0(name, ".pdf"))
  path_png <- file.path("plots", paste0(name, ".png"))
  ggsave(path_pdf, p, width = width, height = height, device = cairo_pdf)
  ggsave(path_png, p, width = width, height = height, dpi = 300)
  cat(sprintf("  Saved: %s (.pdf + .png)\n", name))
  invisible(p)
}

if (!dir.exists("plots")) dir.create("plots")

# ── Load pre-computed results ────────────────────────────────────────────────

cat("Loading pre-computed results...\n")

results <- fread("results_csv/mortality_multiplier_by_age_country.csv")
ts_gcm  <- fread("results_csv/multiplier_ts_country.csv")
erf_raw <- fread("results_csv/erf_curves_country.csv")
mmt_dt  <- fread("results_csv/mmt_country.csv")

ssp_labels <- c("1" = "SSP1-2.6", "2" = "SSP2-4.5", "3" = "SSP3-7.0")
results[,    ssp_label := ssp_labels[ssp]]
ts_gcm[,     ssp_label := ssp_labels[ssp]]

cat(sprintf("  Multiplier rows: %s\n", format(nrow(results), big.mark = ",")))
cat(sprintf("  Time-series rows: %s\n", format(nrow(ts_gcm),  big.mark = ",")))

# ── Country name lookup ───────────────────────────────────────────────────────

country_names <- c(
  AT = "Austria",    BE = "Belgium",    BG = "Bulgaria",   CH = "Switzerland",
  CY = "Cyprus",     CZ = "Czechia",    DE = "Germany",    DK = "Denmark",
  EE = "Estonia",    EL = "Greece",     ES = "Spain",      FI = "Finland",
  FR = "France",     HR = "Croatia",    HU = "Hungary",    IE = "Ireland",
  IT = "Italy",      LT = "Lithuania",  LU = "Luxembourg", LV = "Latvia",
  MT = "Malta",      NL = "Netherlands",NO = "Norway",     PL = "Poland",
  PT = "Portugal",   RO = "Romania",    SE = "Sweden",     SI = "Slovenia",
  SK = "Slovakia",   UK = "United Kingdom"
)

results[,  country_name := country_names[country_code]]
ts_gcm[,   country_name := country_names[country_code]]
mmt_dt[,   country_name := country_names[country_code]]

# ── Summary stats helper ──────────────────────────────────────────────────────

ts_summary <- ts_gcm[, .(
  median = median(multiplier),
  q10    = quantile(multiplier, 0.10),
  q90    = quantile(multiplier, 0.90),
  q25    = quantile(multiplier, 0.25),
  q75    = quantile(multiplier, 0.75)
), by = .(country_code, country_name, ssp, ssp_label, year)]

################################################################################
# Figure 1 — Europe Choropleth Map (2050, 2075, 2099)
################################################################################

cat("\nFigure 1: Europe choropleth maps...\n")

# Load Eurostat country geometries once
cntr_sf <- tryCatch(
  st_read("data/CNTR_RG_20M_2020_4326.geojson", quiet = TRUE),
  error = function(e) {
    stop("Could not read data/CNTR_RG_20M_2020_4326.geojson — is the file present?")
  }
)
id_col <- intersect(c("CNTR_ID", "id", "ISO2_CODE", "NUTS_ID"), names(cntr_sf))
if (length(id_col) == 0) id_col <- names(cntr_sf)[1]
id_col <- id_col[1]
cntr_sf[["_code"]] <- as.character(cntr_sf[[id_col]])

# Compute a shared symmetric scale limit across all three years so the colour
# meaning is consistent when comparing panels
all_map_vals <- results[component == "total" & ssp == "3" &
                          year %in% c(2050, 2075, 2099) & age == 65, multiplier]
map_limit_global <- max(abs(all_map_vals - 1), na.rm = TRUE) * 1.05

make_map <- function(yr) {
  map_data <- results[component == "total" & ssp == "3" & year == yr & age == 65,
                      .(country_code, country_name, multiplier)]

  cntr_map <- merge(cntr_sf, map_data,
                    by.x = "_code", by.y = "country_code",
                    all.x = TRUE)
  cntr_map <- st_crop(cntr_map, xmin = -25, xmax = 45, ymin = 32, ymax = 72)

  map_labels <- suppressWarnings(
    st_centroid(cntr_map[!is.na(cntr_map$multiplier), ])
  )

  ggplot(cntr_map) +
    geom_sf(aes(fill = multiplier), color = "white", linewidth = 0.25) +
    shadowtext::geom_shadowtext(
      data          = map_labels,
      aes(geometry  = geometry, label = sprintf("%.2f", multiplier)),
      stat          = "sf_coordinates",
      size          = 2.4,
      colour        = "white",
      bg.colour     = "black",
      bg.r          = 0.18,
      fontface      = "bold",
      check_overlap = TRUE
    ) +
    scale_fill_gradient2(
      low      = "#2166ac",
      mid      = "white",
      high     = "#b2182b",
      midpoint = 1,
      limits   = c(1 - map_limit_global, 1 + map_limit_global),
      na.value = "black",
      name     = "Mortality\nmultiplier",
      labels   = label_number(accuracy = 0.01),
      guide    = guide_colorbar(
        barwidth = unit(8, "cm"), barheight = unit(0.4, "cm"),
        title.position = "top", title.hjust = 0.5
      )
    ) +
    coord_sf(xlim = c(-12, 40), ylim = c(34, 71), expand = FALSE) +
    labs(
      title    = "Southern and Eastern Europe face the steepest rise in heat mortality",
      subtitle = sprintf(
        "Projected mortality multiplier under SSP3-7.0 by %d, relative to 1990\u20132019 baseline (age 65)", yr
      ),
      caption  = paste0(
        "Method: population-weighted country ERF applied to country-level projected temperatures.\n",
        "Baseline: 1990\u20132019 pooled GCM ensemble. SSP3-7.0 (+4\u00b0C by 2100)."
      )
    ) +
    theme_pub(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      axis.text        = element_blank(),
      axis.title       = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#d6e4f0", color = NA),
      legend.position  = "bottom"
    )
}

for (yr in c(2050, 2075, 2099)) {
  cat(sprintf("  year %d\n", yr))
  save_fig(make_map(yr),
           sprintf("country_map_multiplier_%d_ssp3", yr),
           width = 10, height = 8)
}

################################################################################
# Figure 2 — ERF curves (selected countries, all age groups)
################################################################################

cat("\nFigure 2: ERF curves...\n")

# Select 9 representative countries spanning N–S and E–W gradients
focus_countries <- c("FI", "NO", "DK", "DE", "PL", "FR", "AT", "ES", "RO")

erf_long <- melt(erf_raw[country_code %in% focus_countries],
                 id.vars       = c("country_code", "temp"),
                 measure.vars  = c("20-44", "45-64", "65-74", "75-84", "85+"),
                 variable.name = "agegroup",
                 value.name    = "rr")

erf_long[, country_name := country_names[country_code]]
erf_long[, country_name := factor(country_name,
                                  levels = country_names[focus_countries])]

# MMT markers
mmt_focus <- mmt_dt[country_code %in% focus_countries]
mmt_focus[, country_name := factor(country_name,
                                   levels = country_names[focus_countries])]
# Average MMT across age groups for the vertical line
mmt_avg <- mmt_focus[, .(mmt = mean(mmt)), by = .(country_code, country_name)]

# Fixed axes so all panels are directly comparable
erf_xlim <- range(erf_long$temp, na.rm = TRUE)
erf_ylim <- c(1, max(erf_long$rr, na.rm = TRUE) * 1.02)

p_erf <- ggplot(erf_long, aes(x = temp, y = rr, colour = agegroup)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_vline(data = mmt_avg,
             aes(xintercept = mmt),
             linetype = "dashed", linewidth = 0.45, colour = "#666666") +
  geom_hline(yintercept = 1, linewidth = 0.3, colour = "#aaaaaa") +
  scale_colour_manual(values = age_palette, name = "Age group") +
  scale_x_continuous(limits = erf_xlim, labels = label_number(accuracy = 1)) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.01),
    limits = erf_ylim
  ) +
  facet_wrap(~country_name, ncol = 3, axes = "all") +
  labs(
    title    = "How temperature shapes mortality risk across Europe",
    subtitle = "Country-level exposure-response function (RR vs temperature) by age group.\nDashed line = minimum mortality temperature (MMT). Axes fixed for cross-country comparison.",
    x        = "Daily mean temperature (°C)",
    y        = "Relative risk (RR)",
    caption  = "ERF derived from population-weighted mean of city-level B-spline coefficients (Masselot et al., 2025)."
  ) +
  theme_pub(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.spacing   = unit(0.8, "lines"),
    axis.text.x     = element_text(),
    axis.text.y     = element_text()
  ) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(linewidth = 1.5)))

save_fig(p_erf, "country_erf_curves", width = 11, height = 10)

################################################################################
# Figure 2b — ERF curves with linear extrapolation beyond historical range
################################################################################

cat("\nFigure 2b: ERF curves with extrapolation...\n")

# Global shared temperature range across all focus countries
extrap_xlim <- c(
  floor(min(erf_long$temp) / 5) * 5,
  ceiling(max(erf_long$temp) / 5) * 5
)
extrap_temp_seq <- seq(extrap_xlim[1], extrap_xlim[2], by = 0.5)
rr_cap <- 2.0   # visualization cap

# Linear extrapolation of log-RR beyond each country's historical range.
# This mirrors dlnm::onebasis behaviour: B-splines extrapolate linearly
# (degree-1 polynomial) outside Bound.  Cold tail is floored at RR = 1
# (same floor applied in compute_country_rr_curves).
extrapolate_curve <- function(temp_obs, rr_obs, temp_new,
                               n_slope = 6L) {
  log_rr <- log(pmax(rr_obs, 1e-9))   # avoid log(0)
  t_min  <- min(temp_obs)
  t_max  <- max(temp_obs)

    # Right-tail slope from last n_slope points (in log-RR space)
  ri         <- tail(seq_along(temp_obs), n_slope)
  rfit       <- lm.fit(cbind(1, temp_obs[ri]), log_rr[ri])
  r_slope    <- rfit$coefficients[2]
  r_intercept <- log_rr[length(log_rr)] - r_slope * t_max

  # Left-tail slope from first n_slope points (same linear approach)
  li         <- head(seq_along(temp_obs), n_slope)
  lfit       <- lm.fit(cbind(1, temp_obs[li]), log_rr[li])
  l_slope    <- lfit$coefficients[2]
  l_intercept <- log_rr[1L] - l_slope * t_min

  result <- numeric(length(temp_new))
  for (k in seq_along(temp_new)) {
    t <- temp_new[k]
    if (t < t_min) {
      result[k] <- exp(l_intercept + l_slope * t)
    } else if (t > t_max) {
      result[k] <- exp(r_intercept + r_slope * t)
    } else {
      result[k] <- exp(approx(temp_obs, log_rr, xout = t)$y)
    }
  }
  pmax(result, 1)             # floor at 1 everywhere
}

erf_extrap_list <- list()
for (ctry in focus_countries) {
  ctry_data <- erf_raw[country_code == ctry]
  t_lo <- min(ctry_data$temp)
  t_hi <- max(ctry_data$temp)
  for (ag in c("20-44", "45-64", "65-74", "75-84", "85+")) {
    rr_ext <- extrapolate_curve(ctry_data$temp, ctry_data[[ag]],
                                extrap_temp_seq)
    # Assign a segment label so cold-extra and hot-extra are NEVER connected
    # by geom_line (which would draw a dotted line through the observed region)
    seg <- ifelse(extrap_temp_seq < t_lo, "cold_extra",
                  ifelse(extrap_temp_seq > t_hi, "hot_extra", "observed"))
    erf_extrap_list[[paste(ctry, ag)]] <- data.table(
      country_code = ctry,
      agegroup     = ag,
      temp         = extrap_temp_seq,
      rr           = rr_ext,
      segment      = seg,
      region       = ifelse(seg == "observed", "observed", "extrapolated"),
      line_group   = paste(ctry, ag, seg)   # unique group per segment run
    )
  }
}
erf_extrap <- rbindlist(erf_extrap_list)
erf_extrap[, country_name := factor(country_names[country_code],
                                    levels = country_names[focus_countries])]
erf_extrap[, agegroup := factor(agegroup,
                                levels = c("20-44","45-64","65-74","75-84","85+"))]

p_erf_ext <- ggplot(erf_extrap,
                    aes(x = temp, y = rr,
                        colour = agegroup, linetype = region,
                        group  = line_group)) +
  geom_line(linewidth = 0.65, alpha = 0.9) +
  geom_vline(data = mmt_avg,
             aes(xintercept = mmt),
             linetype = "dashed", linewidth = 0.45, colour = "#666666",
             inherit.aes = FALSE) +
  geom_hline(yintercept = 1, linewidth = 0.3, colour = "#aaaaaa") +
  scale_colour_manual(values = age_palette, name = "Age group") +
  scale_linetype_manual(
    values = c(observed = "solid", extrapolated = "dotted"),
    name   = NULL,
    labels = c(observed = "Within historical range",
               extrapolated = "Extrapolated (linear)")
  ) +
  scale_x_continuous(limits = extrap_xlim,
                     labels = label_number(accuracy = 1)) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  coord_cartesian(ylim = c(1, rr_cap)) +
  facet_wrap(~country_name, ncol = 3, axes = "all") +
  labs(
    title    = "Temperature-mortality risk extends beyond historical climate bounds",
    subtitle = sprintf(
      "ERF with linear extrapolation beyond each country's historical temperature range.\nDotted lines = extrapolated; RR capped at %.0f for readability. Dashed line = MMT.", rr_cap
    ),
    x       = "Daily mean temperature (\u00b0C)",
    y       = "Relative risk (RR)",
    caption = "Extrapolation method: linear continuation of B-spline at both boundaries (dlnm convention). RR floored at 1; y-axis cropped at 2."
  ) +
  theme_pub(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.spacing   = unit(0.8, "lines"),
    axis.text.x     = element_text(),
    axis.text.y     = element_text()
  ) +
  guides(
    colour   = guide_legend(nrow = 1, override.aes = list(linewidth = 1.5)),
    linetype = guide_legend(nrow = 1, override.aes = list(linewidth = 0.8))
  )

save_fig(p_erf_ext, "country_erf_curves_extrapolated", width = 11, height = 10)

################################################################################
# Figure 3 — Heat vs cold trade-off scatter (2050, 2075, 2099)
################################################################################

cat("\nFigure 3: Heat vs cold scatter...\n")

make_hc_scatter <- function(yr) {
  hc_data <- results[year == yr & ssp == "3" & age == 65 &
                     component %in% c("heat", "cold")]
  hc_wide <- dcast(hc_data, country_code + country_name ~ component,
                   value.var = "multiplier")
  hc_wide[, total := results[year == yr & ssp == "3" & age == 65 &
                               component == "total" &
                               country_code %in% hc_wide$country_code,
                             .(multiplier, country_code)][
                match(hc_wide$country_code, country_code), multiplier]]

  ggplot(hc_wide, aes(x = cold, y = heat)) +
    geom_vline(xintercept = 1, linewidth = 0.4, colour = "#cccccc") +
    geom_hline(yintercept = 1, linewidth = 0.4, colour = "#cccccc") +
    geom_point(aes(fill = total, size = total),
               shape = 21, colour = "white", stroke = 0.5, alpha = 0.9) +
    scale_fill_gradientn(
      colours = c("#fee8c8", "#fdbb84", "#e34a33", "#b30000"),
      name    = "Total\nmultiplier",
      guide   = guide_colorbar(barwidth = unit(5, "cm"), barheight = unit(0.35, "cm"),
                               title.position = "top", title.hjust = 0.5)
    ) +
    scale_size_continuous(range = c(2, 9), guide = "none") +
    geom_label_repel(
      aes(label = country_code),
      size = 3.2, family = "Montserrat",
      fontface = "bold", color = OceanBlue,
      fill = alpha("white", 0.85), label.size = 0.15,
      box.padding = 0.35, point.padding = 0.3, max.overlaps = 30,
      seed = 42
    ) +
    annotate("rect", xmin = -Inf, xmax = 1, ymin = 1, ymax = Inf,
             fill = "#fee0d2", alpha = 0.15) +
    annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1,
             fill = "#deebf7", alpha = 0.15) +
    scale_x_continuous(labels = label_number(accuracy = 0.01)) +
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +
    labs(
      title    = "Climate change widens the gap between heat and cold mortality",
      subtitle = sprintf(
        "Change in heat vs cold mortality multiplier by %d under SSP3-7.0, age 65.\nValues > 1 = increased burden relative to 1990–2019 baseline.", yr
      ),
      x       = sprintf("Cold mortality multiplier (%d / baseline)", yr),
      y       = sprintf("Heat mortality multiplier (%d / baseline)", yr),
      caption = "Each bubble = one country; size proportional to total (heat + cold) multiplier."
    ) +
    theme_pub(base_size = 12)
}

for (yr in c(2050, 2075, 2099)) {
  cat(sprintf("  year %d\n", yr))
  p_hc_yr <- make_hc_scatter(yr)
  if (yr == 2050) p_hc <- p_hc_yr  # keep for combined panel
  save_fig(p_hc_yr, sprintf("country_heat_cold_scatter_%d", yr), width = 9, height = 8)
}

################################################################################
# Figure 4 — Time-series ribbons (all SSPs, age 65-74)
################################################################################

cat("\nFigure 4: Time-series ribbons...\n")

# 5-year centred rolling mean to remove annual temperature noise
roll5 <- function(x) {
  n <- length(x)
  out <- x
  for (i in seq_along(x)) {
    idx <- max(1, i - 2):min(n, i + 2)
    out[i] <- mean(x[idx], na.rm = TRUE)
  }
  out
}

ts_smooth <- copy(ts_summary)
ts_smooth[, median := roll5(median), by = .(country_code, ssp)]
ts_smooth[, q10    := roll5(q10),    by = .(country_code, ssp)]
ts_smooth[, q90    := roll5(q90),    by = .(country_code, ssp)]

# Highlight a geographically diverse set
highlight <- c("FI", "DE", "FR", "IT", "ES", "RO", "PL", "EL")
highlight_names <- country_names[highlight]

# Background: SSP3 only, all non-highlighted countries — smoothed median
ts_bg <- ts_smooth[!(country_code %in% highlight) & ssp == "3"]
ts_hl <- ts_smooth[country_code %in% highlight]

ts_hl[, country_name := factor(country_name, levels = highlight_names)]
ts_hl[, ssp_label    := factor(ssp_label, levels = names(ssp_palette))]

# EU median ribbon across all countries
ts_eu <- ts_smooth[, .(
  median = median(median),
  q10    = quantile(median, 0.10),
  q90    = quantile(median, 0.90)
), by = .(ssp, ssp_label, year)]
ts_eu[, ssp_label := factor(ssp_label, levels = names(ssp_palette))]

# Build label positions (offset so labels don't overlap)
ts_eu_labels <- ts_eu[year == 2075][order(median)]
ts_eu_labels[, y_nudge := median + c(-0.003, 0, 0.003)]

p_ts <- ggplot() +
  # Baseline reference
  geom_hline(yintercept = 1, linewidth = 0.35, colour = "#aaaaaa",
             linetype = "dashed") +
  # Background spaghetti: SSP3, all other countries, faint grey
  geom_line(data = ts_bg,
            aes(x = year, y = median, group = country_code),
            colour = "#cccccc", linewidth = 0.3, alpha = 0.7) +
  # EU median ribbon (80% CI across countries)
  geom_ribbon(data = ts_eu,
              aes(x = year, ymin = q10, ymax = q90, fill = ssp_label),
              alpha = 0.14) +
  # EU median line (thick)
  geom_line(data = ts_eu,
            aes(x = year, y = median, colour = ssp_label),
            linewidth = 1.15) +
  # Highlighted countries (SSP3 only for legibility)
  geom_line(data = ts_hl[ssp == "3"],
            aes(x = year, y = median, group = country_name),
            colour = ssp_palette[["SSP3-7.0"]],
            linewidth = 0.55, alpha = 0.65) +
  # End-of-century labels (nudged to avoid overlap)
  geom_text(data = ts_eu_labels,
            aes(x = 2101, y = y_nudge, label = ssp_label, colour = ssp_label),
            hjust = 0, size = 3.2, family = "Montserrat", fontface = "bold",
            show.legend = FALSE) +
  scale_colour_manual(values = ssp_palette, name = "Scenario",
                      guide = guide_legend(override.aes = list(linewidth = 1.5))) +
  scale_fill_manual(values = ssp_palette, guide = "none") +
  scale_x_continuous(breaks = seq(2020, 2099, 20),
                     limits = c(2015, 2130), expand = c(0, 0)) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(
    title    = "Diverging mortality futures under different emission pathways",
    subtitle = paste0("Total mortality multiplier for age group 65–74, relative to 1990–2019 baseline.\n",
                      "Thick line = European median; shaded band = 10th–90th percentile across countries (5-yr smoothed)."),
    x        = NULL,
    y        = "Mortality multiplier",
    caption  = paste0("GCM ensemble of 19 models pooled. Grey lines = individual countries under SSP3-7.0.\n",
                      "Highlighted coloured lines = selected countries (FI, DE, FR, IT, ES, RO, PL, EL) under SSP3-7.0.")
  ) +
  theme_pub(base_size = 12) +
  theme(legend.position = "bottom")

save_fig(p_ts, "country_multiplier_timeseries", width = 10, height = 6.5)

################################################################################
# Figure 5 — Combined panel (journal-ready)
################################################################################

cat("\nFigure 5: Combined journal panel...\n")

p_map_2050 <- make_map(2050)

p_combined <- (p_map_2050 | p_hc) /
  (p_erf + theme(plot.margin = margin(t = 6))) +
  plot_annotation(
    title   = "Climate change and temperature-related mortality in Europe",
    subtitle = paste0("Country-level projections under three emission scenarios (SSP1-2.6 / SSP2-4.5 / SSP3-7.0)\n",
                      "Masselot et al. (2025) exposure-response functions aggregated to national scale"),
    caption = paste0("Data: CORDEX-EUR11 downscaled CMIP6 projections (19 GCMs). ",
                     "ERF: B-spline DLNM fitted to MCC network data, 854 European cities.\n",
                     "Mortality multiplier = average RR in target year ÷ average RR in 1990–2019 baseline."),
    theme = theme_pub(base_size = 12) +
      theme(
        plot.title    = element_text(face = "bold", size = 15, color = OceanBlue),
        plot.subtitle = element_text(size = 11, color = OceanBlue)
      )
  ) +
  plot_layout(heights = c(1.2, 1.5))

save_fig(p_combined, "country_combined_panel", width = 16, height = 22)

################################################################################
# Figure 6 — Age-gradient heatmap (countries × age groups) (2050, 2075, 2099)
################################################################################

cat("\nFigure 6: Age-gradient heatmap...\n")

make_heatmap <- function(yr) {
  hmd <- results[
    component == "total" & ssp == "3" & year == yr &
      age %in% c(32, 55, 67, 80, 90),
    .(country_code, country_name, age, multiplier)
  ]
  hmd[, age_label := fcase(
    age == 32, "20–44",
    age == 55, "45–64",
    age == 67, "65–74",
    age == 80, "75–84",
    age == 90, "85+",
    default   = as.character(age)
  )]
  hmd[, age_label := factor(age_label,
                             levels = c("20–44", "45–64", "65–74",
                                        "75–84", "85+"))]
  country_order <- hmd[age == 67][order(multiplier), country_code]
  hmd[, country_code := factor(country_code, levels = country_order)]
  hmd[, country_name := factor(country_name, levels = country_names[country_order])]

  ggplot(hmd, aes(x = age_label, y = country_name, fill = multiplier)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.2f", multiplier)),
              size = 3, family = "Montserrat", color = "white", fontface = "bold") +
    scale_fill_gradientn(
      colours = c("#fff5f0", "#fcbba1", "#fc8d59", "#ef6548",
                  "#d7301f", "#990000", "#67000d"),
      name    = "Mortality\nmultiplier",
      guide   = guide_colorbar(barwidth = unit(6, "cm"), barheight = unit(0.4, "cm"),
                               title.position = "top", title.hjust = 0.5)
    ) +
    labs(
      title    = "Older Europeans face exponentially higher mortality multipliers",
      subtitle = sprintf(
        "Total mortality multiplier at %d under SSP3-7.0 by country and age group.\nCountries sorted by multiplier at age 65–74.", yr
      ),
      x       = "Age group",
      y       = NULL,
      caption = "Multiplier = projected average RR ÷ 1990–2019 baseline average RR."
    ) +
    theme_pub(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid      = element_blank(),
      axis.text.y     = element_text(size = 9)
    )
}

for (yr in c(2050, 2075, 2099)) {
  cat(sprintf("  year %d\n", yr))
  save_fig(make_heatmap(yr), sprintf("country_multiplier_heatmap_%d", yr),
           width = 8, height = 11)
}

cat("\nAll figures saved to plots/\n")
cat("Files produced:\n")
for (f in list.files("plots", pattern = "country_", full.names = FALSE)) {
  cat(sprintf("  plots/%s\n", f))
}
