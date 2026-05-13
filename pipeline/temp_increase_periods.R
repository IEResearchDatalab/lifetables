## Temperature increase by period: RCP2.6 and RCP7.0
## -------------------------------------------------------
## EU/EEA countries (30): internal pipeline (population-weighted, bias-corrected)
## Non-EU VIG markets  :  IPCC AR6 Atlas CMIP6 (area-weighted, raw model output)
##
## Baseline     : 2000-2025 (26 years, CMIP6 historical 2000-2014 + scenario-own extension 2015-2025)
##                RCP2.6 baseline = hist + SSP1-2.6 (2015-2025)
##                RCP4.5 baseline = hist + SSP2-4.5 (2015-2025)
##                RCP7.0 baseline = hist + SSP3-7.0 (2015-2025)
## Periods      : 2030-2050 | 2051-2070 | 2071-2099
## Delta_T      : ensemble-mean future annual-mean T minus ensemble-mean baseline T
## -------------------------------------------------------
##
## METHODOLOGICAL LIMITATIONS (non-EU countries):
##
##  1. SPATIAL AGGREGATION: The IPCC AR6 Atlas provides only 4 reference regions
##     for Europe (NEU, WCE, EEU, MED). EEU spans ~9.7M km² from Serbia to the
##     Russian Urals. All countries assigned to the same region receive mathematically
##     identical warming estimates. No country-level differentiation is possible from
##     this data source.
##
##  2. GEOGRAPHIC ASSIGNMENTS (per IPCC AR6 reference region definitions):
##     - EEU: Serbia (RS), Ukraine (UA), Bosnia (BA), Moldova (MD)
##     - MED: Turkey (TR), Albania (AL), Montenegro (ME), North Macedonia (MK)
##     Note: ME and MK are classified MED because they drain to the Adriatic/Aegean
##     and share Mediterranean climate characteristics (~42-41°N).
##
##  3. METHODOLOGICAL NOTE:
##     - EU data:     bias-corrected, 0.11° EURO-CORDEX downscaling, population-weighted
##     - Non-EU data: raw global CMIP6 output, cosine-latitude weighted, regional mean
##     Baseline = CMIP6 historical (2000-2014) spliced with the scenario's own run
##     (2015-2025): RCP2.6 uses SSP1-2.6, RCP7.0 uses SSP3-7.0. Each scenario is
##     fully self-contained. Delta_T is internally consistent (no obs/model mismatch).
##
##  4. RCP 2.6 non-monotonic pattern is PHYSICALLY CORRECT: SSP1-2.6 features
##     net-negative emissions after ~2050, causing temperatures to peak mid-century
##     and decline slightly toward 2100.
##
##  PROPER FIX: Extract country-level temperatures from gridded CMIP6/CORDEX data
##  using country masks. The EURO-CORDEX domain covers all non-EU VIG markets but
##  the processing pipeline was run only for EU Urban Audit cities.
## -------------------------------------------------------

library(arrow)
library(dplyr)
library(data.table)
library(httr)
library(sf)
library(ggplot2)
library(scales)
library(showtext)

font_add("Montserrat",
         regular    = "fonts/Montserrat-Regular.ttf",
         bold       = "fonts/Montserrat-Bold.ttf",
         italic     = "fonts/Montserrat-Italic.ttf",
         bolditalic = "fonts/Montserrat-BoldItalic.ttf")
showtext_auto()
showtext_opts(dpi = 180)

set.seed(42)

# ---- Parameters --------------------------------------------------------
# Baseline period: CMIP6 historical run covers 1850-2014 and SSP runs start
# from 2015. We splice hist (2000-2014) with the scenario's own SSP run
# (2015-2025) to form a modern 26-year reference period per scenario.
hist_yrs     <- 2000:2014              # covered by CMIP6 'historical' run
ssp2_yrs     <- 2015:2025              # 2015-2025 taken from each scenario's own SSP run
baseline_yrs <- c(hist_yrs, ssp2_yrs) # full reference period: 2000-2025

# Future comparison periods
periods <- list(
  "2030-2050" = 2030:2050,
  "2051-2070" = 2051:2070,
  "2071-2099" = 2071:2099
)
period_names    <- names(periods)
period_suffixes <- gsub("-", "_", period_names)   # "2030_2050" etc.

# ---- Helper: ensemble daily mean -> annual mean over given years --------
# Takes a collected data.table (rows = daily observations, cols include GCM
# temperature columns and a 'date' column), averages across GCMs to form an
# ensemble mean for each day, then returns the annual mean for the requested
# years, aggregated by URAU_CODE (2-letter country ISO code in this dataset).
agg_period_fn <- function(dt_collected, years, gcms) {
  dt <- as.data.table(dt_collected)
  dt[, ensemble_T := rowMeans(.SD, na.rm = TRUE), .SDcols = gcms]  # cross-GCM mean per day
  dt[, year := as.integer(format(as.Date(date), "%Y"))]
  dt[year %in% years,
     .(mean_T = mean(ensemble_T, na.rm = TRUE)),  # annual mean across all days in window
     by = URAU_CODE]
}

# ---- 1. EU COUNTRIES from internal pipeline ----------------------------
# Source: data/tmeanproj_country.parquet
#   - Rows: one per country-day combination
#   - URAU_CODE: 2-letter ISO country code (EU Urban Audit convention)
#   - ssp: "hist" = CMIP6 historical run (up to 2014),
#          "1" = SSP1-2.6, "2" = SSP2-4.5, "3" = SSP3-7.0 (from 2015)
#   - tas_*: per-GCM surface air temperature (K), bias-corrected, 0.11° grid,
#            population-weighted city means aggregated to national level
cat("[1/4] Loading internal parquet...\n")
ds       <- open_dataset("data/tmeanproj_country.parquet")
gcm_cols <- grep("^tas_", names(ds), value = TRUE)  # identify GCM temperature columns

# Load historical run once; 2000-2014 slice is the common hist component of both baselines
cat("  Loading CMIP6 historical for EU baseline...\n")
hist_raw  <- ds |> filter(ssp == "hist") |> collect()
eu_base_h <- agg_period_fn(hist_raw, hist_yrs, gcm_cols)  # country means 2000-2014
rm(hist_raw); gc()

# For each scenario, load the full SSP run once and:
#   (a) extract 2015-2025 to complete the scenario-specific baseline
#   (b) compute the mean for each future period
# This ensures each scenario's delta_T is 100% self-contained (no cross-scenario mixing)
eu_delta <- list()
for (ssp_code in c("1", "2", "3")) {
  ssp_short <- switch(ssp_code, "1" = "rcp26", "2" = "rcp45", "3" = "rcp70")
  cat(sprintf("  Computing EU deltas for SSP%s (baseline: hist + SSP%s 2015-2025)...\n",
              ssp_code, ssp_code))
  fut_raw <- ds |> filter(ssp == ssp_code) |> collect()  # full SSP run for this scenario

  # Weighted splice: combine hist and SSP means proportional to number of years
  base_s  <- agg_period_fn(fut_raw, ssp2_yrs, gcm_cols)  # 2015-2025 from this scenario
  eu_base <- merge(eu_base_h, base_s, by = "URAU_CODE", suffixes = c("_h", "_s"))
  eu_base[, mean_T := (mean_T_h * length(hist_yrs) + mean_T_s * length(ssp2_yrs)) /
                       length(baseline_yrs)]  # weighted mean => 2000-2025 baseline T
  eu_base <- eu_base[, .(URAU_CODE, mean_T)]

  # Compute delta_T for each future period
  for (i in seq_along(periods)) {
    fut_mean <- agg_period_fn(fut_raw, periods[[i]], gcm_cols)
    merged   <- merge(eu_base, fut_mean, by = "URAU_CODE",
                      suffixes = c("_base", "_fut"))
    merged[, delta_T := round(mean_T_fut - mean_T_base, 2)]
    col <- paste0("delta_", ssp_short, "_", period_suffixes[i])
    eu_delta[[col]] <- merged[, .(URAU_CODE, delta_T)]
    setnames(eu_delta[[col]], "delta_T", col)
  }
  rm(fut_raw, base_s, eu_base); gc()
}

# Merge all six delta columns into one wide table
eu_wide <- eu_delta[[1]][, .(country_code = URAU_CODE)]
for (col in names(eu_delta)) {
  tmp <- copy(eu_delta[[col]])
  setnames(tmp, "URAU_CODE", "country_code")
  eu_wide <- merge(eu_wide, tmp, by = "country_code")
}
eu_wide[, source := "Internal pipeline"]

# Add country names
country_names <- c(
  AT = "Austria",        BE = "Belgium",        BG = "Bulgaria",
  CH = "Switzerland",    CY = "Cyprus",         CZ = "Czech Republic",
  DE = "Germany",        DK = "Denmark",        EE = "Estonia",
  EL = "Greece",         ES = "Spain",          FI = "Finland",
  FR = "France",         HR = "Croatia",        HU = "Hungary",
  IE = "Ireland",        IT = "Italy",          LT = "Lithuania",
  LU = "Luxembourg",     LV = "Latvia",         MT = "Malta",
  NL = "Netherlands",    NO = "Norway",         PL = "Poland",
  PT = "Portugal",       RO = "Romania",        SE = "Sweden",
  SI = "Slovenia",       SK = "Slovakia",       UK = "United Kingdom"
)
eu_wide[, country_name := country_names[country_code]]
eu_wide <- eu_wide[!is.na(country_name)]
cat("  Done. EU countries:", nrow(eu_wide), "\n")

# ---- 2. NON-EU VIG MARKETS from IPCC AR6 Atlas CMIP6 ------------------
# Source: IPCC AR6 Interactive Atlas GitHub repository
#   https://github.com/IPCC-WG1/Atlas/tree/main/datasets-aggregated-regionally/
# The Atlas provides pre-aggregated monthly land temperatures per CMIP6 model
# for IPCC AR6 reference regions. Each CSV covers one model x scenario x run.
#
# Region assignments follow official IPCC AR6 WGI definitions:
#   MED (Mediterranean): TR, AL, ME (~42.5°N, Adriatic coast), MK (~41.6°N, Aegean)
#   EEU (Eastern Europe): RS (~44°N, Danube), UA (~49°N), BA (~44°N), MD (~47°N)
# IMPORTANT: all countries within a region receive IDENTICAL estimates because
# the Atlas provides no sub-regional differentiation within each reference region.
vig_noeu <- data.table(
  country_code = c("TR", "RS", "UA", "BA", "MD", "MK", "ME", "AL"),
  country_name = c("Turkey", "Serbia", "Ukraine", "Bosnia and Herzegovina",
                   "Moldova", "North Macedonia", "Montenegro", "Albania"),
  ipcc_region  = c("MED", "EEU", "EEU", "EEU", "EEU", "MED", "MED", "MED")
)

ipcc_models <- list(
  `ACCESS-CM2`       = "r1i1p1f1",
  `ACCESS-ESM1-5`    = "r1i1p1f1",
  `BCC-CSM2-MR`      = "r1i1p1f1",
  `CanESM5`          = "r1i1p1f1",
  `EC-Earth3`        = "r1i1p1f1",
  `EC-Earth3-Veg-LR` = "r1i1p1f1",
  `GFDL-ESM4`        = "r1i1p1f1",
  `IITM-ESM`         = "r1i1p1f1",
  `INM-CM4-8`        = "r1i1p1f1",
  `INM-CM5-0`        = "r1i1p1f1",
  `IPSL-CM6A-LR`     = "r1i1p1f1",
  `KACE-1-0-G`       = "r2i1p1f1",
  `MIROC6`           = "r1i1p1f1",
  `MPI-ESM1-2-HR`    = "r1i1p1f1",
  `MPI-ESM1-2-LR`    = "r1i1p1f1",
  `MRI-ESM2-0`       = "r1i1p1f1",
  `NorESM2-LM`       = "r1i1p1f1",
  `NorESM2-MM`       = "r1i1p1f1"
)

BASE_URL <- paste0(
  "https://raw.githubusercontent.com/IPCC-WG1/Atlas/main/",
  "datasets-aggregated-regionally/data/CMIP6/CMIP6_tas_land/"
)

# Download one IPCC Atlas CSV for a given model/scenario/run combination.
# Returns a data.table with columns: date (YYYY-MM-DD), and one column per
# IPCC reference region (e.g. EEU, MED, NEU, WCE...) with monthly land-mean
# surface air temperature (°C). Returns NULL on HTTP error or parse failure.
fetch_ipcc <- function(model, run, scenario) {
  fname <- sprintf("CMIP6_%s_%s_%s.csv", model, scenario, run)
  url   <- paste0(BASE_URL, fname)
  resp  <- tryCatch(GET(url, timeout(60)), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) {
    message("  SKIP ", fname,
            " (", if (!is.null(resp)) status_code(resp) else "error", ")")
    return(NULL)
  }
  txt   <- content(resp, "text", encoding = "UTF-8")
  lines <- strsplit(txt, "\n")[[1]]
  lines <- lines[!startsWith(trimws(lines), "#")]  # strip comment header rows
  tryCatch(
    fread(text = paste(lines, collapse = "\n"), header = TRUE, data.table = TRUE),
    error = function(e) { message("  PARSE ERROR ", fname); NULL }
  )
}

# Compute the mean temperature for a given IPCC region column over the requested
# years (filters by the first 4 chars of the 'date' field, i.e. the year).
region_annual_mean <- function(dt, region_col, years) {
  if (is.null(dt) || !(region_col %in% names(dt))) return(NA_real_)
  dt2 <- copy(dt)
  dt2[, year := as.integer(substring(get("date"), 1, 4))]
  sub <- dt2[year %in% years, .SD, .SDcols = c("year", region_col)]
  mean(sub[[region_col]], na.rm = TRUE)
}

cat("[2/4] Downloading IPCC Atlas CMIP6 files for EEU and MED regions...\n")
cat("  (", length(ipcc_models), "models \u00d7 4 files each: historical + ssp126 + ssp245 + ssp370)\n")

noeu_results <- list()  # will accumulate one row per (model, scenario, region, period)

for (mdl in names(ipcc_models)) {
  run <- ipcc_models[[mdl]]
  cat("  Model:", mdl, "\n")

  # Historical run covers 1850-2014; extract the 2000-2014 regional means once
  # (shared component of both scenario baselines for this model)
  hist_dt <- fetch_ipcc(mdl, run, "historical")
  if (is.null(hist_dt)) {
    message("  SKIP model (no historical): ", mdl)
    next
  }
  hist_EEU <- region_annual_mean(hist_dt, "EEU", hist_yrs)
  hist_MED <- region_annual_mean(hist_dt, "MED", hist_yrs)
  rm(hist_dt)

  for (scen_ipcc in c("ssp126", "ssp245", "ssp370")) {
    fut_dt <- fetch_ipcc(mdl, run, scen_ipcc)  # SSP run starts from 2015
    if (is.null(fut_dt)) next
    rcp_label <- switch(scen_ipcc, ssp126 = "RCP2.6", ssp245 = "RCP4.5", ssp370 = "RCP7.0")

    # Build scenario-specific 2000-2025 baseline:
    #   base_T = weighted mean of hist(2000-2014) + this SSP's 2015-2025
    # Then compute delta_T = future_period_mean - base_T for each period
    for (reg in c("EEU", "MED")) {
      hist_T <- if (reg == "EEU") hist_EEU else hist_MED
      if (is.na(hist_T)) next
      scen_T <- region_annual_mean(fut_dt, reg, ssp2_yrs)
      base_T <- if (!is.na(scen_T)) {
        (hist_T * length(hist_yrs) + scen_T * length(ssp2_yrs)) / length(baseline_yrs)
      } else {
        hist_T  # fallback to hist-only if 2015-2025 unavailable
      }

      for (pname in period_names) {
        fut_T <- region_annual_mean(fut_dt, reg, periods[[pname]])
        if (is.na(fut_T)) next
        noeu_results[[length(noeu_results) + 1]] <- data.table(
          region   = reg,
          model    = mdl,
          scenario = rcp_label,
          period   = pname,
          delta_T  = fut_T - base_T
        )
      }
    }
  }
}

noeu_dt <- rbindlist(noeu_results)
noeu_region_mean <- noeu_dt[,
  .(delta_T_mean = mean(delta_T, na.rm = TRUE), n_models = .N),
  by = .(region, scenario, period)
]
cat("  Models per region x scenario x period:\n")
print(noeu_region_mean[order(region, scenario, period)])

# Build wide non-EU table
noeu_wide_rows <- lapply(seq_len(nrow(vig_noeu)), function(j) {
  row <- vig_noeu[j]
  reg <- row$ipcc_region
  out <- data.table(country_code = row$country_code,
                    country_name = row$country_name)
  for (i in seq_along(periods)) {
    psuffix <- period_suffixes[i]
    pname   <- period_names[i]
    for (ssp_label in c("RCP2.6", "RCP4.5", "RCP7.0")) {
      ssp_short <- switch(ssp_label, "RCP2.6" = "rcp26", "RCP4.5" = "rcp45", "RCP7.0" = "rcp70")
      col <- paste0("delta_", ssp_short, "_", psuffix)
      val <- noeu_region_mean[region == reg & scenario == ssp_label & period == pname,
                              delta_T_mean]
      out[, (col) := round(if (length(val) == 1L) val else NA_real_, 2)]
    }
  }
  out[, source := paste0("IPCC AR6 Atlas (", reg, " region)")]
  out
})
noeu_wide <- rbindlist(noeu_wide_rows)
cat("  Done. Non-EU VIG markets:", nrow(noeu_wide), "\n")

# ---- 3. COMBINE --------------------------------------------------------
cat("[3/4] Combining results...\n")

# Column order: interleaved rcp26 / rcp45 / rcp70 per period
delta_cols <- c(rbind(
  paste0("delta_rcp26_", period_suffixes),
  paste0("delta_rcp45_", period_suffixes),
  paste0("delta_rcp70_", period_suffixes)
))

eu_sub   <- eu_wide[,   c("country_code", "country_name", delta_cols),
                    with = FALSE]
noeu_sub <- noeu_wide[, c("country_code", "country_name", delta_cols),
                       with = FALSE]

combined <- rbindlist(list(eu_sub, noeu_sub), fill = TRUE)
setorder(combined, country_name)
combined[, vig_noneu := country_code %in% vig_noeu$country_code]

cat("  Total countries:", nrow(combined), "\n")

# ---- 4. EXPORT ---------------------------------------------------------
cat("[4/4] Exporting outputs...\n")

dir.create("results_csv", showWarnings = FALSE)
dir.create("plots",       showWarnings = FALSE)

## 4a. CSV (wide: one row per country, interleaved rcp26/rcp70 columns per period)
fwrite(combined[, c("country_code", "country_name", delta_cols),
                with = FALSE],
       "results_csv/temp_increase_periods.csv")
cat("  Saved: results_csv/temp_increase_periods.csv\n")

## 4b. Graphic table (heatmap, VIG markets only, faceted by period)
vig_eu_markets <- c("AT", "BG", "CZ", "DE", "EE", "HR", "HU",
                    "LT", "LV", "PL", "RO", "SK")
vig_all_codes  <- c(vig_eu_markets, vig_noeu$country_code)
vig_combined   <- combined[country_code %in% vig_all_codes]

# Long format: one row per (country, period, scenario)
plot_dt <- melt(
  vig_combined[, c("country_code", "country_name", "vig_noneu", delta_cols),
               with = FALSE],
  id.vars      = c("country_code", "country_name", "vig_noneu"),
  measure.vars = delta_cols,
  variable.name = "col_id",
  value.name    = "delta_T"
)

# Parse scenario and period labels from column name
plot_dt[, scenario_label := fcase(
  grepl("_rcp26_", col_id), "RCP 2.6",
  grepl("_rcp45_", col_id), "RCP 4.5",
  grepl("_rcp70_", col_id), "RCP 7.0"
)]
plot_dt[, period_label := fcase(
  grepl("2030_2050", col_id), "2030\u20132050",
  grepl("2051_2070", col_id), "2051\u20132070",
  grepl("2071_2099", col_id), "2071\u20132099"
)]
plot_dt[, period_label := factor(period_label,
  levels = c("2030\u20132050", "2051\u20132070", "2071\u20132099"))]

# Country latitude (approximate centroid) for north-to-south ordering
lat_ref <- c(
  FI = 64.0, NO = 62.0, SE = 60.0, EE = 59.0, LV = 57.0, LT = 56.0,
  DK = 56.0, IE = 53.5, UK = 54.0, NL = 52.5, PL = 52.0, BE = 50.8,
  DE = 51.0, CZ = 50.0, SK = 48.7, LU = 49.8, AT = 47.5, HU = 47.0,
  CH = 47.0, RO = 46.0, FR = 46.5, SI = 46.0, HR = 45.5, BG = 43.0,
  IT = 43.0, PT = 39.5, ES = 40.0, EL = 39.0, CY = 35.0, MT = 35.9,
  UA = 49.0, MD = 47.0, RS = 44.0, BA = 44.0, MK = 41.6, ME = 42.5,
  AL = 41.0, TR = 39.0
)
plot_dt[, lat := lat_ref[country_code]]
# Sort: highest latitude first (north at top)
plot_dt[, country_label := paste0(country_code, " \u2013 ", country_name)]
plot_dt[, country_label := factor(country_label,
  levels = unique(plot_dt[order(-lat), country_label]))]
plot_dt[, country_label := factor(country_label,
  levels = rev(levels(plot_dt$country_label)))]

max_delta   <- ceiling(max(plot_dt$delta_T, na.rm = TRUE) * 2) / 2
breaks_vals <- c(0, max_delta / 2, max_delta)

gg <- ggplot(plot_dt,
             aes(x = scenario_label, y = country_label, fill = delta_T)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.2f", delta_T)),
            size = 3.5, colour = "black", fontface = "bold",
            family = "Montserrat") +
  scale_fill_gradientn(
    colours = c("#FFFDE7", "#FF9800", "#B71C1C"),
    values  = scales::rescale(breaks_vals, from = c(0, max_delta)),
    limits  = c(0, max_delta),
    name    = "\u0394T (\u00b0C)"
  ) +
  scale_x_discrete(position = "top", expand = c(0, 0)) +
  facet_wrap(~period_label, nrow = 1) +
  labs(
    title    = "Average Temperature Increase vs 2000\u20132025 Baseline",
    subtitle = "Ensemble-mean \u0394T by period | Countries sorted north to south",
    x = NULL, y = NULL,
    caption  = paste0(
      "EU/EEA: EURO-CORDEX CMIP6, population-weighted (", length(gcm_cols),
      " GCMs).\n",
      "Non-EU VIG: IPCC AR6 Atlas CMIP6 land temperatures, ",
      noeu_dt[, uniqueN(model)], " GCMs. ",
      "Non-EU values are IPCC AR6 reference region means ",
      "(EEU \u224810M km\u00b2; MED \u22486M km\u00b2); ",
      "countries within the same region are assigned identical values.\n",
      "RCP 2.6 = SSP1-2.6; RCP 7.0 = SSP3-7.0. ",
      "RCP 2.6 peak-and-decline in the late period reflects net-negative emissions under SSP1-2.6."
    )
  ) +
  theme_minimal(base_size = 12, base_family = "Montserrat") +
  theme(
    plot.title    = element_text(face = "bold", size = 15, family = "Montserrat"),
    plot.subtitle = element_text(size = 10, colour = "grey40", family = "Montserrat"),
    plot.caption  = element_text(size = 8, colour = "grey50", hjust = 0,
                                 family = "Montserrat"),
    axis.text.x   = element_text(face = "bold", size = 11, family = "Montserrat"),
    axis.text.y   = element_text(size = 10, family = "Montserrat"),
    strip.text    = element_text(face = "bold", size = 12, family = "Montserrat",
                                 margin = margin(b = 4)),
    legend.text        = element_text(size = 10, family = "Montserrat"),
    legend.title       = element_text(size = 11, family = "Montserrat"),
    legend.position    = "right",
    panel.grid         = element_blank(),
    panel.spacing      = unit(0.5, "cm"),
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    plot.margin        = margin(12, 12, 12, 12)
  )

n_countries <- nrow(vig_combined)
out_png <- "plots/temp_increase_periods_table.png"
ggsave(out_png, gg,
       width  = 18,
       height = max(7, n_countries * 0.42 + 4),
       dpi    = 180, bg = "white")
cat("  Saved:", out_png, "\n")

## 4c. Maps: one PNG per scenario x period, with country delta_T labels
cat("[5/5] Generating maps...\n")

world   <- st_read("data/CNTR_RG_20M_2020_4326.geojson", quiet = TRUE)
bbox_eu <- st_bbox(c(xmin = -25, xmax = 45, ymin = 30, ymax = 72),
                   crs = st_crs(4326))
europe  <- st_crop(world, bbox_eu)
europe  <- merge(europe, combined, by.x = "CNTR_ID", by.y = "country_code",
                 all.x = TRUE)

scale_min   <- 0
scale_max   <- ceiling(max(unlist(combined[, delta_cols, with = FALSE],
                                  use.names = FALSE), na.rm = TRUE) * 2) / 2
mid_val_map <- scale_max / 2
map_colours <- c("#FFFDE7", "#FF9800", "#B71C1C")

map_specs <- list(
  list(col = "delta_rcp26_2030_2050", scenario = "RCP 2.6", period = "2030\u20132050"),
  list(col = "delta_rcp45_2030_2050", scenario = "RCP 4.5", period = "2030\u20132050"),
  list(col = "delta_rcp70_2030_2050", scenario = "RCP 7.0", period = "2030\u20132050"),
  list(col = "delta_rcp26_2051_2070", scenario = "RCP 2.6", period = "2051\u20132070"),
  list(col = "delta_rcp45_2051_2070", scenario = "RCP 4.5", period = "2051\u20132070"),
  list(col = "delta_rcp70_2051_2070", scenario = "RCP 7.0", period = "2051\u20132070"),
  list(col = "delta_rcp26_2071_2099", scenario = "RCP 2.6", period = "2071\u20132099"),
  list(col = "delta_rcp45_2071_2099", scenario = "RCP 4.5", period = "2071\u20132099"),
  list(col = "delta_rcp70_2071_2099", scenario = "RCP 7.0", period = "2071\u20132099")
)

for (m in map_specs) {
  plot_sf          <- europe
  plot_sf$fill_val <- st_drop_geometry(plot_sf)[[m$col]]
  labeled_sf       <- plot_sf[!is.na(plot_sf$fill_val), ]

  slug     <- gsub("delta_", "", m$col)
  out_file <- paste0("plots/temp_increase_map_", slug, ".png")

  gg_map <- ggplot(plot_sf) +
    geom_sf(aes(fill = fill_val), colour = "white", linewidth = 0.2) +
    geom_sf_text(data = labeled_sf,
                 aes(label = sprintf("%.2f", fill_val)),
                 size = 2.6, colour = "black", fontface = "bold",
                 family = "Montserrat", check_overlap = TRUE) +
    scale_fill_gradientn(
      colours  = map_colours,
      values   = scales::rescale(c(scale_min, mid_val_map, scale_max),
                                 from = c(scale_min, scale_max)),
      limits   = c(scale_min, scale_max),
      na.value = "grey85",
      name     = "\u0394T (\u00b0C)",
      guide    = guide_colorbar(
        barwidth  = unit(0.4, "cm"),
        barheight = unit(5,   "cm"),
        ticks     = TRUE
      )
    ) +
    coord_sf(xlim = c(-25, 45), ylim = c(30, 72), expand = FALSE) +
    labs(
      title    = paste0(m$scenario, "  \u2014  ", m$period),
      subtitle = "Ensemble-mean \u0394T vs 2000\u20132025 baseline",
      caption  = paste0(
        "EU/EEA: EURO-CORDEX CMIP6, population-weighted. ",
        "Non-EU VIG: IPCC AR6 Atlas regional means (EEU or MED). ",
        "Grey = no data."
      )
    ) +
    theme_void(base_family = "Montserrat") +
    theme(
      plot.title      = element_text(face = "bold", size = 16,
                                     family = "Montserrat", margin = margin(b = 4)),
      plot.subtitle   = element_text(size = 10, colour = "grey40",
                                     family = "Montserrat", margin = margin(b = 6)),
      plot.caption    = element_text(size = 7, colour = "grey55", hjust = 0,
                                     family = "Montserrat", margin = margin(t = 6)),
      legend.text     = element_text(size = 9,  family = "Montserrat"),
      legend.title    = element_text(size = 10, family = "Montserrat"),
      legend.position = "right",
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin     = margin(12, 12, 10, 12)
    )

  ggsave(out_file, gg_map, width = 9, height = 6.5, dpi = 180, bg = "white")
  cat("  Saved:", out_file, "\n")
}

cat("\n=== ALL DONE ===\n")
print(combined[, c("country_code", "country_name", delta_cols), with = FALSE])
