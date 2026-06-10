################################################################################
#
# Wealth Metric vs Mortality Multiplier Analysis
#
# For each of the 854 Urban Audit cities (Masselot et al.):
#   1. Map each city to NUTS3 via spatial join against Eurostat NUTS polygons
#      (city lat/lon from city_results.csv). The GPKG NUTS3_2021 field is NOT
#      used — it contains errors for some cities.
#   2. Download the configured wealth metric from Eurostat (set active_metric
#      in the config block; default: net disposable household income at NUTS2).
#      NUTS2 codes are derived from NUTS3 by truncating to 4 characters.
#   3. Derive city-specific B-spline knots (p10, p75, p90) from the ERA5
#      historical series (1990-2019). No projection data needed for clustering.
#   4. Cluster cities by knot triplet via k-means.
#   5. Compute the mortality multiplier for each city as:
#         multiplier = avg_RR(2031-2060, SSP) / avg_RR(1990-2019 ERA5 baseline)
#      Computed per GCM separately (19 GCMs), then summarised as mean + 5th/95th
#      percentile across GCMs. Both SSP2-4.5 and SSP3-7.0 are processed.
#      Split by component: heat (above MMT), cold (below MMT), total.
#   6. Diagnostic plots: cluster geography, knot distributions, elbow.
#   7. Scatter plots: wealth metric vs multiplier (heat/cold/total),
#      faceted by cluster with fixed y-scale across panels.
#
# Inputs:
#   data/city_results.csv               city coordinates, population
#   data/coefs.csv                      B-spline coefficients (Masselot 2023)
#   data/era5series.gz.parquet          daily ERA5 temperatures 1990-2019
#   data/tmeanproj.gz.parquet           CMIP6 daily projections
#   data/CNTR_RG_20M_2020_4326.geojson  country boundaries for map background
#
# Outputs:
#   results_csv/nuts_city_map.csv
#   results_csv/nuts_<metric_id>.csv          wealth metric values per NUTS region
#   results_csv/city_knots.csv
#   results_csv/city_clusters.csv
#   results_csv/city_multipliers.csv        mean/p05/p95 across GCMs, per city/SSP/component/age
#   results_csv/city_multipliers_gcm.csv    per-GCM multipliers (age-group mean, for spread plots)
#   plots/nuts_gdp_vs_multiplier.pdf
#
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(dplyr)
  library(sf)
  library(eurostat)
  library(dlnm)
  library(splines)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
})

source("R/utils.R")
source("R/rr_basis.R")

# ── Configuration ─────────────────────────────────────────────────────────────

varfun    <- "bs"
vardegree <- 2
varper    <- c(10, 75, 90)   # Masselot knot percentiles
gcmexcl   <- c("CMCC_CM2_SR5", "TaiESM1")

agelabs       <- c("20-44", "45-64", "65-74", "75-84", "85+")
age_midpoints <- c(32.5, 55, 70, 80, 92.5)
age_range     <- 20:100

baseline_years  <- 1990:2019
focus_ssps      <- c("2", "3")   # SSP2-4.5 and SSP3-7.0
focus_period    <- 2031:2060     # 30-year window (symmetric with 1990-2019 baseline)
focus_agegroup  <- "65-74"
components      <- c("heat", "cold", "total")

# ── Wealth metric configuration ───────────────────────────────────────────────
# Set active_metric to switch between metrics:
#   "hh_income"  Net disposable household income per capita (NUTS2, nama_10r_2hhinc)
#   "gdp"        GDP per inhabitant (NUTS3, nama_10r_3gdp)
active_metric <- "hh_income"

metric_configs <- list(
  hh_income = list(
    id         = "hh_income",
    label      = "Net disposable household income per capita",
    short      = "Household income",
    dataset    = "nama_10r_2hhinc",
    na_item    = "B6N",           # balance of net disposable income
    unit       = "PPS_EU27_2020_HAB",
    nuts_chars = 4L,              # NUTS2 = 4-character geo code
    nuts_col   = "nuts2",
    ref_year   = 2019,
    caption    = "Income: Eurostat nama_10r_2hhinc (B6N), net disposable household income per capita, PPS at EU27 2020 price level (https://ec.europa.eu/eurostat)"
  ),
  gdp = list(
    id         = "gdp",
    label      = "GDP per inhabitant",
    short      = "GDP",
    dataset    = "nama_10r_3gdp",
    na_item    = NULL,            # GDP dataset has no na_item dimension
    unit       = "PPS_EU27_2020_HAB",
    nuts_chars = 5L,              # NUTS3 = 5-character geo code
    nuts_col   = "nuts3",
    ref_year   = 2019,
    caption    = "GDP: Eurostat nama_10r_3gdp, PPS per inhabitant at EU27 2020 price level (https://ec.europa.eu/eurostat)"
  )
)
metric <- metric_configs[[active_metric]]

n_clusters      <- 4

if (!dir.exists("results_csv")) dir.create("results_csv")
if (!dir.exists("plots"))       dir.create("plots")

ssp_labels <- c("1" = "SSP1-2.6", "2" = "SSP2-4.5", "3" = "SSP3-7.0",
                "hist" = "Historical")

# ── Step 1: NUTS3 mapping via spatial join ───────────────────────────────────

cat_header(sprintf("%s vs Mortality Multiplier Analysis", metric$short))
cat_step(1, "Mapping cities to NUTS3 via spatial join")

coefs_all    <- fread("data/coefs.csv")
city_results <- fread("data/city_results.csv")
city_meta    <- unique(city_results[, .(URAU_CODE, LABEL, CNTR_CODE, lon, lat, pop)])

all_city_codes <- unique(coefs_all$URAU_CODE)
cat(sprintf("  %d cities with coefficients\n", length(all_city_codes)))

# Spatial join: city points → NUTS3 polygons (Eurostat, 2021)
# This is the authoritative mapping — the GPKG NUTS3_2021 field has known errors.
cat("  Downloading NUTS3 polygons from Eurostat...\n")
nuts3_sf <- get_eurostat_geospatial(resolution = "20", nuts_level = 3,
                                    year = 2021, make_valid = TRUE)

city_pts <- st_as_sf(city_meta[URAU_CODE %in% all_city_codes],
                     coords = c("lon", "lat"), crs = 4326, remove = FALSE)

do_nuts_join <- function(city_pts_sf, nuts_sf, id_col = "nuts3") {
  joined <- st_join(
    st_transform(city_pts_sf, st_crs(nuts_sf)),
    nuts_sf[, "NUTS_ID"],
    join = st_within
  )
  dt <- as.data.table(st_drop_geometry(joined))[, .(URAU_CODE, nuts_id = NUTS_ID)]
  setnames(dt, "nuts_id", id_col)

  # Fallback for cities outside any polygon (border/island edge cases):
  # use nearest polygon centroid
  missed <- dt[is.na(get(id_col)), URAU_CODE]
  if (length(missed) > 0) {
    cat(sprintf("    %d cities outside polygon — using nearest centroid\n",
                length(missed)))
    nn <- st_nearest_feature(
      st_transform(city_pts_sf[city_pts_sf$URAU_CODE %in% missed, ], st_crs(nuts_sf)),
      nuts_sf
    )
    dt[URAU_CODE %in% missed, (id_col) := nuts_sf$NUTS_ID[nn]]
  }
  dt
}

nuts_map <- do_nuts_join(city_pts, nuts3_sf, "nuts3")

cat(sprintf("  Mapped %d cities to %d NUTS3 regions\n",
            nrow(nuts_map), uniqueN(nuts_map$nuts3)))
fwrite(nuts_map, "results_csv/nuts_city_map.csv")
nuts_map[, nuts2 := substr(nuts3, 1, 4)]  # NUTS2 derived from NUTS3 by truncation

# ── Step 2: Wealth metric from Eurostat ───────────────────────────────────────

cat_step(2, sprintf("Downloading %s (%s)", metric$label, metric$dataset))

get_metric_data <- function(cfg) {
  raw <- tryCatch(
    get_eurostat(cfg$dataset, cache = TRUE),
    error = function(e) { cat(sprintf("    ERROR: %s\n", e$message)); NULL }
  )
  if (is.null(raw)) return(NULL)
  dt <- as.data.table(raw)
  # Standardise geo/value column names across Eurostat datasets
  if ("geo"    %in% names(dt)) setnames(dt, "geo",    "region")
  if ("values" %in% names(dt)) setnames(dt, "values", "metric_val")
  if ("value"  %in% names(dt) && !"metric_val" %in% names(dt))
    setnames(dt, "value", "metric_val")
  # Filter: unit, NUTS level (by code length), year
  dt <- dt[unit == cfg$unit & nchar(region) == cfg$nuts_chars &
           format(TIME_PERIOD, "%Y") == as.character(cfg$ref_year)]
  # Optional na_item filter (needed for multi-indicator datasets like nama_10r_2hhinc)
  if (!is.null(cfg$na_item) && "na_item" %in% names(dt))
    dt <- dt[na_item == cfg$na_item]
  dt <- dt[!is.na(metric_val), .(region, metric_val)]
  setnames(dt, "region", cfg$nuts_col)
  cat(sprintf("  %s: %d %s regions\n", cfg$dataset, nrow(dt), toupper(cfg$nuts_col)))
  dt
}

metric_dt <- get_metric_data(metric)
fwrite(metric_dt, sprintf("results_csv/nuts_%s.csv", metric$id))

# ── Steps 3–6: ERA5 knots, clustering, multipliers ───────────────────────────
# Cache: if pre-computed CSVs exist, skip the heavy computation

cache_files <- c("results_csv/city_knots.csv", "results_csv/city_clusters.csv",
                 "results_csv/city_multipliers.csv", "results_csv/city_multipliers_gcm.csv")

if (all(file.exists(cache_files))) {
  cat("  [CACHE] Pre-computed CSVs found — skipping Steps 3-6\n")
  knots_dt         <- fread("results_csv/city_knots.csv")
  feat_dt          <- fread("results_csv/city_clusters.csv")
  feat_dt[, cluster := factor(cluster, levels = paste0("C", 1:n_clusters))]
  city_multipliers <- fread("results_csv/city_multipliers.csv")
  gcm_spread_dt    <- fread("results_csv/city_multipliers_gcm.csv")
  # Recover gcm_cols count from saved per-GCM data
  gcm_cols <- unique(gcm_spread_dt$gcm)
  cat(sprintf("  Loaded %d cities, %d GCMs\n",
              uniqueN(city_multipliers$URAU_CODE), length(gcm_cols)))
  # Rebuild elbow_df and feat_mat/era5_lookup stubs (not needed for plotting)
  feat_mat     <- NULL
  era5_lookup  <- list()
  elbow_df     <- data.frame(k = integer(0), wss = numeric(0))
} else {

# ── Step 3: ERA5 knots (p10, p75, p90) per city ──────────────────────────────

cat_step(3, "Computing knots from ERA5 series (bulk load)")

era5 <- open_dataset("data/era5series.gz.parquet") %>%
  collect() %>% as.data.table()
cat(sprintf("  ERA5: %d rows, %d cities\n", nrow(era5), uniqueN(era5$URAU_CODE)))

knots_dt <- era5[, {
  q <- quantile(era5landtmean, varper / 100, na.rm = TRUE)
  .(p10 = q[1], p75 = q[2], p90 = q[3])
}, by = URAU_CODE]
cat(sprintf("  Knots computed for %d cities\n", nrow(knots_dt)))
fwrite(knots_dt, "results_csv/city_knots.csv")

era5_lookup <- split(era5$era5landtmean, era5$URAU_CODE)
rm(era5); invisible(gc())

# ── Step 4: Cluster cities by knots ──────────────────────────────────────────

cat_step(4, sprintf("Clustering cities by knots (k-means, k=%d)", n_clusters))

feat_dt  <- knots_dt[complete.cases(knots_dt)]
feat_mat <- scale(as.matrix(feat_dt[, .(p10, p75, p90)]))
rownames(feat_mat) <- feat_dt$URAU_CODE

set.seed(42)
wss_vec <- sapply(2:10, function(k)
  kmeans(feat_mat, centers = k, nstart = 25, iter.max = 100)$tot.withinss)
elbow_df <- data.frame(k = 2:10, wss = wss_vec)

km <- kmeans(feat_mat, centers = n_clusters, nstart = 50, iter.max = 200)
feat_dt[, cluster_raw := km$cluster]

# Re-label cold → warm by median p75 → C1 (coldest) to C4 (warmest)
med_ord <- feat_dt[, .(med = median(p75)), by = cluster_raw]
setorder(med_ord, med)
med_ord[, cluster := factor(paste0("C", seq_len(.N)),
                            levels = paste0("C", 1:n_clusters))]
feat_dt <- merge(feat_dt, med_ord[, .(cluster_raw, cluster)], by = "cluster_raw")

cat("  Cluster sizes:\n"); print(table(feat_dt$cluster))
fwrite(feat_dt[, .(URAU_CODE, p10, p75, p90, cluster)],
       "results_csv/city_clusters.csv")

# ── Step 5: Load temperatures in bulk ────────────────────────────────────────

cat_step(5, "Loading projected temperatures in bulk")

ds_proj      <- open_dataset("data/tmeanproj.gz.parquet")
gcm_cols_all <- names(ds_proj)[grepl("^tas_", names(ds_proj))]
gcm_cols     <- gcm_cols_all[!gsub("tas_", "", gcm_cols_all) %in% gcmexcl]
cat(sprintf("  Using %d GCMs; period %d-%d\n",
            length(gcm_cols), min(focus_period), max(focus_period)))

# ── Step 6: Compute mortality multipliers per city, per SSP, per GCM ─────────

cat_step(6, sprintf("Computing mortality multipliers (%d cities × %d SSPs × %d GCMs)",
                    length(all_city_codes), length(focus_ssps), length(gcm_cols)))

all_multipliers  <- vector("list", length(focus_ssps))
all_gcm_spread   <- vector("list", length(focus_ssps))

for (si in seq_along(focus_ssps)) {
  ssp <- focus_ssps[si]
  cat(sprintf("\n  == SSP%s: loading %d years ==\n", ssp, length(focus_period)))

  proj_ssp <- ds_proj %>%
    filter(ssp == !!ssp, year(date) %in% !!focus_period) %>%
    select(c("URAU_CODE", "date", all_of(gcm_cols))) %>%
    collect() %>% as.data.table()
  cat(sprintf("  SSP%s: %d rows, %d cities\n",
              ssp, nrow(proj_ssp), uniqueN(proj_ssp$URAU_CODE)))

  proj_lookup <- split(proj_ssp, proj_ssp$URAU_CODE)
  rm(proj_ssp); invisible(gc())

  results_ssp  <- vector("list", length(all_city_codes))
  gcm_ssp      <- vector("list", length(all_city_codes))

  for (ci in seq_along(all_city_codes)) {
    city <- all_city_codes[ci]

    krow       <- knots_dt[URAU_CODE == city]
    era5_temps <- era5_lookup[[city]]
    coefs_city <- coefs_all[URAU_CODE == city]
    proj_rows  <- proj_lookup[[city]]

    if (nrow(krow) == 0L || is.null(era5_temps) || length(era5_temps) < 50 ||
        nrow(coefs_city) == 0L || is.null(proj_rows) || nrow(proj_rows) == 0L) next

    city_knots <- c(krow$p10, krow$p75, krow$p90)
    city_bound <- range(era5_temps, na.rm = TRUE)
    argvar <- list(fun = varfun, degree = vardegree,
                   knots = city_knots, Bound = city_bound)

    rr_res <- tryCatch(
      compute_rr_curves(coefs_city, agelabs, age_midpoints, argvar, city_bound),
      error = function(e) NULL
    )
    if (is.null(rr_res)) next

    rr_interp <- interpolate_rr_to_single_age(
      rr_res$rr_matrix, rr_res$mmt_vec, age_midpoints, age_range
    )

    # Baseline RR (ERA5, same for all SSPs/GCMs) — compute once per component
    base_rr_list <- lapply(components, function(comp)
      compute_avg_rr_by_age(
        era5_temps, rr_res$temp_seq,
        rr_interp$rr_single_age, rr_interp$mmt_single_age,
        age_range, component = comp
      )
    )
    names(base_rr_list) <- components

    # Per-GCM future RR
    gcm_rows <- lapply(gcm_cols, function(gcm) {
      gcm_temps <- na.omit(proj_rows[[gcm]])
      if (length(gcm_temps) < 50) return(NULL)
      comp_rows <- lapply(components, function(comp) {
        fut_rr <- compute_avg_rr_by_age(
          gcm_temps, rr_res$temp_seq,
          rr_interp$rr_single_age, rr_interp$mmt_single_age,
          age_range, component = comp
        )
        data.table(
          gcm        = gcm,
          component  = comp,
          age        = age_range,
          multiplier = fut_rr / base_rr_list[[comp]]
        )
      })
      rbindlist(comp_rows)
    })
    gcm_dt <- rbindlist(Filter(Negate(is.null), gcm_rows))
    if (nrow(gcm_dt) == 0L) next

    # Aggregate across GCMs → mean + 5th/95th percentile
    city_multi <- gcm_dt[, .(
      multiplier     = mean(multiplier, na.rm = TRUE),
      multiplier_p05 = quantile(multiplier, 0.05, na.rm = TRUE),
      multiplier_p95 = quantile(multiplier, 0.95, na.rm = TRUE),
      n_gcm          = sum(!is.na(multiplier))
    ), by = .(component, age)]
    city_multi[, `:=`(URAU_CODE = city, ssp = ssp)]
    results_ssp[[ci]] <- city_multi

    # Per-GCM summary (age-group mean) for spread diagnostics
    age_lo2 <- as.integer(strsplit(focus_agegroup, "-")[[1]][1])
    age_hi2 <- as.integer(strsplit(focus_agegroup, "-")[[1]][2])
    focus_ages_inner <- age_range[age_range >= age_lo2 & age_range <= age_hi2]
    gcm_summary <- gcm_dt[age %in% focus_ages_inner,
                           .(multiplier = mean(multiplier, na.rm = TRUE)),
                           by = .(gcm, component)]
    gcm_summary[, `:=`(URAU_CODE = city, ssp = ssp)]
    gcm_ssp[[ci]] <- gcm_summary

    if (ci %% 100 == 0)
      cat(sprintf("  SSP%s [%d/%d] done\n", ssp, ci, length(all_city_codes)))
  }

  all_multipliers[[si]] <- rbindlist(results_ssp)
  all_gcm_spread[[si]]  <- rbindlist(gcm_ssp)
  rm(proj_lookup); invisible(gc())
}

city_multipliers <- rbindlist(all_multipliers)
gcm_spread_dt    <- rbindlist(all_gcm_spread)
cat(sprintf("  Multipliers for %d cities × %d SSPs\n",
            uniqueN(city_multipliers$URAU_CODE), uniqueN(city_multipliers$ssp)))
fwrite(city_multipliers, "results_csv/city_multipliers.csv")
fwrite(gcm_spread_dt,    "results_csv/city_multipliers_gcm.csv")

} # end else (cache miss — steps 3-6 complete)

# ── Step 7: Assemble plot datasets ────────────────────────────────────────────

cat_step(7, "Assembling plot datasets")

age_lo <- as.integer(strsplit(focus_agegroup, "-")[[1]][1])
age_hi <- as.integer(strsplit(focus_agegroup, "-")[[1]][2])
focus_ages <- age_range[age_range >= age_lo & age_range <= age_hi]

# Average multiplier (mean across GCMs) for the focus age group, per city/SSP/component
multi_city <- city_multipliers[age %in% focus_ages,
                               .(multiplier     = mean(multiplier,     na.rm = TRUE),
                                 multiplier_p05 = mean(multiplier_p05, na.rm = TRUE),
                                 multiplier_p95 = mean(multiplier_p95, na.rm = TRUE)),
                               by = .(URAU_CODE, ssp, component)]

make_plot_dt <- function(gdp_col_name, gdp_table, by_col) {
  base <- merge(multi_city,
                nuts_map[, c("URAU_CODE", by_col), with = FALSE],
                by = "URAU_CODE")
  base <- merge(base,
                feat_dt[, .(URAU_CODE, p10, p75, p90, cluster)],
                by = "URAU_CODE")
  base <- merge(base, city_meta[, .(URAU_CODE, LABEL, CNTR_CODE)],
                by = "URAU_CODE")
  gdp_tmp <- copy(gdp_table)
  setnames(gdp_tmp, c(by_col, "gdp_pps"))
  base <- merge(base, gdp_tmp, by = by_col, all.x = TRUE)
  base[!is.na(gdp_pps) & !is.na(multiplier)]
}

plot_main <- make_plot_dt(metric$nuts_col, metric_dt, metric$nuts_col)

cat(sprintf("  %s: %d city×SSP rows with complete data\n",
            toupper(metric$nuts_col), nrow(plot_main)))

# ── Step 8: Plotting helpers ──────────────────────────────────────────────────

cat_step(8, "Generating plots")

cluster_colors <- c(C1 = "#2166AC", C2 = "#92C5DE", C3 = "#F4A582", C4 = "#D6604D")

clust_labels_fn <- function(dt) {
  meta <- dt[, .(med_p75 = median(p75), n = .N), by = cluster]
  setorder(meta, cluster)
  setNames(
    sprintf("%s  (median T75 = %.1f°C, n = %d)", meta$cluster, meta$med_p75, meta$n),
    as.character(meta$cluster)
  )
}

make_metric_label <- function() {
  sprintf(
    "%s — %s level (%d)\nPPS, calibrated to EU27 2020 price level",
    metric$label, toupper(metric$nuts_col), metric$ref_year
  )
}

# ── Data sources (used in plot captions) ─────────────────────────────────────
src_era5  <- "Temperature: ERA5 reanalysis, Copernicus Climate Change Service (https://cds.climate.copernicus.eu/)"
src_gdp   <- metric$caption
src_proj  <- "Projections: CORDEX-EUR-11 GCM ensemble via ESGF (https://esgf-node.llnl.gov/)"
src_coefs <- "Mortality dose-response: Masselot et al. (2023) Lancet Planet Health, doi:10.1016/S2542-5196(22)00069-2"
src_bound <- "Boundaries: Eurostat GISCO (https://ec.europa.eu/eurostat/web/gisco)"

# ── CLUSTER DIAGNOSTIC PLOTS ──────────────────────────────────────────────────

# Load Europe background
euro_sf <- tryCatch(
  st_read("data/CNTR_RG_20M_2020_4326.geojson", quiet = TRUE),
  error = function(e) NULL
)

# A. Map of cluster assignments
feat_geo <- merge(feat_dt[, .(URAU_CODE, cluster)],
                  city_meta[, .(URAU_CODE, lon, lat)], by = "URAU_CODE")

p_map <- ggplot() +
  {if (!is.null(euro_sf))
    geom_sf(data = euro_sf, fill = "grey93", colour = "grey70", linewidth = 0.2)
  } +
  geom_point(data = feat_geo,
             aes(lon, lat, colour = cluster), size = 1.2, alpha = 0.8) +
  scale_colour_manual(values = cluster_colors,
                      labels = clust_labels_fn(
                        merge(feat_dt, city_meta, by = "URAU_CODE")
                      ),
                      name = "Temperature cluster") +
  coord_sf(xlim = c(-12, 35), ylim = c(34, 72), expand = FALSE) +
  labs(title = "Geographic distribution of temperature clusters",
       subtitle = sprintf(
         "k-means (k=%d) on ERA5 p10/p75/p90 per city, 1990-2019", n_clusters
       ),
       caption = paste(src_era5, src_bound, sep = "\n")) +
  theme_bw(base_size = 10) +
  theme(legend.position = "right",
        axis.title   = element_blank(),
        plot.title   = element_text(face = "bold"),
        plot.caption = element_text(size = 7, colour = "grey40", hjust = 0))

# B. Pairwise scatter of knot percentiles coloured by cluster
pair_data <- feat_dt[, .(URAU_CODE, p10, p75, p90, cluster)]

# Shared temperature range across all three knot variables
temp_range <- range(c(pair_data$p10, pair_data$p75, pair_data$p90), na.rm = TRUE)

pair_scale <- function(xvar, yvar, xl, yl)
  ggplot(pair_data, aes(.data[[xvar]], .data[[yvar]], colour = cluster)) +
    geom_point(size = 1.5, alpha = 0.7) +
    scale_colour_manual(values = cluster_colors, guide = "none") +
    scale_x_continuous(name = xl, limits = temp_range) +
    scale_y_continuous(name = yl, limits = temp_range) +
    theme_bw(base_size = 9)

p_p10_p75 <- pair_scale("p10", "p75", "p10 (°C)", "p75 (°C)")
p_p75_p90 <- pair_scale("p75", "p90", "p75 (°C)", "p90 (°C)")
p_p10_p90 <- pair_scale("p10", "p90", "p10 (°C)", "p90 (°C)")

# Add shared colour legend via a dummy plot
dummy_leg <- ggplot(pair_data, aes(p10, p75, colour = cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_colour_manual(values = cluster_colors,
                      labels = clust_labels_fn(
                        merge(feat_dt, city_meta, by = "URAU_CODE")
                      ),
                      name = "Cluster") +
  theme_void() +
  theme(legend.position = "right")
leg <- cowplot::get_legend(dummy_leg)

p_pairs <- (p_p10_p75 | p_p75_p90 | p_p10_p90) +
  plot_annotation(
    title   = "Pairwise scatter of ERA5 knot percentiles by cluster",
    subtitle = "Axes = daily temperature percentiles (1990-2019)",
    caption  = src_era5,
    theme    = theme(plot.title   = element_text(face = "bold", size = 11),
                     plot.caption = element_text(size = 7, colour = "grey40",
                                                 hjust = 0))
  )

# C. Boxplots of each knot percentile per cluster
box_long <- melt(feat_dt[, .(URAU_CODE, cluster, p10, p75, p90)],
                 id.vars = c("URAU_CODE", "cluster"),
                 variable.name = "percentile", value.name = "temp_C")
box_long[, percentile := factor(percentile, levels = c("p10", "p75", "p90"),
                                labels = c("p10 (cold extreme)",
                                           "p75 (warm moderate)",
                                           "p90 (heat extreme)"))]

p_box <- ggplot(box_long, aes(cluster, temp_C, fill = cluster)) +
  geom_boxplot(outlier.size = 0.6, alpha = 0.8) +
  scale_fill_manual(values = cluster_colors, guide = "none") +
  facet_wrap(~percentile, scales = "fixed") +
  labs(x = "Cluster (C1 = coldest → C4 = warmest)",
       y = "Temperature (°C)",
       title    = "Knot percentile distributions per cluster",
       subtitle = "ERA5 1990-2019 historical series; each point = one city",
       caption  = src_era5) +
  theme_bw(base_size = 10) +
  theme(strip.background = element_rect(fill = "grey92"),
        plot.title   = element_text(face = "bold"),
        plot.caption = element_text(size = 7, colour = "grey40", hjust = 0))

# D. Elbow plot
if (nrow(elbow_df) > 0) {
  p_elbow <- ggplot(elbow_df, aes(k, wss)) +
    geom_line(colour = "steelblue") +
    geom_point(colour = "steelblue", size = 2.5) +
    geom_vline(xintercept = n_clusters, linetype = "dashed", colour = "firebrick") +
    annotate("text", x = n_clusters + 0.15, y = max(elbow_df$wss) * 0.95,
             label = sprintf("k = %d (chosen)", n_clusters),
             hjust = 0, colour = "firebrick", size = 3.5) +
    scale_x_continuous(breaks = 2:10) +
    labs(x = "k", y = "Total within-cluster SS",
         title   = "Elbow plot — k-means on city ERA5 knots (p10, p75, p90)",
         caption = src_era5) +
    theme_bw(base_size = 10) +
    theme(plot.caption = element_text(size = 7, colour = "grey40", hjust = 0))
} else {
  p_elbow <- ggplot() + labs(title = "Elbow plot (cached run — not available)")
}

# ── GDP vs MULTIPLIER SCATTER PLOTS ───────────────────────────────────────────

period_lbl <- sprintf("%d-%d average", min(focus_period), max(focus_period))

make_scatter_pages <- function(plot_dt, comp, ssp_val) {
  sub_dt <- plot_dt[component == comp & ssp == ssp_val]
  if (nrow(sub_dt) == 0) return(invisible(NULL))

  nuts_lbl <- toupper(metric$nuts_col)
  clbl     <- clust_labels_fn(sub_dt)
  x_lab    <- make_metric_label()
  ssp_lbl  <- ssp_labels[ssp_val]
  y_lab <- sprintf(
    "%s-mortality multiplier\n(%s, %s vs 1990-2019 ERA5, age %s)",
    tools::toTitleCase(comp), ssp_lbl, period_lbl, focus_agegroup
  )
  caption_txt <- paste(
    paste0("Multiplier = mean RR(", ssp_lbl, " ", period_lbl, ", ",
           length(gcm_cols), " GCMs) / mean RR(1990-2019 ERA5 baseline)",
           "  |  Error bars = 5th\u201395th pct across GCMs"),
    src_era5, src_proj, src_gdp, src_coefs,
    sep = "\n"
  )

  y_range <- range(c(sub_dt$multiplier_p05, sub_dt$multiplier_p95), na.rm = TRUE)
  y_lims  <- c(floor(y_range[1] * 20) / 20, ceiling(y_range[2] * 20) / 20)

  # Plot A: all cities combined
  p_all <- ggplot(sub_dt, aes(gdp_pps, multiplier,
                               colour = cluster, label = LABEL)) +
    geom_linerange(aes(ymin = multiplier_p05, ymax = multiplier_p95),
                   alpha = 0.2, linewidth = 0.3) +
    geom_point(size = 1.8, alpha = 0.75) +
    geom_smooth(aes(group = cluster), method = "lm", se = TRUE,
                linewidth = 0.7, alpha = 0.12) +
    geom_text_repel(size = 1.8, max.overlaps = 15, segment.colour = "grey60") +
    scale_colour_manual(values = cluster_colors, labels = clbl,
                        name = "Temperature\ncluster") +
    scale_x_continuous(name = x_lab) +
    scale_y_continuous(name = y_lab, limits = y_lims) +
    geom_hline(yintercept = 1, linetype = "dotted", colour = "grey40") +
    labs(
      title = sprintf(
        "%s vs %s-mortality multiplier — all cities (%s, %s)",
        metric$short, comp, nuts_lbl, ssp_lbl),
      subtitle = sprintf(
        "%s at %s level (spatial join). Clusters = k-means on ERA5 p10/p75/p90, k=%d. Error bars = 5th\u201395th pct across GCMs.",
        metric$short, nuts_lbl, n_clusters),
      caption = caption_txt
    ) +
    theme_bw(base_size = 11) +
    theme(plot.title    = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 8, colour = "grey40"),
          plot.caption  = element_text(size = 7, colour = "grey40", hjust = 0))

  # Plot B: faceted by cluster — FIXED y-scale across panels
  p_facet <- ggplot(sub_dt, aes(gdp_pps, multiplier,
                                 colour = cluster, label = LABEL)) +
    geom_linerange(aes(ymin = multiplier_p05, ymax = multiplier_p95),
                   alpha = 0.25, linewidth = 0.3) +
    geom_point(size = 1.8, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE,
                linewidth = 0.8, alpha = 0.15, colour = "black") +
    geom_text_repel(size = 1.6, max.overlaps = 10, segment.colour = "grey70") +
    geom_hline(yintercept = 1, linetype = "dotted", colour = "grey40") +
    scale_colour_manual(values = cluster_colors, guide = "none") +
    scale_x_continuous(name = x_lab) +
    scale_y_continuous(name = y_lab, limits = y_lims) +
    facet_wrap(~cluster, labeller = as_labeller(clbl), scales = "fixed") +
    labs(
      title    = sprintf("%s vs %s-mortality multiplier by cluster (%s, %s)",
                         metric$short, comp, nuts_lbl, ssp_lbl),
      subtitle = "Fixed y-scale across panels. Black band = OLS 95% CI.",
      caption  = caption_txt
    ) +
    theme_bw(base_size = 10) +
    theme(strip.background = element_rect(fill = "grey92"),
          plot.title   = element_text(face = "bold"),
          plot.caption = element_text(size = 7, colour = "grey40", hjust = 0))

  # Per-cluster individual plots
  clusters_present <- sort(unique(sub_dt$cluster))
  p_clusters <- lapply(clusters_present, function(cl) {
    cl_dt  <- sub_dt[cluster == cl]
    cl_col <- cluster_colors[cl]
    cl_lbl <- clbl[cl]
    ggplot(cl_dt, aes(gdp_pps, multiplier, label = LABEL)) +
      geom_linerange(aes(ymin = multiplier_p05, ymax = multiplier_p95),
                     alpha = 0.25, linewidth = 0.4, colour = cl_col) +
      geom_point(size = 2.5, alpha = 0.8, colour = cl_col) +
      geom_smooth(method = "lm", se = TRUE,
                  linewidth = 0.9, alpha = 0.15, colour = "black") +
      geom_text_repel(size = 3.2, max.overlaps = 25, segment.colour = "grey60",
                      segment.size = 0.3) +
      geom_hline(yintercept = 1, linetype = "dotted", colour = "grey40") +
      scale_x_continuous(name = x_lab) +
      scale_y_continuous(name = y_lab, limits = y_lims) +
      labs(
        title    = sprintf("%s vs %s-mortality multiplier \u2014 %s (%s, %s)",
                           metric$short, comp, cl_lbl, nuts_lbl, ssp_lbl),
        subtitle = "OLS trend with 95% CI shown.",
        caption  = caption_txt
      ) +
      theme_bw(base_size = 13) +
      theme(plot.title   = element_text(face = "bold"),
            plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))
  })
  names(p_clusters) <- clusters_present

  list(all = p_all, facet = p_facet, clusters = p_clusters)
}

# SSP2-4.5 vs SSP3-7.0 direct comparison (one point per city)
make_ssp_comparison <- function(plot_dt, comp) {
  nuts_lbl <- toupper(metric$nuts_col)
  sub  <- plot_dt[component == comp, .(URAU_CODE, ssp, multiplier, cluster, LABEL)]
  wide <- dcast(sub, URAU_CODE + cluster + LABEL ~ ssp, value.var = "multiplier")
  setnames(wide, c("2", "3"), c("ssp2", "ssp3"))
  wide <- wide[!is.na(ssp2) & !is.na(ssp3)]
  if (nrow(wide) == 0) return(invisible(NULL))

  clbl <- clust_labels_fn(
    merge(wide, feat_dt[, .(URAU_CODE, p10, p75, p90)], by = "URAU_CODE")
  )
  lims <- range(c(wide$ssp2, wide$ssp3), na.rm = TRUE)
  lims <- c(floor(lims[1] * 20) / 20, ceiling(lims[2] * 20) / 20)

  ggplot(wide, aes(ssp2, ssp3, colour = cluster, label = LABEL)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(size = 1.8, alpha = 0.75) +
    geom_text_repel(size = 1.7, max.overlaps = 15, segment.colour = "grey60") +
    scale_colour_manual(values = cluster_colors, labels = clbl,
                        name = "Temperature\ncluster") +
    scale_x_continuous(name = "SSP2-4.5 multiplier", limits = lims) +
    scale_y_continuous(name = "SSP3-7.0 multiplier", limits = lims) +
    labs(
      title    = sprintf("SSP2-4.5 vs SSP3-7.0 %s-mortality multiplier (%s)",
                         comp, nuts_lbl),
      subtitle = sprintf(
        "Each point = one city. Diagonal = no scenario difference. %s, age %s.",
        period_lbl, focus_agegroup),
      caption  = paste(src_era5, src_proj, src_coefs, sep = "\n")
    ) +
    theme_bw(base_size = 11) +
    theme(plot.title   = element_text(face = "bold"),
          plot.caption = element_text(size = 7, colour = "grey40", hjust = 0))
}

# GCM uncertainty boxplots per cluster and scenario
make_gcm_spread_plot <- function(comp_val) {
  spread <- gcm_spread_dt[component == comp_val]
  spread <- merge(spread, feat_dt[, .(URAU_CODE, cluster)], by = "URAU_CODE")
  spread[, ssp_lbl := ssp_labels[ssp]]

  ggplot(spread, aes(cluster, multiplier, fill = ssp_lbl)) +
    geom_boxplot(alpha = 0.75, outlier.size = 0.4,
                 position = position_dodge(width = 0.8)) +
    geom_hline(yintercept = 1, linetype = "dotted", colour = "grey40") +
    scale_fill_manual(values = c("SSP2-4.5" = "#2C7BB6", "SSP3-7.0" = "#D7191C"),
                      name = "Scenario") +
    labs(
      x       = "Cluster (C1 = coldest \u2192 C4 = warmest)",
      y       = sprintf(
        "%s-mortality multiplier\n(per-GCM, %s, age %s)",
        tools::toTitleCase(comp_val), period_lbl, focus_agegroup),
      title   = sprintf(
        "GCM uncertainty in %s-mortality multiplier by cluster", comp_val),
      subtitle = sprintf(
        "Each box = distribution across %d GCMs \u00d7 cities in cluster | Both scenarios",
        length(gcm_cols)),
      caption  = paste(src_era5, src_proj, src_coefs, sep = "\n")
    ) +
    theme_bw(base_size = 11) +
    theme(plot.title   = element_text(face = "bold"),
          plot.caption = element_text(size = 7, colour = "grey40", hjust = 0))
}

# ── Collect all plots with names ──────────────────────────────────────────────

all_plots <- list(
  cluster_map        = p_map,
  cluster_knot_pairs = p_pairs,
  cluster_knot_boxes = p_box,
  cluster_elbow      = p_elbow
)

# Per-SSP scatter plots
for (ssp_val in focus_ssps) {
  for (comp in components) {
    pg  <- make_scatter_pages(plot_main, comp, ssp_val)
    sfx <- paste0("_ssp", ssp_val)
    if (!is.null(pg)) {
      all_plots[[paste0(metric$nuts_col, "_", comp, sfx, "_all")]]   <- pg$all
      all_plots[[paste0(metric$nuts_col, "_", comp, sfx, "_facet")]] <- pg$facet
      for (cl in names(pg$clusters)) {
        all_plots[[paste0(metric$nuts_col, "_", comp, sfx, "_", tolower(cl))]] <- pg$clusters[[cl]]
      }
    }
  }
}

# SSP comparison and GCM spread
for (comp in components) {
  p3 <- make_ssp_comparison(plot_main, comp)
  if (!is.null(p3)) all_plots[[paste0(metric$nuts_col, "_", comp, "_ssp_compare")]] <- p3
  all_plots[[paste0("gcm_spread_", comp)]] <- make_gcm_spread_plot(comp)
}

# ── Save PDF ──────────────────────────────────────────────────────────────────

out_pdf <- "plots/nuts_gdp_vs_multiplier.pdf"
pdf(out_pdf, width = 14, height = 9)
for (p in all_plots) print(p)
dev.off()
cat(sprintf("  Saved: %s\n", out_pdf))

# ── Save individual PNGs (300 dpi, publication quality) ───────────────────────

png_dir <- "plots/nuts_gdp_vs_multiplier_png"
if (!dir.exists(png_dir)) dir.create(png_dir)

for (nm in names(all_plots)) {
  out_png <- file.path(png_dir, paste0(nm, ".png"))
  ggsave(out_png, plot = all_plots[[nm]],
         width = 14, height = 9, units = "in",
         dpi = 300, device = "png", bg = "white")
}
cat(sprintf("  Saved %d PNGs to: %s/\n", length(all_plots), png_dir))

# ── Second pass: generate plots for all remaining metric configs ──────────────

for (extra_id in setdiff(names(metric_configs), active_metric)) {
  metric    <- metric_configs[[extra_id]]
  cat_header(sprintf("%s vs Mortality Multiplier Analysis (second pass)", metric$short))

  cat_step(2, sprintf("Downloading %s (%s)", metric$label, metric$dataset))
  metric_dt <- get_metric_data(metric)
  fwrite(metric_dt, sprintf("results_csv/nuts_%s.csv", metric$id))

  # Update data-source caption for this metric
  src_gdp <- metric$caption

  cat_step(7, "Assembling plot datasets")
  plot_main <- make_plot_dt(metric$nuts_col, metric_dt, metric$nuts_col)
  cat(sprintf("  %s: %d city×SSP rows with complete data\n",
              toupper(metric$nuts_col), nrow(plot_main)))

  cat_step(8, "Generating plots")
  extra_plots <- list()

  for (ssp_val in focus_ssps) {
    for (comp in components) {
      pg  <- make_scatter_pages(plot_main, comp, ssp_val)
      sfx <- paste0("_ssp", ssp_val)
      if (!is.null(pg)) {
        extra_plots[[paste0(metric$nuts_col, "_", comp, sfx, "_all")]]   <- pg$all
        extra_plots[[paste0(metric$nuts_col, "_", comp, sfx, "_facet")]] <- pg$facet
        for (cl in names(pg$clusters)) {
          extra_plots[[paste0(metric$nuts_col, "_", comp, sfx, "_", tolower(cl))]] <- pg$clusters[[cl]]
        }
      }
    }
  }
  for (comp in components) {
    p3 <- make_ssp_comparison(plot_main, comp)
    if (!is.null(p3)) extra_plots[[paste0(metric$nuts_col, "_", comp, "_ssp_compare")]] <- p3
    extra_plots[[paste0("gcm_spread_", comp, "_", metric$nuts_col)]] <- make_gcm_spread_plot(comp)
  }

  for (nm in names(extra_plots)) {
    out_png <- file.path(png_dir, paste0(nm, ".png"))
    ggsave(out_png, plot = extra_plots[[nm]],
           width = 14, height = 9, units = "in",
           dpi = 300, device = "png", bg = "white")
  }
  cat(sprintf("  Saved %d %s PNGs to: %s/\n",
              length(extra_plots), toupper(metric$nuts_col), png_dir))
}

cat_header("Analysis complete")
