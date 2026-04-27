################################################################################
#
# NUTS2/NUTS3 GDP vs Mortality Multiplier Analysis
#
# For each of the 854 NUTS3 Urban Audit cities (Masselot et al.):
#   1. Map each city to NUTS2 and NUTS3 via spatial join against Eurostat
#      NUTS polygons (city lat/lon from city_results.csv). The GPKG
#      NUTS3_2021 field is NOT used — it contains errors for some cities.
#   2. Download GDP per inhabitant at NUTS2 level (nama_10r_2gdp) and NUTS3
#      level (nama_10r_3gdp), unit PPS_EU27_2020_HAB (EU27 average = 100).
#   3. Derive city-specific B-spline knots (p10, p75, p90) from the ERA5
#      historical series (1990-2019). No projection data needed for clustering.
#   4. Cluster cities by knot triplet via k-means.
#   5. Compute the mortality multiplier for each city as:
#         multiplier = avg_RR(focus_year, SSP) / avg_RR(1990-2019 ERA5 baseline)
#      computed separately for heat, cold, and total components.
#      ERA5 and projected temperatures are loaded in bulk upfront.
#   6. Diagnostic plots: cluster geography, knot distributions, elbow.
#   7. Scatter plots: GDP (NUTS2 and NUTS3) vs multiplier (heat/cold/total),
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
#   results_csv/nuts2_gdp.csv
#   results_csv/nuts3_gdp.csv
#   results_csv/city_knots.csv
#   results_csv/city_clusters.csv
#   results_csv/city_multipliers.csv
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
focus_ssp       <- "3"        # SSP3-7.0
focus_year      <- 2050
focus_agegroup  <- "65-74"
components      <- c("heat", "cold", "total")

gdp_ref_year    <- 2019
gdp_unit        <- "PPS_EU27_2020_HAB"  # GDP/inhabitant, EU27 2020 = 100

n_clusters      <- 4

if (!dir.exists("results_csv")) dir.create("results_csv")
if (!dir.exists("plots"))       dir.create("plots")

ssp_labels <- c("1" = "SSP1-2.6", "2" = "SSP2-4.5", "3" = "SSP3-7.0",
                "hist" = "Historical")

# ── Step 1: NUTS2 and NUTS3 mapping via spatial join ─────────────────────────

cat_header("NUTS2/NUTS3 GDP vs Mortality Multiplier Analysis")
cat_step(1, "Mapping cities to NUTS2 and NUTS3 via spatial join")

coefs_all    <- fread("data/coefs.csv")
city_results <- fread("data/city_results.csv")
city_meta    <- unique(city_results[, .(URAU_CODE, LABEL, CNTR_CODE, lon, lat, pop)])

all_city_codes <- unique(coefs_all$URAU_CODE)
cat(sprintf("  %d cities with coefficients\n", length(all_city_codes)))

# Spatial join: city points → NUTS2 / NUTS3 polygons (Eurostat, 2021)
# This is the authoritative mapping — the GPKG NUTS3_2021 field has known errors.
cat("  Downloading NUTS polygons from Eurostat...\n")
nuts2_sf <- get_eurostat_geospatial(resolution = "20", nuts_level = 2,
                                    year = 2021, make_valid = TRUE)
nuts3_sf <- get_eurostat_geospatial(resolution = "20", nuts_level = 3,
                                    year = 2021, make_valid = TRUE)

city_pts <- st_as_sf(city_meta[URAU_CODE %in% all_city_codes],
                     coords = c("lon", "lat"), crs = 4326, remove = FALSE)

do_nuts_join <- function(city_pts_sf, nuts_sf, id_col = "nuts2") {
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

city_nuts2 <- do_nuts_join(city_pts, nuts2_sf, "nuts2")
city_nuts3 <- do_nuts_join(city_pts, nuts3_sf, "nuts3")
nuts_map   <- merge(city_nuts2, city_nuts3, by = "URAU_CODE")

cat(sprintf("  Mapped %d cities to %d NUTS2 and %d NUTS3 regions\n",
            nrow(nuts_map), uniqueN(nuts_map$nuts2), uniqueN(nuts_map$nuts3)))
fwrite(nuts_map, "results_csv/nuts_city_map.csv")

# ── Step 2: GDP per inhabitant from Eurostat ──────────────────────────────────

cat_step(2, "Downloading GDP per inhabitant (PPS_EU27_2020_HAB)")

get_gdp <- function(dataset, nuts_level_char) {
  raw <- tryCatch(
    get_eurostat(dataset, cache = TRUE),
    error = function(e) { cat(sprintf("    ERROR: %s\n", e$message)); NULL }
  )
  if (is.null(raw)) return(NULL)
  dt <- as.data.table(raw)
  setnames(dt, c("geo", "values"), c("region", "gdp_pps"), skip_absent = TRUE)
  if ("value" %in% names(dt) && !"gdp_pps" %in% names(dt))
    setnames(dt, "value", "gdp_pps")
  dt <- dt[unit == gdp_unit &
           nchar(region) == nchar(nuts_level_char) &
           format(TIME_PERIOD, "%Y") == as.character(gdp_ref_year),
           .(region, gdp_pps)]
  dt <- dt[!is.na(gdp_pps)]
  cat(sprintf("  %s: %d %s regions with GDP\n",
              dataset, uniqueN(dt$region), nuts_level_char))
  dt
}

gdp_nuts2_raw <- get_gdp("nama_10r_2gdp", "XXXX")   # 4-char NUTS2
gdp_nuts3_raw <- get_gdp("nama_10r_3gdp", "XXXXX")  # 5-char NUTS3

# Re-key to generic column names
gdp_nuts2 <- gdp_nuts2_raw[nchar(region) == 4]; setnames(gdp_nuts2, "region", "nuts2")
gdp_nuts3 <- gdp_nuts3_raw[nchar(region) == 5]; setnames(gdp_nuts3, "region", "nuts3")

fwrite(gdp_nuts2, "results_csv/nuts2_gdp.csv")
fwrite(gdp_nuts3, "results_csv/nuts3_gdp.csv")

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
cat(sprintf("  Using %d GCMs\n", length(gcm_cols)))

cat(sprintf("  Loading SSP%s year %d...\n", focus_ssp, focus_year))
proj_future <- ds_proj %>%
  filter(ssp == focus_ssp, year(date) == focus_year) %>%
  select(c("URAU_CODE", "date", all_of(gcm_cols))) %>%
  collect() %>% as.data.table()
cat(sprintf("  Future: %d rows, %d cities\n",
            nrow(proj_future), uniqueN(proj_future$URAU_CODE)))

proj_lookup <- split(proj_future, proj_future$URAU_CODE)
rm(proj_future); invisible(gc())

# ── Step 6: Compute mortality multipliers per city (all components) ───────────

cat_step(6, "Computing mortality multipliers for all cities and components")

results_list <- vector("list", length(all_city_codes))

for (ci in seq_along(all_city_codes)) {
  city <- all_city_codes[ci]

  krow       <- knots_dt[URAU_CODE == city]
  era5_temps <- era5_lookup[[city]]
  coefs_city <- coefs_all[URAU_CODE == city]
  fut_rows   <- proj_lookup[[city]]

  if (nrow(krow) == 0L || is.null(era5_temps) || length(era5_temps) < 50 ||
      nrow(coefs_city) == 0L || is.null(fut_rows) || nrow(fut_rows) == 0L) next

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

  fut_temps <- unlist(fut_rows[, ..gcm_cols], use.names = FALSE)
  fut_temps <- fut_temps[!is.na(fut_temps)]
  if (length(fut_temps) < 50) next

  city_rows <- lapply(components, function(comp) {
    base_rr <- compute_avg_rr_by_age(
      era5_temps, rr_res$temp_seq,
      rr_interp$rr_single_age, rr_interp$mmt_single_age,
      age_range, component = comp
    )
    fut_rr <- compute_avg_rr_by_age(
      fut_temps, rr_res$temp_seq,
      rr_interp$rr_single_age, rr_interp$mmt_single_age,
      age_range, component = comp
    )
    data.table(
      URAU_CODE   = city,
      component   = comp,
      age         = age_range,
      baseline_rr = base_rr,
      future_rr   = fut_rr,
      multiplier  = fut_rr / base_rr
    )
  })
  results_list[[ci]] <- rbindlist(city_rows)

  if (ci %% 100 == 0)
    cat(sprintf("  [%d/%d] done\n", ci, length(all_city_codes)))
}

city_multipliers <- rbindlist(results_list)
cat(sprintf("  Multipliers for %d cities\n",
            uniqueN(city_multipliers$URAU_CODE)))
fwrite(city_multipliers, "results_csv/city_multipliers.csv")

# ── Step 7: Assemble plot datasets ────────────────────────────────────────────

cat_step(7, "Assembling plot datasets")

age_lo <- as.integer(strsplit(focus_agegroup, "-")[[1]][1])
age_hi <- as.integer(strsplit(focus_agegroup, "-")[[1]][2])
focus_ages <- age_range[age_range >= age_lo & age_range <= age_hi]

multi_city <- city_multipliers[age %in% focus_ages,
                               .(multiplier = mean(multiplier, na.rm = TRUE)),
                               by = .(URAU_CODE, component)]

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

plot_nuts2 <- make_plot_dt("nuts2", gdp_nuts2, "nuts2")
plot_nuts3 <- make_plot_dt("nuts3", gdp_nuts3, "nuts3")

cat(sprintf("  NUTS2: %d cities; NUTS3: %d cities with complete data\n",
            uniqueN(plot_nuts2$URAU_CODE), uniqueN(plot_nuts3$URAU_CODE)))

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

make_gdp_label <- function(nuts_level) {
  sprintf(
    "GDP per inhabitant — %s level (%d)\nPPS index, EU27 2020 average = 100",
    nuts_level, gdp_ref_year
  )
}

# ── Data sources (used in plot captions) ─────────────────────────────────────
src_era5  <- "Temperature: ERA5 reanalysis, Copernicus Climate Change Service (https://cds.climate.copernicus.eu/)"
src_gdp   <- "GDP: Eurostat nama_10r_2gdp / nama_10r_3gdp, PPS per inhabitant (https://ec.europa.eu/eurostat)"
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

# ── GDP vs MULTIPLIER SCATTER PLOTS ───────────────────────────────────────────

make_scatter_pages <- function(plot_dt, nuts_level, comp) {
  sub_dt <- plot_dt[component == comp]
  if (nrow(sub_dt) == 0) return(invisible(NULL))

  clbl  <- clust_labels_fn(sub_dt)
  x_lab <- make_gdp_label(nuts_level)
  y_lab <- sprintf("%s-mortality multiplier\n(SSP%s %d vs 1990-2019 ERA5, age %s)",
                   tools::toTitleCase(comp),
                   ssp_labels[focus_ssp], focus_year, focus_agegroup)
  caption_txt <- paste(
    paste0("Multiplier = mean RR(", focus_year, ", ", ssp_labels[focus_ssp],
           ") / mean RR(1990-2019 ERA5 baseline)"),
    src_era5, src_proj, src_gdp, src_coefs,
    sep = "\n"
  )

  y_range <- range(sub_dt$multiplier, na.rm = TRUE)
  y_lims  <- c(floor(y_range[1] * 20) / 20, ceiling(y_range[2] * 20) / 20)

  # Plot A: all cities combined
  p_all <- ggplot(sub_dt, aes(gdp_pps, multiplier,
                               colour = cluster, label = LABEL)) +
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
        "GDP vs %s-mortality multiplier — all cities (%s)", comp, nuts_level),
      subtitle = sprintf(
        "GDP at %s level (spatial join). Clusters = k-means on ERA5 p10/p75/p90, k=%d.",
        nuts_level, n_clusters),
      caption = caption_txt
    ) +
    theme_bw(base_size = 11) +
    theme(plot.title    = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 8, colour = "grey40"),
          plot.caption  = element_text(size = 7, colour = "grey40", hjust = 0))

  # Plot B: faceted by cluster — FIXED y-scale across panels
  p_facet <- ggplot(sub_dt, aes(gdp_pps, multiplier,
                                 colour = cluster, label = LABEL)) +
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
      title    = sprintf("GDP vs %s-mortality multiplier by cluster (%s)", comp, nuts_level),
      subtitle = "Fixed y-scale across panels. Black band = OLS 95% CI.",
      caption  = caption_txt
    ) +
    theme_bw(base_size = 10) +
    theme(strip.background = element_rect(fill = "grey92"),
          plot.title   = element_text(face = "bold"),
          plot.caption = element_text(size = 7, colour = "grey40", hjust = 0))

  list(all = p_all, facet = p_facet)
}

# ── Collect all plots with names ──────────────────────────────────────────────

all_plots <- list(
  cluster_map        = p_map,
  cluster_knot_pairs = p_pairs,
  cluster_knot_boxes = p_box,
  cluster_elbow      = p_elbow
)

for (comp in components) {
  pg2 <- make_scatter_pages(plot_nuts2, "NUTS2", comp)
  pg3 <- make_scatter_pages(plot_nuts3, "NUTS3", comp)
  if (!is.null(pg2)) {
    all_plots[[paste0("nuts2_", comp, "_all")]]   <- pg2$all
    all_plots[[paste0("nuts2_", comp, "_facet")]] <- pg2$facet
  }
  if (!is.null(pg3)) {
    all_plots[[paste0("nuts3_", comp, "_all")]]   <- pg3$all
    all_plots[[paste0("nuts3_", comp, "_facet")]] <- pg3$facet
  }
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

cat_header("Analysis complete")
