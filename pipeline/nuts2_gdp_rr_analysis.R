################################################################################
#
# NUTS2 GDP vs Mortality Multiplier Analysis
#
# For each of the 854 NUTS3 Urban Audit cities (Masselot et al.):
#   1. Map each city to its NUTS2 region and assign NUTS2-level GDP per
#      inhabitant from Eurostat (nama_10r_2gdp, PPS_EU27_2020_HAB, 2019).
#   2. Derive city-specific B-spline knots (p10, p75, p90) from the ERA5
#      historical series (era5series.gz.parquet, 1990-2019). No projection
#      data are needed for this step.
#   3. Cluster cities by their knot triplet using k-means.
#   4. Compute the mortality multiplier for each city as:
#         multiplier = avg_RR(target year, SSP) / avg_RR(historical baseline)
#      where avg_RR is the population-averaged RR over all days in the period,
#      using the city's own B-spline basis (coefs.csv + ERA5 knots).
#      All temperature data (ERA5 baseline + projections) are loaded in bulk
#      upfront to avoid per-city I/O.
#   5. Scatter-plot GDP per inhabitant vs mortality multiplier, coloured by
#      temperature cluster.
#
# Inputs:
#   data/urban_audit_cities_2020.gpkg   city → NUTS3 code
#   data/city_results.csv               city coordinates, population
#   data/coefs.csv                      B-spline coefficients (Masselot 2023)
#   data/era5series.gz.parquet          daily ERA5 temperatures 1990-2019
#   data/tmeanproj.gz.parquet           CMIP6 daily projections 1990-2099
#
# Outputs:
#   results_csv/nuts2_city_map.csv
#   results_csv/nuts2_gdp.csv
#   results_csv/city_knots.csv
#   results_csv/city_clusters.csv
#   results_csv/city_multipliers.csv
#   plots/nuts2_gdp_vs_multiplier.pdf
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
focus_ssp       <- "2"       # SSP2-4.5
focus_year      <- 2050
focus_agegroup  <- "65-74"
focus_comp      <- "total"   # "heat", "cold", or "total"

gdp_ref_year    <- 2019
gdp_unit        <- "PPS_EU27_2020_HAB"

n_clusters      <- 4

if (!dir.exists("results_csv")) dir.create("results_csv")
if (!dir.exists("plots"))       dir.create("plots")

# ── Step 1: City → NUTS2 mapping ─────────────────────────────────────────────

cat_header("NUTS2 GDP vs Mortality Multiplier Analysis")
cat_step(1, "Mapping cities to NUTS2")

coefs_all    <- fread("data/coefs.csv")
city_results <- fread("data/city_results.csv")
city_meta    <- unique(city_results[, .(URAU_CODE, LABEL, CNTR_CODE, lon, lat, pop)])

all_city_codes <- unique(coefs_all$URAU_CODE)
cat(sprintf("  %d cities with coefficients\n", length(all_city_codes)))

# Primary: NUTS3_2021 from Urban Audit GPKG, truncated to 4 chars = NUTS2
ua <- st_read("data/urban_audit_cities_2020.gpkg", quiet = TRUE)
ua_dt <- as.data.table(st_drop_geometry(ua))[, .(URAU_CODE, NUTS3_2021)]
ua_dt[, URAU_CODE_short := sub("(C)[0-9]+$", "\\1", URAU_CODE)]
ua_dt[, nuts2 := substr(NUTS3_2021, 1, 4)]
ua_map <- ua_dt[!is.na(NUTS3_2021) & nchar(NUTS3_2021) >= 4,
                .(URAU_CODE = URAU_CODE_short, nuts2)]

city_nuts2 <- merge(data.table(URAU_CODE = all_city_codes),
                    ua_map, by = "URAU_CODE", all.x = TRUE)
n_primary <- sum(!is.na(city_nuts2$nuts2))
cat(sprintf("  Primary mapping: %d / %d cities\n", n_primary, length(all_city_codes)))

# Fallback: spatial join with Eurostat NUTS2 shapefile
unmatched <- city_nuts2[is.na(nuts2), URAU_CODE]
if (length(unmatched) > 0) {
  cat(sprintf("  Spatial-join fallback for %d unmatched cities...\n", length(unmatched)))
  nuts_sf <- tryCatch(
    get_eurostat_geospatial(resolution = "20", nuts_level = 2,
                            year = 2021, make_valid = TRUE),
    error = function(e) { cat("    WARNING: NUTS2 shapefile unavailable\n"); NULL }
  )
  if (!is.null(nuts_sf)) {
    pts <- st_as_sf(city_meta[URAU_CODE %in% unmatched],
                    coords = c("lon", "lat"), crs = 4326)
    joined <- st_join(pts, st_transform(nuts_sf[, "NUTS_ID"], 4326),
                      join = st_within)
    jdt <- as.data.table(st_drop_geometry(joined))[!is.na(NUTS_ID),
                                                    .(URAU_CODE, nuts2 = NUTS_ID)]
    cat(sprintf("    Resolved %d / %d\n", nrow(jdt), length(unmatched)))
    city_nuts2[jdt, nuts2 := i.nuts2, on = "URAU_CODE"]
  }
}
city_nuts2[is.na(nuts2), nuts2 := paste0(substr(URAU_CODE, 1, 2), "00")]

cat(sprintf("  Final: %d cities in %d NUTS2 regions\n",
            nrow(city_nuts2), uniqueN(city_nuts2$nuts2)))
fwrite(city_nuts2, "results_csv/nuts2_city_map.csv")

# ── Step 2: NUTS2 GDP from Eurostat ──────────────────────────────────────────

cat_step(2, "Downloading NUTS2 GDP per inhabitant from Eurostat")

gdp_raw <- tryCatch(
  get_eurostat("nama_10r_2gdp",
               filters = list(unit = gdp_unit,
                              time = as.character(gdp_ref_year)),
               time_format = "num", cache = TRUE),
  error = function(e) get_eurostat("nama_10r_2gdp", cache = TRUE)
)
gdp_dt <- as.data.table(gdp_raw)[nchar(geo) == 4]
if ("unit" %in% names(gdp_dt))        gdp_dt <- gdp_dt[unit == gdp_unit]
if ("time" %in% names(gdp_dt))        gdp_dt <- gdp_dt[time == gdp_ref_year]
if ("TIME_PERIOD" %in% names(gdp_dt)) gdp_dt <- gdp_dt[TIME_PERIOD == gdp_ref_year]
if ("value" %in% names(gdp_dt) && !"values" %in% names(gdp_dt))
  setnames(gdp_dt, "value", "values")
setnames(gdp_dt, c("geo", "values"), c("nuts2", "gdp_pps"), skip_absent = TRUE)
gdp_nuts2 <- gdp_dt[, .(nuts2, gdp_pps)][!is.na(gdp_pps)]
cat(sprintf("  %d NUTS2 regions\n", nrow(gdp_nuts2)))
fwrite(gdp_nuts2, "results_csv/nuts2_gdp.csv")

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

# Keep ERA5 per-city lookup for baseline RR computation
era5_lookup <- split(era5$era5landtmean, era5$URAU_CODE)
rm(era5); invisible(gc())

# ── Step 4: Cluster cities by knots ──────────────────────────────────────────

cat_step(4, sprintf("Clustering cities by knots (k-means, k=%d)", n_clusters))

feat_dt  <- knots_dt[complete.cases(knots_dt)]
feat_mat <- scale(as.matrix(feat_dt[, .(p10, p75, p90)]))
rownames(feat_mat) <- feat_dt$URAU_CODE

set.seed(42)
wss <- sapply(2:10, function(k)
  kmeans(feat_mat, centers = k, nstart = 25, iter.max = 100)$tot.withinss)
elbow_df <- data.frame(k = 2:10, wss = wss)

km <- kmeans(feat_mat, centers = n_clusters, nstart = 50, iter.max = 200)
feat_dt[, cluster_raw := km$cluster]

# Re-label clusters cold → warm by median p75
med_ord <- feat_dt[, .(med = median(p75)), by = cluster_raw]
setorder(med_ord, med)
med_ord[, cluster := paste0("C", seq_len(.N))]
feat_dt <- merge(feat_dt, med_ord[, .(cluster_raw, cluster)], by = "cluster_raw")
feat_dt[, cluster := factor(cluster, levels = paste0("C", 1:n_clusters))]

cat(sprintf("  Cluster sizes:\n"))
print(table(feat_dt$cluster))
fwrite(feat_dt[, .(URAU_CODE, p10, p75, p90, cluster)],
       "results_csv/city_clusters.csv")

# ── Step 5: Load temperatures in bulk ────────────────────────────────────────

cat_step(5, "Loading projected temperatures in bulk")

ds_proj <- open_dataset("data/tmeanproj.gz.parquet")
gcm_cols_all <- names(ds_proj)[grepl("^tas_", names(ds_proj))]
gcm_cols     <- gcm_cols_all[!gsub("tas_", "", gcm_cols_all) %in% gcmexcl]
cat(sprintf("  Using %d GCMs\n", length(gcm_cols)))

# Future temperatures: target year, target SSP — all cities at once
cat(sprintf("  Loading SSP%s year %d for all cities...\n", focus_ssp, focus_year))
proj_future <- ds_proj %>%
  filter(ssp == focus_ssp, year(date) == focus_year) %>%
  select(c("URAU_CODE", "date", all_of(gcm_cols))) %>%
  collect() %>% as.data.table()
cat(sprintf("  Future slice: %d rows, %d cities\n",
            nrow(proj_future), uniqueN(proj_future$URAU_CODE)))

# Build per-city future temperature lookup
proj_lookup <- split(proj_future, proj_future$URAU_CODE)
rm(proj_future); invisible(gc())

# ── Step 6: Compute mortality multiplier per city ────────────────────────────

cat_step(6, "Computing mortality multipliers for all cities")

results_list <- vector("list", length(all_city_codes))

for (ci in seq_along(all_city_codes)) {
  city <- all_city_codes[ci]

  # Knots and temperature bounds from ERA5
  krow <- knots_dt[URAU_CODE == city]
  if (nrow(krow) == 0L) next

  era5_temps <- era5_lookup[[city]]
  if (is.null(era5_temps) || length(era5_temps) < 50) next

  # Build basis using city-specific ERA5 knots
  city_knots  <- c(krow$p10, krow$p75, krow$p90)
  city_bound  <- range(era5_temps, na.rm = TRUE)
  argvar <- list(fun = varfun, degree = vardegree,
                 knots = city_knots, Bound = city_bound)

  # Coefficients for this city
  coefs_city <- coefs_all[URAU_CODE == city]
  if (nrow(coefs_city) == 0L) next

  # Build RR curves on ERA5 temperature grid
  tryCatch({
    rr_res <- compute_rr_curves(coefs_city, agelabs, age_midpoints,
                                argvar, city_bound)
  }, error = function(e) {
    cat(sprintf("  [%d] %s: RR error: %s\n", ci, city, e$message))
    return(NULL)
  })
  if (is.null(rr_res)) next

  rr_interp <- interpolate_rr_to_single_age(
    rr_res$rr_matrix, rr_res$mmt_vec, age_midpoints, age_range
  )

  # Baseline avg RR from ERA5 historical temperatures
  baseline_rr <- compute_avg_rr_by_age(
    era5_temps, rr_res$temp_seq,
    rr_interp$rr_single_age, rr_interp$mmt_single_age,
    age_range, component = focus_comp
  )

  # Future avg RR from projected temperatures
  fut_rows <- proj_lookup[[city]]
  if (is.null(fut_rows) || nrow(fut_rows) == 0L) next

  fut_temps <- unlist(fut_rows[, ..gcm_cols], use.names = FALSE)
  fut_temps <- fut_temps[!is.na(fut_temps)]
  if (length(fut_temps) < 50) next

  future_rr <- compute_avg_rr_by_age(
    fut_temps, rr_res$temp_seq,
    rr_interp$rr_single_age, rr_interp$mmt_single_age,
    age_range, component = focus_comp
  )

  multiplier <- future_rr / baseline_rr

  results_list[[ci]] <- data.table(
    URAU_CODE  = city,
    age        = age_range,
    avg_rr     = future_rr,
    baseline_rr = baseline_rr,
    multiplier = multiplier
  )

  if (ci %% 50 == 0)
    cat(sprintf("  [%d/%d] %s done\n", ci, length(all_city_codes), city))
}

city_multipliers <- rbindlist(results_list)
cat(sprintf("  Multipliers computed for %d cities\n",
            uniqueN(city_multipliers$URAU_CODE)))
fwrite(city_multipliers, "results_csv/city_multipliers.csv")
cat("  Saved: results_csv/city_multipliers.csv\n")

# ── Step 7: Assemble plot dataset ─────────────────────────────────────────────

cat_step(7, "Assembling plot dataset")

# Mean multiplier over focus age group
focus_ages <- age_range[age_range >= as.integer(strsplit(focus_agegroup, "-")[[1]][1]) &
                        age_range <= as.integer(strsplit(focus_agegroup, "-")[[1]][2])]
multi_focus <- city_multipliers[age %in% focus_ages,
                                 .(multiplier = mean(multiplier, na.rm = TRUE)),
                                 by = URAU_CODE]

plot_dt <- Reduce(
  function(x, y) merge(x, y, by = "URAU_CODE", all = FALSE),
  list(
    multi_focus,
    city_nuts2[, .(URAU_CODE, nuts2)],
    feat_dt[, .(URAU_CODE, p10, p75, p90, cluster)],
    city_meta[, .(URAU_CODE, LABEL, CNTR_CODE)]
  )
)
plot_dt <- merge(plot_dt, gdp_nuts2, by = "nuts2", all.x = TRUE)
plot_dt <- plot_dt[!is.na(gdp_pps) & !is.na(multiplier)]
cat(sprintf("  %d cities with complete data\n", nrow(plot_dt)))

# ── Step 8: Scatter plots ─────────────────────────────────────────────────────

cat_step(8, "Generating scatter plots")

ssp_labels <- c("1" = "SSP1-2.6", "2" = "SSP2-4.5", "3" = "SSP3-7.0")
comp_label <- tools::toTitleCase(focus_comp)
x_lab <- sprintf("GDP per inhabitant (%d, PPS index EU27=100)", gdp_ref_year)
y_lab <- sprintf("%s-mortality multiplier (SSP%s %d, age %s)",
                 comp_label, ssp_labels[focus_ssp], focus_year, focus_agegroup)

clust_meta <- plot_dt[, .(med_p75 = median(p75), n = .N), by = cluster]
setorder(clust_meta, med_p75)
clust_labels <- setNames(
  sprintf("%s  (T75 med=%.1f°C, n=%d)", clust_meta$cluster,
          clust_meta$med_p75, clust_meta$n),
  as.character(clust_meta$cluster)
)

cluster_colors <- setNames(
  c("#2166AC", "#92C5DE", "#F4A582", "#D6604D")[seq_len(n_clusters)],
  levels(plot_dt$cluster)
)

# Plot A: all cities
p_all <- ggplot(plot_dt, aes(gdp_pps, multiplier,
                              colour = cluster, label = LABEL)) +
  geom_point(size = 1.8, alpha = 0.75) +
  geom_smooth(aes(group = cluster), method = "lm", se = TRUE,
              linewidth = 0.7, alpha = 0.12) +
  geom_text_repel(size = 1.8, max.overlaps = 15,
                  segment.colour = "grey60") +
  scale_colour_manual(values = cluster_colors, labels = clust_labels,
                      name = "Temperature\ncluster") +
  scale_x_continuous(name = x_lab) +
  scale_y_continuous(name = y_lab) +
  labs(
    title = "NUTS3 cities: GDP per inhabitant vs temperature-attributable mortality multiplier",
    subtitle = sprintf(
      "GDP at NUTS2 level. Clusters = k-means on Masselot ERA5 knots (p10/p75/p90), k=%d",
      n_clusters),
    caption = sprintf(
      "Multiplier = avg RR(%d %s) / avg RR(1990-2019 ERA5 baseline). GDP: Eurostat %s %d.",
      focus_year, ssp_labels[focus_ssp], gdp_unit, gdp_ref_year)
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right",
        plot.title    = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 8, colour = "grey40"))

# Plot B: faceted by cluster
p_facet <- ggplot(plot_dt, aes(gdp_pps, multiplier,
                                colour = cluster, label = LABEL)) +
  geom_point(size = 1.8, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE,
              linewidth = 0.8, alpha = 0.15, colour = "black") +
  geom_text_repel(size = 1.6, max.overlaps = 10,
                  segment.colour = "grey70") +
  scale_colour_manual(values = cluster_colors, guide = "none") +
  scale_x_continuous(name = x_lab) +
  scale_y_continuous(name = y_lab) +
  facet_wrap(~cluster, labeller = as_labeller(clust_labels),
             scales = "free") +
  labs(
    title    = "GDP vs mortality multiplier by temperature cluster",
    subtitle = "Each panel = one k-means temperature cluster; black band = OLS 95% CI"
  ) +
  theme_bw(base_size = 10) +
  theme(strip.background = element_rect(fill = "grey92"),
        plot.title = element_text(face = "bold"))

# Plot C: elbow diagnostic
p_elbow <- ggplot(elbow_df, aes(k, wss)) +
  geom_line(colour = "steelblue") +
  geom_point(colour = "steelblue", size = 2.5) +
  geom_vline(xintercept = n_clusters, linetype = "dashed",
             colour = "firebrick") +
  annotate("text", x = n_clusters + 0.15,
           y = max(elbow_df$wss) * 0.95,
           label = sprintf("k = %d (chosen)", n_clusters),
           hjust = 0, colour = "firebrick", size = 3.5) +
  scale_x_continuous(breaks = 2:10) +
  labs(x = "k", y = "Total within-cluster SS",
       title = "Elbow plot — k-means on city ERA5 knots (p10, p75, p90)") +
  theme_bw(base_size = 10)

out_pdf <- "plots/nuts2_gdp_vs_multiplier.pdf"
pdf(out_pdf, width = 12, height = 8)
print(p_all); print(p_facet); print(p_elbow)
dev.off()
cat(sprintf("  Saved: %s\n", out_pdf))

cat_header("Analysis complete")
