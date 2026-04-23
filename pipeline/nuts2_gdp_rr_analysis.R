################################################################################
#
# NUTS2 GDP vs Mortality Multiplier Analysis
#
# For each NUTS2 region, this script:
#   1. Maps cities to NUTS2 via Urban Audit GPKG (NUTS3_2021 -> substr 1:4).
#      Cities not found in the GPKG are assigned NUTS2 via a spatial join with
#      the Eurostat NUTS2 shapefile (downloaded via the `eurostat` package).
#   2. Downloads NUTS2-level GDP per inhabitant (PPS) from Eurostat
#      (dataset: `nama_10r_2gdp`, reference year 2019).
#   3. Aggregates city ERF curves and temperature distributions to NUTS2 level
#      using population-weighted averaging, mirroring compute_country_multipliers.R.
#   4. Computes temperature distribution percentiles (p25, p50, p75) per NUTS2
#      from historical (1990–2019) city temperatures, population-weighted.
#   5. Clusters NUTS2 regions by their temperature distribution (p25/p50/p75)
#      using k-means.
#   6. Scatter-plots GDP per inhabitant vs mortality multiplier, coloured by
#      temperature cluster.
#
# Inputs:
#   data/urban_audit_cities_2020.gpkg   — NUTS3 code per city
#   data/coefs.csv                      — city B-spline coefficients
#   data/city_results.csv               — city population weights & coordinates
#   data/tmeanproj.gz.parquet           — daily temperature projections
#
# Outputs (results_csv/ and plots/):
#   results_csv/nuts2_city_map.csv          — city → NUTS2 mapping
#   results_csv/nuts2_gdp.csv               — NUTS2 GDP per inhabitant
#   results_csv/nuts2_temp_percentiles.csv  — p25/p50/p75 per NUTS2
#   results_csv/nuts2_multipliers.csv       — mortality multiplier per NUTS2
#   plots/nuts2_gdp_vs_multiplier.pdf       — scatter plots by cluster
#
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(dplyr)
  library(dlnm)
  library(splines)
  library(sf)
  library(eurostat)
  library(ggplot2)
  library(ggrepel)
  library(factoextra)   # for fviz_nbclust / elbow plot
})

source("R/utils.R")
source("R/rr_basis.R")
source("R/load_data.R")

# ── Configuration (mirrors compute_country_multipliers.R) ────────────────────

varfun    <- "bs"
vardegree <- 2
varper    <- c(10, 75, 90)
gcmexcl   <- c("CMCC_CM2_SR5", "TaiESM1")

agelabs       <- c("20-44", "45-64", "65-74", "75-84", "85+")
age_midpoints <- c(32.5, 55, 70, 80, 92.5)
age_range     <- 20:100

baseline_temp_period <- 1990:2019

# Reference scenario and year for the multiplier scatter plot
focus_ssp      <- "2"          # SSP2-4.5
focus_year     <- 2050
focus_agegroup <- "65-74"
focus_comp     <- "total"

gdp_ref_year   <- 2019         # Eurostat GDP reference year
gdp_unit       <- "PPS_EU27_2020_HAB"  # GDP per inhabitant, PPS per inhabitant (EU27=100)

n_clusters     <- 4            # number of k-means temperature clusters

if (!dir.exists("results_csv")) dir.create("results_csv")
if (!dir.exists("plots"))       dir.create("plots")

# ── Step 1: Map cities to NUTS2 ──────────────────────────────────────────────

cat_header("NUTS2 GDP vs Mortality Multiplier Analysis")
cat_step(1, "Mapping cities to NUTS2")

coefs_all    <- fread("data/coefs.csv")
city_results <- fread("data/city_results.csv",
                      select = c("URAU_CODE", "lon", "lat", "pop"))
city_pop     <- unique(city_results[, .(URAU_CODE, lon, lat, pop)])

all_city_codes <- unique(coefs_all$URAU_CODE)
cat(sprintf("  %d cities with coefficients\n", length(all_city_codes)))

# Primary mapping: Urban Audit GPKG (NUTS3_2021 -> NUTS2 = substr(1,4))
ua <- st_read("data/urban_audit_cities_2020.gpkg", quiet = TRUE)
ua_dt <- as.data.table(st_drop_geometry(ua))[, .(URAU_CODE, NUTS3_2021)]
# UA codes end with "1" (e.g. "AT001C1"); coefs codes end with "C" (e.g. "AT001C")
ua_dt[, URAU_CODE_short := sub("(C)1$", "\\1", URAU_CODE)]
ua_dt[, nuts2 := substr(NUTS3_2021, 1, 4)]
ua_map <- ua_dt[!is.na(NUTS3_2021) & nchar(NUTS3_2021) >= 4,
                .(URAU_CODE = URAU_CODE_short, nuts2)]
setkey(ua_map, URAU_CODE)

city_nuts2 <- merge(data.table(URAU_CODE = all_city_codes),
                    ua_map, by = "URAU_CODE", all.x = TRUE)
n_primary <- sum(!is.na(city_nuts2$nuts2))
cat(sprintf("  Primary (UA GPKG) mapping: %d / %d cities\n",
            n_primary, length(all_city_codes)))

# Fallback: spatial join with Eurostat NUTS2 shapefile for unmatched cities
unmatched_codes <- city_nuts2[is.na(nuts2), URAU_CODE]
if (length(unmatched_codes) > 0) {
  cat(sprintf("  Fallback spatial join for %d unmatched cities...\n",
              length(unmatched_codes)))

  # Download NUTS2 shapefile from Eurostat
  nuts_sf <- tryCatch(
    get_eurostat_geospatial(resolution = "20", nuts_level = 2, year = 2021,
                            make_valid = TRUE),
    error = function(e) {
      cat(sprintf("    WARNING: could not download NUTS2 shapefile: %s\n", e$message))
      NULL
    }
  )

  if (!is.null(nuts_sf)) {
    # Build sf point layer for unmatched cities
    unmatched_xy <- city_pop[URAU_CODE %in% unmatched_codes,
                             .(URAU_CODE, lon, lat)]
    if (nrow(unmatched_xy) > 0) {
      pts_sf <- st_as_sf(unmatched_xy, coords = c("lon", "lat"), crs = 4326)
      nuts_sf <- st_transform(nuts_sf, crs = 4326)

      # Spatial join: assign each city point to its NUTS2 polygon
      joined <- st_join(pts_sf, nuts_sf[, c("NUTS_ID")], join = st_within)
      joined_dt <- as.data.table(st_drop_geometry(joined))
      setnames(joined_dt, "NUTS_ID", "nuts2")
      joined_dt <- joined_dt[!is.na(nuts2)]

      cat(sprintf("    Spatial join resolved %d / %d unmatched cities\n",
                  nrow(joined_dt), length(unmatched_codes)))

      # Fill in spatial-join results
      city_nuts2[joined_dt, nuts2 := i.nuts2, on = "URAU_CODE"]
    }
  }
}

# Any still unmatched: fall back to country-code-derived stub NUTS2 (XX00)
still_unmatched <- city_nuts2[is.na(nuts2), URAU_CODE]
if (length(still_unmatched) > 0) {
  cat(sprintf("  Still unmatched after spatial join: %d cities — ",
              length(still_unmatched)))
  cat("assigning country-level pseudo-NUTS2 (first 2 chars + '00')\n")
  city_nuts2[is.na(nuts2), nuts2 := paste0(substr(URAU_CODE, 1, 2), "00")]
}

n_final <- sum(!is.na(city_nuts2$nuts2))
n_regions <- uniqueN(city_nuts2$nuts2)
cat(sprintf("  Final mapping: %d cities in %d NUTS2 regions\n",
            n_final, n_regions))

fwrite(city_nuts2, "results_csv/nuts2_city_map.csv")
cat("  Saved: results_csv/nuts2_city_map.csv\n")

# ── Step 2: Download NUTS2 GDP from Eurostat ──────────────────────────────────

cat_step(2, "Downloading NUTS2 GDP per inhabitant from Eurostat")

gdp_raw <- tryCatch({
  cat(sprintf("  Fetching nama_10r_2gdp (unit=%s, year=%d)...\n",
              gdp_unit, gdp_ref_year))
  get_eurostat("nama_10r_2gdp",
               filters = list(unit = gdp_unit,
                              time = as.character(gdp_ref_year)),
               time_format = "num",
               cache       = TRUE)
}, error = function(e) {
  cat(sprintf("  ERROR fetching Eurostat GDP: %s\n  Retrying without filters...\n",
              e$message))
  tryCatch(
    get_eurostat("nama_10r_2gdp", cache = TRUE),
    error = function(e2) {
      cat(sprintf("  FATAL: cannot retrieve GDP data: %s\n", e2$message))
      NULL
    }
  )
})

if (is.null(gdp_raw)) stop("GDP data unavailable. Aborting.")

gdp_dt <- as.data.table(gdp_raw)

# Keep only NUTS2 rows (4-char codes), requested unit, and reference year
gdp_dt <- gdp_dt[nchar(geo) == 4]
if ("unit" %in% names(gdp_dt)) gdp_dt <- gdp_dt[unit == gdp_unit]
if ("time" %in% names(gdp_dt)) {
  gdp_dt <- gdp_dt[time == gdp_ref_year]
} else if ("TIME_PERIOD" %in% names(gdp_dt)) {
  gdp_dt <- gdp_dt[TIME_PERIOD == gdp_ref_year]
}

# Rename to tidy names
setnames(gdp_dt, old = c("geo", "values"), new = c("nuts2", "gdp_pps"),
         skip_absent = TRUE)
# Some versions return "value" instead of "values"
if ("value" %in% names(gdp_dt) && !"gdp_pps" %in% names(gdp_dt)) {
  setnames(gdp_dt, "value", "gdp_pps")
}

gdp_nuts2 <- gdp_dt[, .(nuts2, gdp_pps)]
gdp_nuts2 <- gdp_nuts2[!is.na(gdp_pps)]
setkey(gdp_nuts2, nuts2)

cat(sprintf("  GDP data: %d NUTS2 regions\n", nrow(gdp_nuts2)))
fwrite(gdp_nuts2, "results_csv/nuts2_gdp.csv")
cat("  Saved: results_csv/nuts2_gdp.csv\n")

# ── Step 3: Open temperature parquet ─────────────────────────────────────────

cat_step(3, "Opening temperature parquet")

ds_city    <- open_dataset("data/tmeanproj.gz.parquet")
gcm_cols_all <- names(ds_city)[grepl("^tas_", names(ds_city))]
gcm_cols     <- gcm_cols_all[!gsub("tas_", "", gcm_cols_all) %in% gcmexcl]
cat(sprintf("  Using %d GCMs (excluded: %s)\n",
            length(gcm_cols), paste(gcmexcl, collapse = ", ")))

# ── Step 4: Aggregate ERF and temperatures per NUTS2 ─────────────────────────

cat_step(4, "Aggregating ERF curves and temperatures per NUTS2 region")

# Attach nuts2 and population to coefs
coefs_all[, country_code := substr(URAU_CODE, 1, 2)]
coefs_all <- merge(coefs_all, city_nuts2, by = "URAU_CODE", all.x = TRUE)
coefs_all <- merge(coefs_all,
                   city_pop[, .(URAU_CODE, pop)],
                   by = "URAU_CODE", all.x = TRUE)
coefs_all[is.na(pop), pop := 1]   # fallback weight
city_pop_vec <- setNames(city_pop$pop, city_pop$URAU_CODE)

nuts2_codes <- sort(unique(coefs_all[!is.na(nuts2), nuts2]))
cat(sprintf("  Processing %d NUTS2 regions\n", length(nuts2_codes)))

# Pre-load ALL historical temperature data for all cities in bulk
# (one pass through parquet is faster than per-city queries)
cat("  Loading historical temperatures for all cities in bulk...\n")
hist_raw_all <- ds_city %>%
  filter(ssp == "hist") %>%
  select(c("URAU_CODE", all_of(gcm_cols))) %>%
  collect() %>%
  as.data.table()

cat(sprintf("  Historical data: %d rows\n", nrow(hist_raw_all)))

# Collect unique city codes from the historical data
city_codes_present <- unique(hist_raw_all$URAU_CODE)

# Build a city -> historical temperatures lookup (named list)
cat("  Building city historical temperature lookup...\n")
city_hist_lookup <- lapply(city_codes_present, function(city) {
  rows <- hist_raw_all[URAU_CODE == city, ..gcm_cols]
  temps <- unlist(rows, use.names = FALSE)
  temps[!is.na(temps)]
})
names(city_hist_lookup) <- city_codes_present
rm(hist_raw_all)  # free memory
invisible(gc())

# Main loop over NUTS2 regions
results_list   <- list()
erf_list       <- list()
mmt_list       <- list()
perc_list      <- list()

target_ssps <- c("1", "2", "3")
ssp_labels  <- c("1" = "SSP1-2.6", "2" = "SSP2-4.5", "3" = "SSP3-7.0")
components  <- c("heat", "cold", "total")

for (ni in seq_along(nuts2_codes)) {
  nuts2_r <- nuts2_codes[ni]   # 'r' = region; avoids collision with column name
  cat(sprintf("\n[%d/%d] %s\n", ni, length(nuts2_codes), nuts2_r))

  city_codes_nuts2 <- unique(coefs_all[nuts2 == nuts2_r, URAU_CODE])
  # Filter to cities that have temp data
  city_codes_nuts2 <- intersect(city_codes_nuts2, city_codes_present)

  if (length(city_codes_nuts2) == 0) {
    cat("  No temperature data for any city — skipping\n")
    next
  }

  cat(sprintf("  %d cities with temp data\n", length(city_codes_nuts2)))

  # Population-weighted historical temperatures for this NUTS2 region
  pop_w_nuts2 <- city_pop_vec[city_codes_nuts2]
  pop_w_nuts2[is.na(pop_w_nuts2)] <- 1

  # NUTS2 historical temperatures = population-weighted pool of city temps
  nuts2_hist_temps <- {
    all_temps  <- numeric(0)
    all_weights <- numeric(0)
    for (city in city_codes_nuts2) {
      ct <- city_hist_lookup[[city]]
      if (length(ct) == 0) next
      w  <- pop_w_nuts2[city]
      all_temps   <- c(all_temps, ct)
      all_weights <- c(all_weights, rep(w / length(ct), length(ct)))
    }
    all_temps
  }

  if (length(nuts2_hist_temps) < 50) {
    cat("  Insufficient historical temperatures — skipping\n")
    next
  }

  # ── Temperature percentiles (p25/p50/p75) ──────────────────────────────
  # Weighted quantiles over the pooled NUTS2 historical temperatures
  perc_list[[nuts2_r]] <- data.table(
    nuts2 = nuts2_r,
    p25   = quantile(nuts2_hist_temps, 0.25, na.rm = TRUE),
    p50   = quantile(nuts2_hist_temps, 0.50, na.rm = TRUE),
    p75   = quantile(nuts2_hist_temps, 0.75, na.rm = TRUE)
  )

  # ── Build basis parameters from NUTS2 pooled historical distribution ────
  basis_params <- build_basis_params(nuts2_hist_temps, varfun, vardegree, varper)

  # ── Compute population-weighted ERF for this NUTS2 ─────────────────────
  coefs_nuts2 <- coefs_all[nuts2 == nuts2_r,
                            .(URAU_CODE, agegroup, b1, b2, b3, b4, b5)]

  rr_result <- compute_country_rr_curves(
    coefs_cities      = coefs_nuts2,
    city_hist_temps   = city_hist_lookup[city_codes_nuts2],
    city_pop_weights  = city_pop_vec,
    agelabs           = agelabs,
    age_midpoints     = age_midpoints,
    country_varbound  = basis_params$varbound,
    varfun            = varfun,
    vardegree         = vardegree,
    varper            = varper
  )

  # Store ERF curves
  erf_dt <- as.data.table(rr_result$rr_matrix)
  setnames(erf_dt, agelabs)
  erf_dt[, nuts2 := nuts2_r]
  erf_dt[, temp  := rr_result$temp_seq]
  erf_list[[nuts2_r]] <- erf_dt

  # Store MMT
  mmt_list[[nuts2_r]] <- data.table(
    nuts2    = nuts2_r,
    agegroup = agelabs,
    mmt      = rr_result$mmt_vec
  )

  # ── Interpolate to single-year ages ────────────────────────────────────
  rr_interp <- interpolate_rr_to_single_age(
    rr_result$rr_matrix, rr_result$mmt_vec,
    age_midpoints, age_range = age_range
  )

  # ── Load NUTS2 projected temperatures (pop-weighted mean across cities) ─
  # Compute on the fly from city-level parquet for this NUTS2
  proj_data_nuts2 <- ds_city %>%
    filter(URAU_CODE %in% city_codes_nuts2) %>%
    select(c("URAU_CODE", "date", "ssp", all_of(gcm_cols))) %>%
    collect() %>%
    as.data.table()

  proj_data_nuts2[, year := year(date)]
  proj_data_nuts2[, doy  := as.integer(format(date, "%j"))]
  proj_data_nuts2[doy > 365L, doy := 365L]

  # Population-weight the temperatures across cities within NUTS2
  pop_dt_nuts2 <- data.table(
    URAU_CODE = city_codes_nuts2,
    pop       = pop_w_nuts2[city_codes_nuts2]
  )
  pop_dt_nuts2[is.na(pop), pop := 1]
  proj_data_nuts2 <- merge(proj_data_nuts2, pop_dt_nuts2,
                            by = "URAU_CODE", all.x = TRUE)

  proj_nuts2_mean <- proj_data_nuts2[, {
    tot_pop <- sum(pop, na.rm = TRUE)
    res <- lapply(gcm_cols, function(col) {
      weighted.mean(get(col), pop / tot_pop, na.rm = TRUE)
    })
    c(list(year = year[1], doy = doy[1]), setNames(res, gcm_cols))
  }, by = .(ssp, date)]

  # ── Baseline average RR ─────────────────────────────────────────────────
  baseline <- pool_baseline_temperatures(proj_nuts2_mean, gcm_cols,
                                         target_ssps, baseline_temp_period)

  baseline_rr <- setNames(
    lapply(components, function(comp) {
      compute_avg_rr_by_age(
        baseline$temps, rr_result$temp_seq,
        rr_interp$rr_single_age, rr_interp$mmt_single_age,
        age_range, component = comp,
        doys = baseline$doys, sw_matrix = NULL
      )
    }),
    components
  )

  # ── Multipliers for all SSPs / years ───────────────────────────────────
  cat(sprintf("  Multipliers: "))
  for (scen in target_ssps) {
    yr_rows   <- proj_nuts2_mean[ssp == scen & year == focus_year]
    if (nrow(yr_rows) == 0L) { cat(sprintf("SSP%s(no data) ", scen)); next }

    all_temps <- unlist(yr_rows[, ..gcm_cols], use.names = FALSE)
    all_doys  <- rep(yr_rows$doy, length(gcm_cols))
    valid     <- !is.na(all_temps)

    for (comp in components) {
      avg_rr <- compute_avg_rr_by_age(
        all_temps[valid], rr_result$temp_seq,
        rr_interp$rr_single_age, rr_interp$mmt_single_age,
        age_range, component = comp,
        doys = all_doys[valid], sw_matrix = NULL
      )
      results_list[[length(results_list) + 1]] <- data.table(
        nuts2       = nuts2_r,
        component   = comp,
        ssp         = scen,
        year        = focus_year,
        age         = age_range,
        avg_rr      = avg_rr,
        multiplier  = avg_rr / baseline_rr[[comp]]
      )
    }
    cat(sprintf("SSP%s ", scen))
  }
  cat("\n")
}

# ── Step 5: Combine results ───────────────────────────────────────────────────

cat_step(5, "Combining and saving intermediate results")

results     <- rbindlist(results_list)
erf_curves  <- rbindlist(erf_list)
mmt_all     <- rbindlist(mmt_list)
perc_all    <- rbindlist(perc_list)

fwrite(perc_all,   "results_csv/nuts2_temp_percentiles.csv")
fwrite(results,    "results_csv/nuts2_multipliers.csv")
fwrite(erf_curves, "results_csv/nuts2_erf_curves.csv")
fwrite(mmt_all,    "results_csv/nuts2_mmt.csv")

cat(sprintf("  nuts2_temp_percentiles.csv: %d regions\n", nrow(perc_all)))
cat(sprintf("  nuts2_multipliers.csv:      %d rows\n",    nrow(results)))

# ── Step 6: Clustering on temperature percentiles ────────────────────────────

cat_step(6, "Clustering NUTS2 regions by temperature distribution")

# Build feature matrix: p25, p50, p75
feat_dt <- perc_all[, .(nuts2, p25, p50, p75)]
feat_dt  <- feat_dt[complete.cases(feat_dt)]

feat_mat <- as.matrix(feat_dt[, .(p25, p50, p75)])
rownames(feat_mat) <- feat_dt$nuts2

# Scale before clustering
feat_scaled <- scale(feat_mat)

# Determine optimal k with a simple within-sum-of-squares elbow
set.seed(42)
wss <- sapply(2:10, function(k) {
  kmeans(feat_scaled, centers = k, nstart = 25, iter.max = 100)$tot.withinss
})
elbow_df <- data.frame(k = 2:10, wss = wss)

# Fit final k-means with chosen n_clusters
km <- kmeans(feat_scaled, centers = n_clusters, nstart = 50, iter.max = 200)
feat_dt[, cluster := as.factor(km$cluster)]

cat(sprintf("  k-means (k=%d) cluster sizes:\n", n_clusters))
print(table(km$cluster))

# Save clustering result
fwrite(feat_dt[, .(nuts2, p25, p50, p75, cluster)],
       "results_csv/nuts2_clusters.csv")
cat("  Saved: results_csv/nuts2_clusters.csv\n")

# ── Step 7: Merge GDP, multiplier, cluster for plotting ──────────────────────

cat_step(7, "Merging GDP, multiplier, and cluster for plotting")

# Focus multiplier: chosen SSP, year, age group 65-74, total component
multi_focus <- results[ssp       == focus_ssp     &
                       year      == focus_year    &
                       component == focus_comp    &
                       age       %in% which(age_range >= 65 & age_range <= 74)]

# Summarise to one value per NUTS2: mean multiplier over the 65-74 age range
multi_nuts2 <- multi_focus[, .(multiplier_mean = mean(multiplier, na.rm = TRUE)),
                             by = nuts2]

# Join GDP, cluster, temp percentiles
plot_dt <- Reduce(
  function(x, y) merge(x, y, by = "nuts2", all = FALSE),
  list(
    multi_nuts2,
    gdp_nuts2,
    feat_dt[, .(nuts2, p25, p50, p75, cluster)]
  )
)
plot_dt <- plot_dt[!is.na(gdp_pps) & !is.na(multiplier_mean)]

# Country label for annotation (first 2 chars of NUTS2)
plot_dt[, country := substr(nuts2, 1, 2)]

cat(sprintf("  %d NUTS2 regions with complete data for plotting\n", nrow(plot_dt)))

fwrite(plot_dt, "results_csv/nuts2_gdp_multiplier_plot_data.csv")
cat("  Saved: results_csv/nuts2_gdp_multiplier_plot_data.csv\n")

# ── Step 8: Scatter plots ─────────────────────────────────────────────────────

cat_step(8, "Generating scatter plots")

ssp_label_str <- ssp_labels[focus_ssp]

# Cluster labels from temperature medians
cluster_medians <- plot_dt[, .(
  med_p50 = median(p50),
  n       = .N
), by = cluster]
setorder(cluster_medians, med_p50)
cluster_medians[, label := sprintf("Cluster %s\n(median T50=%.1f°C, n=%d)",
                                   cluster, med_p50, n)]
clust_labels <- setNames(cluster_medians$label, cluster_medians$cluster)

# Color palette (4 colors, diverging cold→warm)
cluster_colors <- c("#2166AC", "#92C5DE", "#F4A582", "#D6604D")
names(cluster_colors) <- levels(plot_dt$cluster)

# -- Plot A: All NUTS2, coloured by cluster ----------------------------------
p_all <- ggplot(plot_dt, aes(x = gdp_pps, y = multiplier_mean,
                              colour = cluster, label = nuts2)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(aes(group = cluster), method = "lm", se = TRUE,
              linewidth = 0.7, alpha = 0.15) +
  geom_text_repel(size = 2, max.overlaps = 20, segment.colour = "grey60") +
  scale_colour_manual(
    values = cluster_colors,
    labels = clust_labels,
    name   = "Temperature\ncluster"
  ) +
  scale_x_continuous(labels = scales::number_format(suffix = " (EU27=100)"),
                     name   = sprintf("GDP per inhabitant (%d, PPS index EU27=100)", gdp_ref_year)) +
  scale_y_continuous(name = sprintf("Mortality multiplier (%s, %d, age %s)",
                                    ssp_label_str, focus_year, focus_agegroup)) +
  labs(
    title    = "NUTS2: GDP per inhabitant vs Temperature-Attributable Mortality Multiplier",
    subtitle = sprintf("Coloured by temperature distribution cluster (k-means, k=%d on p25/p50/p75)",
                       n_clusters),
    caption  = sprintf("Mortality multiplier relative to %d–%d baseline. ",
                       min(baseline_temp_period), max(baseline_temp_period)) %+%
               sprintf("GDP: Eurostat nama_10r_2gdp (%s).", gdp_unit)
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right",
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, colour = "grey40"))

# -- Plot B: Faceted by cluster (cleaner individual-cluster view) -------------
p_facet <- ggplot(plot_dt, aes(x = gdp_pps, y = multiplier_mean,
                                colour = cluster, label = nuts2)) +
  geom_point(size = 2, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE,
              linewidth = 0.8, alpha = 0.15, colour = "black") +
  geom_text_repel(size = 1.8, max.overlaps = 12, segment.colour = "grey70") +
  scale_colour_manual(values = cluster_colors, guide = "none") +
  scale_x_continuous(labels = scales::number_format(suffix = " (EU27=100)"),
                     name   = sprintf("GDP per inhabitant (%d, PPS index EU27=100)", gdp_ref_year)) +
  scale_y_continuous(name = sprintf("Mortality multiplier (%s, %d)",
                                    ssp_label_str, focus_year)) +
  facet_wrap(~cluster, labeller = as_labeller(clust_labels), scales = "free") +
  labs(
    title    = "GDP vs Mortality Multiplier by Temperature Cluster",
    subtitle = "Each panel = one k-means temperature cluster; grey band = OLS 95% CI"
  ) +
  theme_bw(base_size = 10) +
  theme(strip.background = element_rect(fill = "grey92"),
        plot.title = element_text(face = "bold"))

# -- Plot C: Elbow plot for k selection --------------------------------------
p_elbow <- ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line(colour = "steelblue") +
  geom_point(colour = "steelblue", size = 2) +
  geom_vline(xintercept = n_clusters, linetype = "dashed", colour = "red") +
  annotate("text", x = n_clusters + 0.2, y = max(elbow_df$wss) * 0.95,
           label = sprintf("k = %d\n(chosen)", n_clusters),
           hjust = 0, colour = "red", size = 3.5) +
  scale_x_continuous(breaks = 2:10) +
  labs(x = "Number of clusters (k)",
       y = "Total within-cluster SS",
       title = "Elbow plot for k-means clustering of NUTS2 temperature distributions",
       subtitle = "Features: p25, p50, p75 of historical (1990-2019) daily temperatures") +
  theme_bw(base_size = 10)

# -- Save to PDF (one file, three pages) -------------------------------------
out_pdf <- "plots/nuts2_gdp_vs_multiplier.pdf"
pdf(out_pdf, width = 12, height = 8)
print(p_all)
print(p_facet)
print(p_elbow)
dev.off()

cat(sprintf("  Saved: %s\n", out_pdf))
cat_header("Analysis complete")
cat(sprintf("Outputs in results_csv/ and plots/\n"))
