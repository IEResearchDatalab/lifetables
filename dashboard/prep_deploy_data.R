################################################################################
# Prepare deployment data for the public dashboard
#
# Filters the 3.1 GB temperature parquet to only:
#   - All Romanian cities (35)
#   - European capitals   (30, one overlap = Bucharest)
#   → 64 unique cities
#
# The parquet is written as a **Hive-partitioned dataset** by URAU_CODE so that
# Arrow can read a single city's file (~3-4 MB) without loading the full dataset
# into memory.  This prevents OOM crashes on shinyapps.io (1 GB limit).
#
# Also copies coefs.csv and a trimmed city_results.csv into the deploy folder.
#
# Run:
#   Rscript dashboard/prep_deploy_data.R
################################################################################

library(data.table)
library(arrow)

cat("=== Preparing deployment data ===\n")

# --- Identify target cities ---
city_all <- fread("data/city_results.csv")[
  , .(URAU_CODE, LABEL, CNTR_CODE, cntr_name)
] |> unique()

ro_codes  <- city_all[CNTR_CODE == "RO", URAU_CODE]
cap_codes <- city_all[grepl("001C$", URAU_CODE), URAU_CODE]
keep_codes <- unique(c(ro_codes, cap_codes))

cat(sprintf("Romanian cities: %d\n", length(ro_codes)))
cat(sprintf("EU capitals:     %d\n", length(cap_codes)))
cat(sprintf("Total unique:    %d\n", length(keep_codes)))

# --- Filter temperature projections and write as partitioned dataset ---
cat("\nFiltering & partitioning temperature parquet (this takes a minute)...\n")

out_dir <- "dashboard/data"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Remove old single-file parquet if it exists
old_pq <- file.path(out_dir, "tmeanproj.gz.parquet")
if (file.exists(old_pq)) file.remove(old_pq)

# Remove old partitioned dir if it exists
out_pq_dir <- file.path(out_dir, "tmeanproj")
if (dir.exists(out_pq_dir)) unlink(out_pq_dir, recursive = TRUE)

# Also drop the 2 excluded GCMs at the data-prep stage to save space
gcmexcl <- c("CMCC_CM2_SR5", "TaiESM1")
excl_cols <- paste0("tas_", gcmexcl)

pq <- open_dataset("data/tmeanproj.gz.parquet")
all_cols <- names(pq$schema)
keep_cols <- setdiff(all_cols, excl_cols)

total_rows <- 0L
for (cc in keep_codes) {
  sub <- pq |>
    dplyr::filter(URAU_CODE == cc) |>
    dplyr::select(dplyr::all_of(keep_cols)) |>
    dplyr::collect()
  total_rows <- total_rows + nrow(sub)
  cat(sprintf("  %s : %s rows\n", cc, format(nrow(sub), big.mark = ",")))
}
rm(sub); gc()

# Write partitioned dataset
pq |>
  dplyr::filter(URAU_CODE %in% keep_codes) |>
  dplyr::select(dplyr::all_of(keep_cols)) |>
  write_dataset(out_pq_dir, format = "parquet",
                partitioning = "URAU_CODE",
                compression = "gzip")

# Report sizes
part_files <- list.files(out_pq_dir, recursive = TRUE, full.names = TRUE)
total_mb <- sum(file.size(part_files)) / 1024^2
cat(sprintf("\n  Partitioned dataset: %d files, %.1f MB total\n",
            length(part_files), total_mb))
cat(sprintf("  Total rows: %s\n", format(total_rows, big.mark = ",")))

# --- Copy coefficients ---
file.copy("data/coefs.csv", file.path(out_dir, "coefs.csv"), overwrite = TRUE)
cat("  Copied: coefs.csv\n")

# --- Extract simulated coefficients for ERF uncertainty ---
cat("\nExtracting simulated ERF coefficients for deployed cities...\n")
sim_ds <- open_dataset("data/coef_simu.gz.parquet")
coef_sim <- sim_ds |>
  dplyr::filter(URAU_CODE %in% keep_codes) |>
  dplyr::collect() |>
  as.data.table()
cat(sprintf("  Rows: %s (%d cities × 5 ages × 1000 sims)\n",
            format(nrow(coef_sim), big.mark = ","), length(keep_codes)))

# Keep only 200 simulation draws (out of 1000) to save space + speed
set.seed(42)
keep_sims <- sort(sample(1:1000, 200))
coef_sim <- coef_sim[sim %in% keep_sims]
# Re-index sims 1..200
coef_sim[, sim := match(sim, keep_sims)]

out_sim <- file.path(out_dir, "coef_sim.csv")
fwrite(coef_sim, out_sim)
cat(sprintf("  Saved: coef_sim.csv (%s rows, %.1f MB)\n",
            format(nrow(coef_sim), big.mark = ","),
            file.size(out_sim) / 1024^2))

# --- Trimmed city_results (only kept cities, minimal columns) ---
city_keep <- city_all[URAU_CODE %in% keep_codes]
city_keep[, display := paste0(LABEL, " (", cntr_name, ")")]
fwrite(city_keep, file.path(out_dir, "city_lookup.csv"))
cat(sprintf("  Saved: city_lookup.csv (%d cities)\n", nrow(city_keep)))

cat("\nDone. The dashboard/data/ folder is ready for deployment.\n")
