#!/usr/bin/env Rscript
# ==============================================================================
# CORDEX EUR-11 Temperature Data Processing (RCP8.5)
# Convert NetCDF files to city-level daily temperature parquet
# Output schema matches data/tmeanproj.gz.parquet
# ==============================================================================

library(ncdf4)
library(data.table)
library(arrow)
library(sf)

# Configuration ----------------------------------------------------------------
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else ""
script_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else getwd()

# CLI args (excluding --file)
args_cli <- commandArgs(trailingOnly = TRUE)

# Default CORDEX data folder lives next to this scripts folder
CORDEX_DATA_DIR <- normalizePath(file.path(script_dir, "..", "cordex_data"), mustWork = FALSE)

# Allow overriding directories
raw_dir_arg <- grep("^--raw-dir=", args_cli, value = TRUE)
out_dir_arg <- grep("^--output-dir=", args_cli, value = TRUE)

RAW_DATA_DIR <- if (length(raw_dir_arg) > 0) {
  sub("^--raw-dir=", "", raw_dir_arg[1])
} else {
  Sys.getenv("CORDEX_RAW_DIR", unset = file.path(CORDEX_DATA_DIR, "raw"))
}

OUTPUT_DIR <- if (length(out_dir_arg) > 0) {
  sub("^--output-dir=", "", out_dir_arg[1])
} else {
  Sys.getenv("CORDEX_OUTPUT_DIR", unset = file.path(CORDEX_DATA_DIR, "processed"))
}

# Backward-compatible fallback (older layout: raw/processed under script folder)
if (!dir.exists(RAW_DATA_DIR) && dir.exists(file.path(script_dir, "raw"))) {
  RAW_DATA_DIR <- file.path(script_dir, "raw")
}
if (!dir.exists(OUTPUT_DIR) && dir.exists(file.path(script_dir, "processed"))) {
  OUTPUT_DIR <- file.path(script_dir, "processed")
}
OUTPUT_FILE <- "tmeanproj_rcp85.gz.parquet"
SCENARIO_FILTER <- "rcp85"
SCENARIO_CODE <- "5"

# Optional: provide Urban Audit city polygons to aggregate temperatures following
# Masselot et al. (pixels whose centroids fall within boundaries).
# Usage examples:
#   Rscript process_to_parquet.R --city-polygons=/path/to/city_boundaries.gpkg
#   CITY_POLYGONS=/path/to/city_boundaries.gpkg Rscript process_to_parquet.R
city_polygons_arg <- grep("^--city-polygons=", args_cli, value = TRUE)
default_polygons_path <- file.path(script_dir, "..", "data", "urban_audit_cities_2020.gpkg")

CITY_POLYGONS <- if (length(city_polygons_arg) > 0) {
  sub("^--city-polygons=", "", city_polygons_arg[1])
} else {
  Sys.getenv("CITY_POLYGONS", unset = "")
}

if (!nzchar(CITY_POLYGONS) && file.exists(default_polygons_path)) {
  CITY_POLYGONS <- default_polygons_path
  message(sprintf("Using default city polygons: %s", CITY_POLYGONS))
}

# Ensure output directory exists
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

if (!nzchar(CITY_POLYGONS) || !file.exists(CITY_POLYGONS)) {
  stop(
    "This script now requires a city polygons file. Provide it via ",
    "--city-polygons=/path/to/urban_audit_cities.gpkg (must contain URAU_CODE).\n",
    "Expected default (if present): ", default_polygons_path
  )
}

message(sprintf("Loading city polygons from: %s", CITY_POLYGONS))
city_polys_all <- sf::read_sf(CITY_POLYGONS)
if (!("URAU_CODE" %in% names(city_polys_all))) {
  stop("City polygons must contain a 'URAU_CODE' column")
}
city_polys_all <- city_polys_all[!is.na(city_polys_all$URAU_CODE), ]
if (nrow(city_polys_all) == 0) {
  stop("City polygons file contains zero non-missing URAU_CODE values")
}

# Normalize codes like RO001C1 -> RO001C and deduplicate by keeping the largest
# polygon for each base code.
city_polys_all$URAU_CODE_BASE <- sub("[0-9]+$", "", city_polys_all$URAU_CODE)
city_polys_all$..area_sqm <- as.numeric(sf::st_area(city_polys_all))
city_polys_all <- city_polys_all[order(city_polys_all$URAU_CODE_BASE, -city_polys_all$..area_sqm), ]
city_polys_all <- city_polys_all[!duplicated(city_polys_all$URAU_CODE_BASE), ]
city_polys_all <- sf::st_make_valid(city_polys_all)
city_polys_all <- sf::st_transform(city_polys_all, 4326)

message(sprintf("Using %d unique city polygons (by URAU_CODE_BASE)", nrow(city_polys_all)))

# Model information ------------------------------------------------------------
files <- list.files(RAW_DATA_DIR, pattern = "^tas_EUR-11_.*\\.nc$", full.names = TRUE)

if (length(files) == 0) {
  stop("No NetCDF files found in ", RAW_DATA_DIR)
}

extract_scenario <- function(filename) {
  parts <- strsplit(basename(filename), "_")[[1]]
  parts[4]
}

files <- files[sapply(files, extract_scenario) == SCENARIO_FILTER]

if (length(files) == 0) {
  stop("No NetCDF files found for scenario: ", SCENARIO_FILTER)
}

cat(sprintf("Found %d NetCDF files for %s\n", length(files), SCENARIO_FILTER))

extract_model_info <- function(filename) {
  parts <- strsplit(basename(filename), "_")[[1]]
  gcm <- parts[3]
  rcm <- parts[6]
  member <- parts[5]
  paste(gcm, rcm, member, sep = "_")
}

model_ids <- unique(sapply(files, extract_model_info))
cat(sprintf("Identified %d unique models:\n", length(model_ids)))
cat(paste("  -", model_ids, collapse = "\n"), "\n\n")

# Function to extract city data from NetCDF -----------------------------------
extract_city_temperature <- function(nc_file, city_polys) {
  cat(sprintf("Processing: %s\n", basename(nc_file)))

  tryCatch({
    # Open NetCDF once per file
    nc <- ncdf4::nc_open(nc_file)
    on.exit(ncdf4::nc_close(nc), add = TRUE)

    # Read time axis for robust Date parsing
    time_vals <- ncdf4::ncvar_get(nc, "time")
    time_units <- ncdf4::ncatt_get(nc, "time", "units")$value
    time_origin <- as.Date(sub(".*since ", "", time_units))
    dates <- time_origin + time_vals

    # Polygon aggregation (centroid-in-polygon mean) on a curvilinear grid.
    # We avoid relying on GDAL's NetCDF raster interpretation and instead:
    #   1) use the NetCDF-provided lon/lat of each grid cell (cell centroids),
    #   2) assign grid cells to city polygons in EPSG:4326,
    #   3) compute daily means in time chunks.

    lon <- ncdf4::ncvar_get(nc, "lon")
    lat <- ncdf4::ncvar_get(nc, "lat")
    if (!identical(dim(lon), dim(lat))) {
      stop("Unexpected lon/lat shapes in NetCDF (dims do not match)")
    }

    ncell <- length(lon)
    pts <- sf::st_as_sf(
      data.frame(
        cell_id = seq_len(ncell),
        lon = as.vector(lon),
        lat = as.vector(lat)
      ),
      coords = c("lon", "lat"),
      crs = 4326,
      remove = FALSE
    )

    # For each polygon, get the indices of grid-cell centroids that fall inside.
    cell_idx_by_city <- sf::st_intersects(city_polys, pts, sparse = TRUE)
    if (any(lengths(cell_idx_by_city) == 0)) {
      empty <- which(lengths(cell_idx_by_city) == 0)
      warning(sprintf(
        "Some cities have zero grid cells inside polygon for %s (%d/%d)",
        basename(nc_file), length(empty), length(cell_idx_by_city)
      ))
    }

    # Read tas in time chunks and compute means per city.
    chunk_size <- 31L
    out_list <- vector("list", length = nrow(city_polys))
    for (i in seq_len(nrow(city_polys))) out_list[[i]] <- list()

    for (t0 in seq.int(1L, length(dates), by = chunk_size)) {
      n_this <- min(chunk_size, length(dates) - t0 + 1L)
      tas_chunk <- ncdf4::ncvar_get(
        nc,
        "tas",
        start = c(1, 1, t0),
        count = c(-1, -1, n_this)
      )

      # Flatten to matrix [cell, time]
      tas_mat <- matrix(tas_chunk, nrow = ncell, ncol = n_this)

      for (i in seq_along(cell_idx_by_city)) {
        idx <- cell_idx_by_city[[i]]
        if (length(idx) == 0) next
        means_k <- colMeans(tas_mat[idx, , drop = FALSE], na.rm = TRUE)
        out_list[[i]][[length(out_list[[i]]) + 1L]] <- data.table::data.table(
          URAU_CODE = city_polys$URAU_CODE_BASE[[i]],
          date = dates[t0:(t0 + n_this - 1L)],
          temperature = round(means_k - 273.15, 3)
        )
      }
    }

    long_dt <- data.table::rbindlist(lapply(out_list, data.table::rbindlist), use.names = TRUE)
    data.table::setorder(long_dt, URAU_CODE, date)
    return(long_dt)
  }, error = function(e) {
    warning(sprintf("Error processing %s: %s", basename(nc_file), e$message))
    NULL
  })
}


# Process all files by model --------------------------------------------------
cat("\n=== Starting data extraction ===\n\n")

all_model_data <- list()

for (model_id in model_ids) {
  cat(sprintf("\nProcessing model: %s\n", model_id))
  cat(strrep("=", 60), "\n")

  model_files <- files[sapply(files, extract_model_info) == model_id]
  cat(sprintf("Found %d files for this model\n", length(model_files)))

  model_data_list <- lapply(model_files, function(f) {
    extract_city_temperature(f, city_polys_all)
  })

  model_data_list <- model_data_list[!sapply(model_data_list, is.null)]

  if (length(model_data_list) > 0) {
    model_data <- rbindlist(model_data_list)
    model_data[, model := model_id]

    all_model_data[[model_id]] <- model_data

    cat(sprintf("Extracted %d daily records for %d cities\n",
                nrow(model_data), length(unique(model_data$URAU_CODE))))
  }
}

# Combine and reshape to wide format ------------------------------------------
cat("\n=== Combining and reshaping data ===\n")

if (length(all_model_data) > 0) {
  combined_data <- rbindlist(all_model_data)
  combined_data[, ssp := SCENARIO_CODE]

  cat(sprintf("Total records: %d\n", nrow(combined_data)))
  cat(sprintf("Date range: %s to %s\n", min(combined_data$date), max(combined_data$date)))

  wide_data <- dcast(combined_data,
                     URAU_CODE + date + ssp ~ model,
                     value.var = "temperature")

  model_cols <- setdiff(names(wide_data), c("URAU_CODE", "date", "ssp"))
  setnames(wide_data, model_cols, paste0("tas_", model_cols))

  setorder(wide_data, URAU_CODE, date)

  output_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
  write_parquet(wide_data, output_path, compression = "gzip")

  cat(sprintf("\n=== SUCCESS ===\n"))
  cat(sprintf("Output written to: %s\n", output_path))
  cat(sprintf("Dimensions: %d rows x %d columns\n", nrow(wide_data), ncol(wide_data)))
  cat(sprintf("Cities: %d\n", length(unique(wide_data$URAU_CODE))))
  cat(sprintf("Models: %d\n", length(model_ids)))
  cat(sprintf("Scenario code: %s\n", SCENARIO_CODE))

  cat("\nFirst few rows:\n")
  print(head(wide_data, 10))

} else {
  stop("No data was successfully extracted!")
}

cat("\n=== Processing complete ===\n")
