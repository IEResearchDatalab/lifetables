#!/usr/bin/env Rscript
# ==============================================================================
# CORDEX EUR-11 Temperature Data Processing (RCP8.5)
# Convert NetCDF files to city-level daily temperature parquet
# Output schema matches data/tmeanproj.gz.parquet
# ==============================================================================

library(ncdf4)
library(data.table)
library(arrow)

# Configuration ----------------------------------------------------------------
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else ""
script_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else getwd()

RAW_DATA_DIR <- file.path(script_dir, "raw")
OUTPUT_DIR <- file.path(script_dir, "processed")
OUTPUT_FILE <- "tmeanproj_rcp85.gz.parquet"
SCENARIO_FILTER <- "rcp85"
SCENARIO_CODE <- "5"

# Ensure output directory exists
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# City coordinates -------------------------------------------------------------
# European capitals + Romanian cities
cities <- data.table(
  city_code = c(
    # Major European capitals (ISO country code + 001C for capital)
    "NL001C", "GR001C", "DE001C", "BE001C", "RO001C",
    "HU001C", "DK001C", "IE001C", "FI001C", "PT001C",
    "GB001C", "ES001C", "NO001C", "FR001C", "CZ001C",
    "IT001C", "SE001C", "AT001C", "PL001C", "CH001C",
    # Romanian cities (RO002C to RO039C, alphabetically after capital)
    "RO002C", "RO003C", "RO004C", "RO005C", "RO006C",
    "RO007C", "RO008C", "RO009C", "RO010C", "RO011C",
    "RO012C", "RO013C", "RO014C", "RO015C", "RO016C",
    "RO017C", "RO018C", "RO019C", "RO020C", "RO021C",
    "RO022C", "RO023C", "RO024C", "RO025C", "RO026C",
    "RO027C", "RO028C", "RO029C", "RO030C", "RO031C",
    "RO032C", "RO033C", "RO034C", "RO035C", "RO036C",
    "RO037C", "RO038C", "RO039C"
  ),
  city_name = c(
    # Major European capitals
    "Amsterdam", "Athens", "Berlin", "Brussels", "Bucharest",
    "Budapest", "Copenhagen", "Dublin", "Helsinki", "Lisbon",
    "London", "Madrid", "Oslo", "Paris", "Prague",
    "Rome", "Stockholm", "Vienna", "Warsaw", "Zurich",
    # Romanian cities (from your lifetable data - RO002C to RO039C)
    "Alba Iulia", "Arad", "Bacau", "Baia Mare", "Bistrita",
    "Botosani", "Braila", "Brasov", "Buzau", "Calarasi",
    "Cluj-Napoca", "Constanta", "Craiova", "Deva", "Drobeta-Turnu Severin",
    "Focsani", "Galati", "Giurgiu", "Iasi", "Miercurea Ciuc",
    "Oradea", "Piatra Neamt", "Pitesti", "Ploiesti", "Ramnicu Valcea",
    "Resita", "Satu Mare", "Sibiu", "Slatina", "Slobozia",
    "Suceava", "Targoviste", "Targu Jiu", "Targu Mures", "Timisoara",
    "Tulcea", "Vaslui", "Zalau"
  ),
  lat = c(
    # European capitals
    52.37, 37.98, 52.52, 50.85, 44.43,
    47.50, 55.68, 53.35, 60.17, 38.72,
    51.51, 40.42, 59.91, 48.86, 50.08,
    41.90, 59.33, 48.21, 52.23, 47.37,
    # Romanian cities
    46.07, 46.17, 46.57, 47.66, 47.13,
    47.75, 45.27, 45.65, 45.15, 44.20,
    46.77, 44.18, 44.32, 45.88, 44.64,
    45.70, 45.44, 43.90, 47.16, 46.36,
    47.05, 46.93, 44.85, 44.95, 45.10,
    45.30, 47.79, 45.80, 44.43, 44.56,
    47.65, 44.93, 45.03, 46.54, 45.75,
    45.18, 46.64, 47.20
  ),
  lon = c(
    # European capitals
    4.90, 23.73, 13.40, 4.35, 26.10,
    19.04, 12.57, -6.26, 24.94, -9.14,
    -0.13, -3.70, 10.75, 2.35, 14.44,
    12.50, 18.07, 16.37, 21.01, 8.54,
    # Romanian cities
    23.57, 21.32, 26.91, 23.57, 24.50,
    26.66, 27.97, 25.61, 26.82, 27.33,
    23.60, 28.63, 23.80, 22.90, 22.66,
    27.18, 28.05, 25.97, 27.59, 25.80,
    21.92, 26.37, 24.87, 26.02, 24.37,
    21.89, 22.88, 24.15, 24.37, 27.72,
    26.25, 25.46, 23.27, 24.56, 21.23,
    28.80, 27.73, 23.06
  )
)

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
extract_city_temperature <- function(nc_file, cities_dt) {
  cat(sprintf("Processing: %s\n", basename(nc_file)))

  tryCatch({
    nc <- nc_open(nc_file)

    lons <- ncvar_get(nc, "lon")
    lats <- ncvar_get(nc, "lat")
    time_vals <- ncvar_get(nc, "time")
    time_units <- ncatt_get(nc, "time", "units")$value

    time_origin <- as.Date(sub(".*since ", "", time_units))
    dates <- time_origin + time_vals

    tas <- ncvar_get(nc, "tas")

    nc_close(nc)

    city_data_list <- lapply(1:nrow(cities_dt), function(i) {
      city_code <- cities_dt$city_code[i]
      city_lat <- cities_dt$lat[i]
      city_lon <- cities_dt$lon[i]

      if (is.null(dim(lons))) {
        lat_idx <- which.min(abs(lats - city_lat))
        lon_idx <- which.min(abs(lons - city_lon))
      } else {
        dist <- sqrt((lons - city_lon)^2 + (lats - city_lat)^2)
        min_idx <- which(dist == min(dist), arr.ind = TRUE)[1, ]
        lon_idx <- min_idx[1]
        lat_idx <- min_idx[2]
      }

      if (length(dim(tas)) == 3) {
        temp_ts <- tas[lon_idx, lat_idx, ]
      } else if (length(dim(tas)) == 4) {
        temp_ts <- tas[lon_idx, lat_idx, 1, ]
      } else {
        stop("Unexpected tas dimensions in ", basename(nc_file))
      }

      temp_celsius <- round(temp_ts - 273.15, 3)

      data.table(
        URAU_CODE = city_code,
        date = dates,
        temperature = temp_celsius
      )
    })

    rbindlist(city_data_list)

  }, error = function(e) {
    warning(sprintf("Error processing %s: %s", basename(nc_file), e$message))
    NULL
  })
}


ç
# Process all files by model --------------------------------------------------
cat("\n=== Starting data extraction ===\n\n")

all_model_data <- list()

for (model_id in model_ids) {
  cat(sprintf("\nProcessing model: %s\n", model_id))
  cat(strrep("=", 60), "\n")

  model_files <- files[sapply(files, extract_model_info) == model_id]
  cat(sprintf("Found %d files for this model\n", length(model_files)))

  model_data_list <- lapply(model_files, function(f) {
    extract_city_temperature(f, cities)
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
