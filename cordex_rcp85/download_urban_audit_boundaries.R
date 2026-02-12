################################################################################
#
# Download Urban Audit city boundaries (URAU_CODE polygons) from Eurostat GISCO
#
# These polygons can be used for “centroid-in-polygon” raster aggregation,
# matching the Masselot et al. approach (average raster pixels whose centroids
# fall within the city boundary).
#
# Output: a GeoPackage containing Urban Audit CITIES polygons.
#
################################################################################

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(key, default = NULL) {
  hit <- grep(paste0("^", key, "="), args, value = TRUE)
  if (length(hit) == 0) return(default)
  sub(paste0("^", key, "="), "", hit[[1]])
}

# GISCO provides Urban Audit for several release years.
# 2020 is a common baseline and includes URAU_CODE.
year <- as.integer(get_arg("--year", "2020"))

out_path <- get_arg("--out", sprintf("data/urban_audit_cities_%d.gpkg", year))
out_layer <- get_arg("--layer", "urban_audit_cities")

if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Missing package 'sf'. Install it with install.packages('sf')")
}
if (!requireNamespace("giscoR", quietly = TRUE)) {
  stop("Missing package 'giscoR'. Install it with install.packages('giscoR')")
}

message(sprintf("Downloading Urban Audit CITIES polygons (year=%d)…", year))

ua <- tryCatch(
  {
    giscoR::gisco_get_urban_audit(
      year = year,
      level = "CITIES",
      spatialtype = "RG",
      epsg = 4326,
      verbose = TRUE,
      update_cache = FALSE,
      ext = "gpkg"
    )
  },
  error = function(e) {
    # Backward compatibility with older giscoR versions where `ext` is not available.
    if (grepl("unused argument.*ext", conditionMessage(e), ignore.case = TRUE)) {
      giscoR::gisco_get_urban_audit(
        year = year,
        level = "CITIES",
        spatialtype = "RG",
        epsg = 4326,
        verbose = TRUE,
        update_cache = FALSE
      )
    } else {
      stop(e)
    }
  }
)

if (is.null(ua) || nrow(ua) == 0) {
  stop("No features returned from gisco_get_urban_audit().")
}

needed_cols <- c("URAU_CODE")
missing_cols <- setdiff(needed_cols, names(ua))
if (length(missing_cols) > 0) {
  stop(sprintf(
    "Unexpected schema: missing column(s): %s",
    paste(missing_cols, collapse = ", ")
  ))
}

# Ensure valid geometries
ua <- sf::st_make_valid(ua)

# Write GeoPackage
if (!dir.exists(dirname(out_path))) dir.create(dirname(out_path), recursive = TRUE)

sf::write_sf(ua, out_path, layer = out_layer, delete_dsn = TRUE)

message(sprintf("Wrote %d polygons to %s (layer=%s)", nrow(ua), out_path, out_layer))
