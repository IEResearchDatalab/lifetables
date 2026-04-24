################################################################################
# Deploy to shinyapps.io
#
# Prerequisites:
#   1. Create a free or paid account at https://www.shinyapps.io
#   2. Run rsconnect::setAccountInfo(...) once with your token
#   3. Run this script or source it in RStudio
#
# The dashboard/data/ folder must exist (run prep_deploy_data.R first).
# Data is now Hive-partitioned by URAU_CODE to avoid OOM on the server.
################################################################################

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}

library(rsconnect)

# Check data exists (now partitioned directory instead of single file)
if (!dir.exists("dashboard/data/tmeanproj")) {
  cat("Partitioned data not found. Running prep_deploy_data.R first...\n")
  source("dashboard/prep_deploy_data.R")
}

# Check precomputed data exists (RR surfaces + vcov)
if (!dir.exists("dashboard/data/precomputed")) {
  cat("Precomputed RR data not found. Running precompute_rr.R first...\n")
  source("dashboard/precompute_rr.R")
}

# Build file list: app.R + csv files + partitioned parquet + precomputed
parquet_files <- list.files("dashboard/data/tmeanproj",
                            recursive = TRUE, full.names = FALSE)
parquet_files <- file.path("data/tmeanproj", parquet_files)

precomp_files <- list.files("dashboard/data/precomputed",
                            recursive = TRUE, full.names = FALSE)
precomp_files <- file.path("data/precomputed", precomp_files)

app_files <- c(
  "app.R",
  parquet_files,
  precomp_files,
  "data/coefs.csv",
  "data/city_lookup.csv"
)

cat(sprintf("Deploying %d files (including %d partition files)...\n",
            length(app_files), length(parquet_files)))

# Deploy
deployApp(
  appDir   = "dashboard",
  appName  = "climate-mortality-dashboard",
  appTitle = "Climate–Mortality Pipeline Dashboard",
  appFiles = app_files,
  forceUpdate = TRUE
)
