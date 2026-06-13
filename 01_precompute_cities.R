#!/usr/bin/env Rscript
################################################################################
# Stage 1: Precompute City-Level Data
# 
# This script precomputes city-specific data that is reused across scenarios:
# - ERF coefficients
# - Temperature projections
# - Baseline RR
# - Basis parameters
#
# Usage: Rscript 01_precompute_cities.R [--parallel N]
################################################################################

library(data.table)
library(future.apply)
library(optparse)

# Parse command-line arguments
option_list <- list(
  make_option(c("-p", "--parallel"), type = "integer", default = 8,
              help = "Number of parallel workers [default %default]"),
  make_option(c("-t", "--test"), action = "store_true", default = FALSE,
              help = "Test mode: run only 3 cities"),
  make_option(c("-r", "--resume"), action = "store_true", default = FALSE,
              help = "Resume: skip already precomputed cities")
)

opt <- parse_args(OptionParser(option_list = option_list))

cat("\n=== Stage 1: City Precomputation ===\n")
cat(sprintf("Workers: %d\n", opt$parallel))
cat(sprintf("Test mode: %s\n", opt$test))
cat(sprintf("Resume mode: %s\n", opt$resume))
cat(sprintf("Start time: %s\n\n", Sys.time()))

# Source functions
source("pipeline_functions.R")

# Create output directories
dir.create("precomputed/cities", showWarnings = FALSE, recursive = TRUE)
dir.create("logs", showWarnings = FALSE, recursive = TRUE)

# Load city list
coefs_all <- fread("data/coefs.csv")
city_codes <- unique(coefs_all$URAU_CODE)

if (opt$test) {
  cat("TEST MODE: Running first 3 cities only\n\n")
  city_codes <- head(city_codes, 3)
}

cat(sprintf("Total cities to process: %d\n\n", length(city_codes)))

# Filter already completed (if resume mode)
if (opt$resume) {
  existing <- list.files("precomputed/cities", pattern = "\\.rds$")
  existing_codes <- gsub("\\.rds$", "", existing)
  city_codes <- setdiff(city_codes, existing_codes)
  cat(sprintf("Resume mode: %d cities already completed\n", 
              length(existing_codes)))
  cat(sprintf("Remaining: %d cities\n\n", length(city_codes)))
}

# Initialize progress tracking
progress <- data.table(
  city_code = city_codes,
  status = "pending",
  start_time = as.POSIXct(NA),
  end_time = as.POSIXct(NA),
  runtime_sec = NA_real_,
  file_size_mb = NA_real_,
  error_msg = NA_character_
)

progress_file <- "logs/stage1_progress.csv"
fwrite(progress, progress_file)

# Set up parallel processing
plan(multisession, workers = opt$parallel)

# Run precomputation
cat("Starting parallel precomputation...\n\n")

results <- future_lapply(seq_along(city_codes), function(i) {
  city_code <- city_codes[i]
  
  # Update progress
  progress[i, `:=`(status = "running", start_time = Sys.time())]
  fwrite(progress, progress_file)
  
  # Run precomputation
  result <- tryCatch({
    output_file <- precompute_city(city_code)
    
    # Update success
    progress[i, `:=`(
      status = "completed",
      end_time = Sys.time(),
      runtime_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      file_size_mb = file.size(output_file) / 1024^2
    )]
    fwrite(progress, progress_file)
    
    list(status = "success", city = city_code, file = output_file)
    
  }, error = function(e) {
    # Update failure
    progress[i, `:=`(
      status = "failed",
      end_time = Sys.time(),
      runtime_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      error_msg = e$message
    )]
    fwrite(progress, progress_file)
    
    list(status = "failed", city = city_code, error = e$message)
  })
  
  return(result)
}, future.seed = TRUE)

# Summarize results
cat("\n=== Stage 1 Complete ===\n\n")

successes <- sum(sapply(results, function(x) x$status == "success"))
failures <- sum(sapply(results, function(x) x$status == "failed"))

cat(sprintf("Successful: %d / %d (%.1f%%)\n", 
            successes, length(city_codes),
            100 * successes / length(city_codes)))
cat(sprintf("Failed: %d\n", failures))

# Show failures
if (failures > 0) {
  cat("\nFailed cities:\n")
  failed_cities <- sapply(results[sapply(results, function(x) x$status == "failed")], 
                          function(x) x$city)
  print(failed_cities)
}

# Summary statistics
progress_final <- fread(progress_file)
completed <- progress_final[status == "completed"]

if (nrow(completed) > 0) {
  cat(sprintf("\nRuntime statistics:\n"))
  cat(sprintf("  Mean: %.1f seconds\n", mean(completed$runtime_sec)))
  cat(sprintf("  Median: %.1f seconds\n", median(completed$runtime_sec)))
  cat(sprintf("  Total: %.1f minutes\n", sum(completed$runtime_sec) / 60))
  
  cat(sprintf("\nFile size statistics:\n"))
  cat(sprintf("  Mean: %.1f MB\n", mean(completed$file_size_mb)))
  cat(sprintf("  Total: %.1f GB\n", sum(completed$file_size_mb) / 1024))
}

cat(sprintf("\nEnd time: %s\n", Sys.time()))
cat(sprintf("Progress log: %s\n", progress_file))
