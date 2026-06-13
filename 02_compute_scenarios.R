#!/usr/bin/env Rscript
################################################################################
# Stage 2: Compute Scenario Grid
# 
# This script runs all scenario combinations:
# - All cities × SSPs (2, 3) × Adaptations (0-100% by 10%)
#
# Usage: Rscript 02_compute_scenarios.R [--parallel N] [--detailed]
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
  make_option(c("-d", "--detailed"), action = "store_true", default = FALSE,
              help = "Save detailed outputs (lifetables, multipliers)"),
  make_option(c("-r", "--resume"), action = "store_true", default = FALSE,
              help = "Resume: skip already completed scenarios"),
  make_option(c("--cities"), type = "character", default = NULL,
              help = "Comma-separated list of specific cities (e.g., 'AT001C,ES001C')")
)

opt <- parse_args(OptionParser(option_list = option_list))

cat("\n=== Stage 2: Scenario Grid Computation ===\n")
cat(sprintf("Workers: %d\n", opt$parallel))
cat(sprintf("Test mode: %s\n", opt$test))
cat(sprintf("Detailed outputs: %s\n", opt$detailed))
cat(sprintf("Resume mode: %s\n", opt$resume))
cat(sprintf("Start time: %s\n\n", Sys.time()))

# Source functions
source("pipeline_functions.R")

# Create output directories
dir.create("results", showWarnings = FALSE, recursive = TRUE)
dir.create("logs", showWarnings = FALSE, recursive = TRUE)

# Get list of precomputed cities
precomputed_files <- list.files("precomputed/cities", pattern = "\\.rds$", 
                                full.names = FALSE)
city_codes <- gsub("\\.rds$", "", precomputed_files)

if (length(city_codes) == 0) {
  stop("No precomputed cities found. Run 01_precompute_cities.R first.")
}

# Filter to specific cities if requested
if (!is.null(opt$cities)) {
  requested_cities <- strsplit(opt$cities, ",")[[1]]
  city_codes <- intersect(city_codes, requested_cities)
  cat(sprintf("Filtering to %d requested cities\n", length(city_codes)))
}

if (opt$test) {
  cat("TEST MODE: Running first 3 cities only\n")
  city_codes <- head(city_codes, 3)
}

cat(sprintf("Cities: %d\n", length(city_codes)))

# Define scenario grid
ssps <- c(2, 3)
adaptations <- seq(0, 1, 0.1)

# Generate task manifest
task_manifest <- CJ(
  city_code = city_codes,
  ssp = ssps,
  adaptation = adaptations
)
task_manifest[, task_id := .I]
task_manifest[, status := "pending"]
task_manifest[, start_time := as.POSIXct(NA)]
task_manifest[, end_time := as.POSIXct(NA)]
task_manifest[, runtime_sec := NA_real_]
task_manifest[, error_msg := NA_character_]

cat(sprintf("Total scenarios: %s\n", format(nrow(task_manifest), big.mark = ",")))
cat(sprintf("  = %d cities × %d SSPs × %d adaptations\n\n", 
            length(city_codes), length(ssps), length(adaptations)))

# Save manifest
manifest_file <- "logs/task_manifest.csv"
fwrite(task_manifest, manifest_file)

# Resume mode: load existing results
if (opt$resume) {
  if (file.exists("results/lifespan_inequality_summary.csv")) {
    existing_results <- fread("results/lifespan_inequality_summary.csv")
    
    # Mark completed tasks
    task_manifest <- merge(
      task_manifest,
      existing_results[, .(city_code, ssp, adaptation, completed = TRUE)],
      by = c("city_code", "ssp", "adaptation"),
      all.x = TRUE
    )
    task_manifest[completed == TRUE, status := "completed"]
    task_manifest[, completed := NULL]
    
    n_completed <- sum(task_manifest$status == "completed")
    n_pending <- sum(task_manifest$status == "pending")
    
    cat(sprintf("Resume mode: %s already completed\n", 
                format(n_completed, big.mark = ",")))
    cat(sprintf("Remaining: %s scenarios\n\n", 
                format(n_pending, big.mark = ",")))
    
    # Filter to pending only
    task_manifest <- task_manifest[status == "pending"]
  }
}

# Set up parallel processing
plan(multisession, workers = opt$parallel)

# Run scenarios in batches (by city to maximize data reuse)
cat("Starting parallel computation...\n\n")

# Group tasks by city
tasks_by_city <- split(task_manifest, task_manifest$city_code)

all_results <- list()
batch_counter <- 0

for (city in names(tasks_by_city)) {
  city_tasks <- tasks_by_city[[city]]
  
  cat(sprintf("[%s] Processing %s: %d scenarios\n", 
              Sys.time(), city, nrow(city_tasks)))
  
  # Run all scenarios for this city in parallel
  city_results <- future_lapply(1:nrow(city_tasks), function(i) {
    task <- city_tasks[i]
    
    tryCatch({
      start_time <- Sys.time()
      
      result <- compute_scenario(
        city_code = task$city_code,
        ssp = task$ssp,
        adaptation = task$adaptation,
        output_detailed = opt$detailed
      )
      
      if (!is.null(result)) {
        result
      } else {
        NULL
      }
      
    }, error = function(e) {
      cat(sprintf("[ERROR] %s | SSP%d | %.0f%%: %s\n",
                  task$city_code, task$ssp, task$adaptation * 100, 
                  e$message))
      NULL
    })
  }, future.seed = TRUE)
  
  # Append results
  city_results_valid <- Filter(Negate(is.null), city_results)
  if (length(city_results_valid) > 0) {
    all_results <- c(all_results, city_results_valid)
  }
  
  # Save intermediate results every 10 cities
  batch_counter <- batch_counter + 1
  if (batch_counter %% 10 == 0) {
    cat(sprintf("[%s] Saving intermediate results (%d cities completed)\n",
                Sys.time(), batch_counter))
    
    results_dt <- rbindlist(all_results, fill = TRUE)
    
    if (opt$resume && file.exists("results/lifespan_inequality_summary.csv")) {
      # Append to existing
      existing <- fread("results/lifespan_inequality_summary.csv")
      results_dt <- rbind(existing, results_dt)
    }
    
    fwrite(results_dt, "results/lifespan_inequality_summary.csv")
  }
  
  # Clean up
  gc()
}

# Final save
cat("\n=== Stage 2 Complete ===\n\n")

if (length(all_results) > 0) {
  results_dt <- rbindlist(all_results, fill = TRUE)
  
  # If resuming, append to existing
  if (opt$resume && file.exists("results/lifespan_inequality_summary.csv")) {
    existing <- fread("results/lifespan_inequality_summary.csv")
    results_dt <- rbind(existing, results_dt)
    # Remove duplicates (in case of overlap)
    results_dt <- unique(results_dt, by = c("city_code", "ssp", "adaptation"))
  }
  
  fwrite(results_dt, "results/lifespan_inequality_summary.csv")
  
  cat(sprintf("Results saved: results/lifespan_inequality_summary.csv\n"))
  cat(sprintf("Total rows: %s\n", format(nrow(results_dt), big.mark = ",")))
  cat(sprintf("File size: %.1f MB\n", 
              file.size("results/lifespan_inequality_summary.csv") / 1024^2))
  
  # Summary statistics
  cat("\n=== Summary Statistics ===\n\n")
  
  cat("Scenarios by status:\n")
  print(table(results_dt[, .(city_code, ssp, adaptation)][, .N > 0]))
  
  cat("\nRuntime statistics:\n")
  cat(sprintf("  Mean: %.1f seconds\n", mean(results_dt$runtime_sec, na.rm = TRUE)))
  cat(sprintf("  Median: %.1f seconds\n", median(results_dt$runtime_sec, na.rm = TRUE)))
  cat(sprintf("  Total: %.1f hours\n", sum(results_dt$runtime_sec, na.rm = TRUE) / 3600))
  
  cat("\nKey findings preview:\n")
  cat(sprintf("  Cities with LE reduction: %d\n", 
              sum(results_dt$delta_e20 < 0, na.rm = TRUE)))
  cat(sprintf("  Cities with LI increase: %d\n", 
              sum(results_dt$delta_gini > 0, na.rm = TRUE)))
  
} else {
  cat("WARNING: No results generated\n")
}

cat(sprintf("\nEnd time: %s\n", Sys.time()))
cat(sprintf("Task manifest: %s\n", manifest_file))
