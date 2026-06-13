#!/usr/bin/env Rscript
################################################################################
# Install Required Packages for Simon Pipeline
################################################################################

cat("=== Installing Required Packages ===\n\n")

# List of required packages
required_packages <- c(
  "data.table",      # Fast data manipulation
  "arrow",           # Parquet file reading
  "dplyr",           # Data manipulation
  "dlnm",            # Distributed lag non-linear models
  "splines",         # Spline functions (usually base R)
  "ggplot2",         # Visualization
  "scales",          # Scales for ggplot2
  "future",          # Parallel processing backend
  "future.apply",    # Parallel apply functions
  "optparse"         # Command-line argument parsing
)

# Check which packages are missing
installed <- installed.packages()[, "Package"]
missing <- required_packages[!required_packages %in% installed]

if (length(missing) == 0) {
  cat("✓ All required packages are already installed!\n")
} else {
  cat(sprintf("Installing %d missing packages:\n", length(missing)))
  cat(paste("  -", missing, collapse = "\n"), "\n\n")
  
  # Install missing packages
  for (pkg in missing) {
    cat(sprintf("Installing %s...\n", pkg))
    tryCatch({
      install.packages(pkg, repos = "https://cloud.r-project.org/", 
                      dependencies = TRUE, quiet = TRUE)
      cat(sprintf("  ✓ %s installed successfully\n", pkg))
    }, error = function(e) {
      cat(sprintf("  ✗ Failed to install %s: %s\n", pkg, e$message))
    })
  }
}

cat("\n=== Package Installation Complete ===\n\n")

# Verify installation
cat("Verifying installations:\n")
for (pkg in required_packages) {
  status <- if (requireNamespace(pkg, quietly = TRUE)) "✓" else "✗"
  cat(sprintf("  %s %s\n", status, pkg))
}

cat("\n")
cat("If all packages show ✓, you're ready to run the pipeline!\n")
cat("Run: ./run_pipeline.sh --test --parallel 4\n")
