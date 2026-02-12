#!/bin/bash
# ==============================================================================
# Wrapper script to convert CORDEX NetCDF data to city-level CSV
# ==============================================================================

set -e

cd "$(dirname "$0")"

echo "=========================================="
echo "CORDEX to City CSV Converter"
echo "=========================================="
echo ""

# Check if raw data exists
if [ ! -d "raw" ] || [ -z "$(ls -A raw/*.nc 2>/dev/null)" ]; then
    echo "ERROR: No NetCDF files found in raw/"
    echo "Please ensure CORDEX data is downloaded first."
    exit 1
fi

# Count available files
nc_count=$(ls -1 raw/*.nc 2>/dev/null | wc -l)
echo "Found $nc_count NetCDF files in raw/"
echo ""

# Check R installation
if ! command -v Rscript &> /dev/null; then
    echo "ERROR: Rscript not found"
    echo "Please install R: sudo apt install r-base"
    exit 1
fi

# Check required R packages
echo "Checking R dependencies..."
Rscript -e "
packages <- c('ncdf4', 'data.table', 'sf')
missing <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  cat('Missing R packages:', paste(missing, collapse=', '), '\n')
  cat('Installing...\n')
  install.packages(missing, repos='https://cloud.r-project.org/', quiet=TRUE)
}
cat('All dependencies satisfied\n')
"

echo ""
echo "=========================================="
echo "Starting data processing..."
echo "This may take several hours depending on"
echo "the number of files and cities."
echo "=========================================="
echo ""

# Run the R script
Rscript process_to_csv.R 2>&1 | tee logs/processing_$(date +%Y%m%d_%H%M%S).log

exit_code=${PIPESTATUS[0]}

if [ $exit_code -eq 0 ]; then
    echo ""
    echo "=========================================="
    echo "SUCCESS!"
    echo "=========================================="
    echo ""
    echo "Output file: processed/city_daily_temperature.csv"
    echo ""
    
    # Show file info
    if [ -f "processed/city_daily_temperature.csv" ]; then
        lines=$(wc -l < processed/city_daily_temperature.csv)
        size=$(du -h processed/city_daily_temperature.csv | cut -f1)
        echo "File size: $size"
        echo "Total rows: $lines"
        echo ""
        echo "Preview (first 5 lines):"
        head -5 processed/city_daily_temperature.csv
    fi
else
    echo ""
    echo "=========================================="
    echo "ERROR: Processing failed"
    echo "Check logs for details"
    echo "=========================================="
    exit 1
fi
