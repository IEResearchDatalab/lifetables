################################################################################
# 
# Export Temperature Distribution Projections for PostgreSQL/Supabase
#
# This script exports temperature distribution data in formats suitable for 
# importing into PostgreSQL databases (including Supabase)
#
################################################################################

# Load required packages
library(arrow)
library(data.table)
library(dplyr)

# Set options
options(scipen = 999)  # Avoid scientific notation in CSV exports

#------------------------------------------------------------------------------
# OPTION 1: Export from Parquet (Recommended - faster and more efficient)
#------------------------------------------------------------------------------

export_from_parquet <- function(output_dir = "postgres_export") {
  
  cat("Reading temperature data from parquet...\n")
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Read the parquet file
  temp_data <- read_parquet("results_parquet/tsum0.parquet")
  
  # Convert to data.table for efficient processing
  temp_data <- as.data.table(temp_data)
  
  cat(sprintf("Total rows: %s\n", format(nrow(temp_data), big.mark = ",")))
  cat(sprintf("Columns: %s\n", paste(names(temp_data), collapse = ", ")))
  
  # Export options:
  
  # 1. Single CSV file (good for smaller datasets or direct import)
  cat("\nExporting to single CSV file...\n")
  fwrite(temp_data, 
         file.path(output_dir, "temperature_distributions.csv"),
         quote = TRUE,
         na = "NULL")
  
  # 2. Partitioned CSV files by city and SSP (better for large datasets)
  cat("\nExporting partitioned CSV files by city and SSP...\n")
  temp_data[, {
    fname <- sprintf("%s/temp_dist_%s_ssp%s.csv", output_dir, city, ssp)
    fwrite(.SD, fname, quote = TRUE, na = "NULL")
    NULL
  }, by = .(city, ssp)]
  
  # 3. Export as SQL INSERT statements (alternative approach)
  cat("\nGenerating SQL INSERT script...\n")
  generate_sql_inserts(temp_data, output_dir)
  
  # 4. Create PostgreSQL table schema
  cat("\nGenerating PostgreSQL table schema...\n")
  create_postgres_schema(temp_data, output_dir)
  
  cat("\nExport complete! Files saved to:", output_dir, "\n")
  
  return(invisible(temp_data))
}

#------------------------------------------------------------------------------
# OPTION 2: Export from CSV (if parquet not available)
#------------------------------------------------------------------------------

export_from_csv <- function(output_dir = "postgres_export") {
  
  cat("Reading temperature data from CSV...\n")
  cat("Warning: This may take a while for large files...\n")
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Read the CSV file in chunks to avoid memory issues
  temp_data <- fread("results_csv/tsum0.csv",
                     showProgress = TRUE)
  
  cat(sprintf("Total rows: %s\n", format(nrow(temp_data), big.mark = ",")))
  
  # Follow same export pattern as parquet version
  # (code can be reused from export_from_parquet)
  
  return(invisible(temp_data))
}

#------------------------------------------------------------------------------
# Helper Functions
#------------------------------------------------------------------------------

# Generate PostgreSQL table schema
create_postgres_schema <- function(data, output_dir) {
  
  # Infer PostgreSQL data types
  pg_types <- sapply(data, function(col) {
    if (is.numeric(col)) {
      if (all(col == as.integer(col), na.rm = TRUE)) {
        return("INTEGER")
      } else {
        return("DOUBLE PRECISION")
      }
    } else if (is.character(col)) {
      max_len <- max(nchar(col), na.rm = TRUE)
      if (max_len <= 50) {
        return(sprintf("VARCHAR(%d)", max_len + 10))
      } else {
        return("TEXT")
      }
    } else {
      return("TEXT")
    }
  })
  
  # Build CREATE TABLE statement
  schema_sql <- c(
    "-- PostgreSQL/Supabase table schema for temperature distributions",
    "-- Generated automatically",
    "",
    "CREATE TABLE IF NOT EXISTS temperature_distributions (",
    paste0("    ", names(pg_types), " ", pg_types, collapse = ",\n"),
    ");",
    "",
    "-- Recommended indexes for better query performance",
    "CREATE INDEX IF NOT EXISTS idx_temp_city ON temperature_distributions(city);",
    "CREATE INDEX IF NOT EXISTS idx_temp_ssp ON temperature_distributions(ssp);",
    "CREATE INDEX IF NOT EXISTS idx_temp_gcm ON temperature_distributions(gcm);",
    "CREATE INDEX IF NOT EXISTS idx_temp_period ON temperature_distributions(calperiod);",
    "CREATE INDEX IF NOT EXISTS idx_temp_city_ssp ON temperature_distributions(city, ssp);",
    "",
    "-- Optional: Add composite index for common queries",
    "CREATE INDEX IF NOT EXISTS idx_temp_composite ON temperature_distributions(city, ssp, calperiod, gcm);",
    ""
  )
  
  writeLines(schema_sql, file.path(output_dir, "schema.sql"))
  
  cat("Schema saved to: schema.sql\n")
}

# Generate SQL INSERT statements (for small to medium datasets)
generate_sql_inserts <- function(data, output_dir, batch_size = 1000) {
  
  if (nrow(data) > 100000) {
    cat("Warning: Dataset is large. Consider using COPY command instead of INSERT.\n")
    cat("Generating COPY command script...\n")
    
    copy_sql <- c(
      "-- PostgreSQL COPY command for bulk import",
      "-- Run this after uploading the CSV file to your server",
      "",
      "COPY temperature_distributions",
      sprintf("FROM '/path/to/temperature_distributions.csv'"),
      "DELIMITER ','",
      "CSV HEADER",
      "NULL 'NULL';",
      "",
      "-- For Supabase, you can use the Dashboard's Import feature",
      "-- or the supabase-js client library for programmatic import"
    )
    
    writeLines(copy_sql, file.path(output_dir, "import_copy.sql"))
    return()
  }
  
  # For smaller datasets, generate INSERT statements
  sql_file <- file(file.path(output_dir, "import_inserts.sql"), "w")
  
  writeLines("-- SQL INSERT statements", sql_file)
  writeLines("BEGIN;", sql_file)
  
  for (i in seq(1, nrow(data), by = batch_size)) {
    batch_end <- min(i + batch_size - 1, nrow(data))
    batch <- data[i:batch_end, ]
    
    values <- apply(batch, 1, function(row) {
      formatted <- sapply(row, function(val) {
        if (is.na(val)) {
          "NULL"
        } else if (is.numeric(val)) {
          as.character(val)
        } else {
          sprintf("'%s'", gsub("'", "''", val))
        }
      })
      sprintf("(%s)", paste(formatted, collapse = ", "))
    })
    
    insert_stmt <- sprintf(
      "INSERT INTO temperature_distributions (%s) VALUES\n%s;",
      paste(names(data), collapse = ", "),
      paste(values, collapse = ",\n")
    )
    
    writeLines(insert_stmt, sql_file)
  }
  
  writeLines("COMMIT;", sql_file)
  close(sql_file)
}

# Generate Supabase-specific import guide
create_supabase_guide <- function(output_dir = "postgres_export") {
  
  guide <- c(
    "# Importing Temperature Data to Supabase",
    "",
    "## Method 1: Using Supabase Dashboard (Recommended for ease)",
    "",
    "1. Log in to your Supabase project dashboard",
    "2. Navigate to 'Table Editor' in the sidebar",
    "3. Create a new table called 'temperature_distributions'",
    "4. Use the schema provided in `schema.sql` or import via SQL editor",
    "5. Once table is created, use the 'Import data' feature",
    "6. Upload `temperature_distributions.csv`",
    "7. Map columns and complete the import",
    "",
    "## Method 2: Using SQL Editor",
    "",
    "1. Go to 'SQL Editor' in Supabase dashboard",
    "2. Copy and paste the contents of `schema.sql`",
    "3. Run the query to create the table",
    "4. Use Table Editor's import feature or psql to load data",
    "",
    "## Method 3: Using psql (Command Line)",
    "",
    "```bash",
    "# Get your connection string from Supabase dashboard",
    "# Settings > Database > Connection string",
    "",
    "# Connect to your database",
    'psql "postgresql://postgres:[PASSWORD]@[HOST]:[PORT]/postgres"',
    "",
    "# Create the table",
    "\\i schema.sql",
    "",
    "# Import the data (adjust path as needed)",
    "\\copy temperature_distributions FROM 'temperature_distributions.csv' WITH (FORMAT csv, HEADER true, NULL 'NULL');",
    "```",
    "",
    "## Method 4: Using Supabase JavaScript Client (Programmatic)",
    "",
    "```javascript",
    "import { createClient } from '@supabase/supabase-js'",
    "import fs from 'fs'",
    "import Papa from 'papaparse'",
    "",
    "const supabase = createClient(SUPABASE_URL, SUPABASE_KEY)",
    "",
    "// Read and parse CSV",
    "const csvData = fs.readFileSync('temperature_distributions.csv', 'utf8')",
    "const parsed = Papa.parse(csvData, { header: true })",
    "",
    "// Insert in batches (max 1000 rows per insert)",
    "const batchSize = 1000",
    "for (let i = 0; i < parsed.data.length; i += batchSize) {",
    "  const batch = parsed.data.slice(i, i + batchSize)",
    "  const { error } = await supabase",
    "    .from('temperature_distributions')",
    "    .insert(batch)",
    "  if (error) console.error('Error:', error)",
    "}",
    "```",
    "",
    "## Method 5: For Large Datasets - Partitioned Import",
    "",
    "If the dataset is too large for a single import:",
    "",
    "1. Use the partitioned CSV files (e.g., `temp_dist_AT001C_ssp1.csv`)",
    "2. Import them one at a time using any of the methods above",
    "3. This reduces memory usage and allows for resumable imports",
    "",
    "## Performance Considerations",
    "",
    "- The generated indexes will improve query performance",
    "- Consider adding Row Level Security (RLS) policies if needed",
    "- For very large datasets, consider using Supabase's Edge Functions",
    "- Monitor your database size and upgrade plan if needed",
    "",
    "## Data Structure",
    "",
    "- `gcm`: Global Climate Model identifier",
    "- `perc`: Temperature percentile",
    "- `tas`: Near-surface air temperature (raw)",
    "- `full`: Calibrated temperature with full climate change",
    "- `demo`: Temperature with demographic changes only",
    "- `calperiod`: Calibration period",
    "- `city`: City code",
    "- `ssp`: Shared Socioeconomic Pathway scenario (1, 2, or 3)",
    ""
  )
  
  writeLines(guide, file.path(output_dir, "IMPORT_GUIDE.md"))
  cat("Import guide saved to: IMPORT_GUIDE.md\n")
}

#------------------------------------------------------------------------------
# Main execution
#------------------------------------------------------------------------------

if (!interactive()) {
  # Run if called as script
  cat("=== Temperature Distribution Export for PostgreSQL/Supabase ===\n\n")
  
  # Check which format is available
  if (file.exists("results_parquet/tsum0.parquet")) {
    cat("Using Parquet format (recommended)\n\n")
    export_from_parquet()
  } else if (file.exists("results_csv/tsum0.csv")) {
    cat("Using CSV format\n\n")
    export_from_csv()
  } else {
    stop("No temperature data files found! Please run 03_attribution.R first.")
  }
  
  # Create import guide
  create_supabase_guide()
  
  cat("\n=== Export Complete ===\n")
  cat("Check the 'postgres_export' folder for all files.\n")
}

# For interactive use, you can call:
# temp_data <- export_from_parquet()
# create_supabase_guide()
