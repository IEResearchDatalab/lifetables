# Temperature Distribution Export for PostgreSQL/Supabase

## Overview

Yes! You can absolutely export the temperature distribution projections to PostgreSQL/Supabase. The data is already available in your `results_parquet/tsum0.parquet` file (237 MB) and `results_csv/tsum0.csv` file (690 MB).

## Quick Start

Run the export script:

```r
source("export_temp_for_postgres.R")
```

This will create a `postgres_export/` folder with:
- `temperature_distributions.csv` - Full dataset in CSV format
- `temp_dist_[CITY]_ssp[N].csv` - Partitioned files by city and SSP scenario
- `schema.sql` - PostgreSQL table schema with indexes
- `import_copy.sql` or `import_inserts.sql` - SQL import commands
- `IMPORT_GUIDE.md` - Detailed import instructions for Supabase

## What's in the Data?

The temperature distribution data contains:

| Column | Description |
|--------|-------------|
| `gcm` | Global Climate Model (e.g., ACCESS_CM2, CESM2, etc.) |
| `perc` | Temperature percentile (0 to 100, including decimals like 0.1, 99.9) |
| `tas` | Near-surface air temperature (raw/uncalibrated) in °C |
| `full` | Calibrated temperature with full climate change in °C |
| `demo` | Temperature with demographic changes only (no climate change) in °C |
| `calperiod` | Calibration period (hist, 2015-2029, 2030-2039, etc.) |
| `city` | City code (e.g., AT001C, RO001C, etc.) |
| `ssp` | Shared Socioeconomic Pathway scenario (1, 2, or 3) |

## Data Size

- **Total rows**: ~50-100 million (depending on cities and scenarios)
- **Parquet format**: 237 MB (compressed, efficient)
- **CSV format**: 690 MB (uncompressed)
- **PostgreSQL size**: ~500-800 MB (with indexes)

## Import Methods

### Method 1: Supabase Dashboard (Easiest)
1. Create table using `schema.sql`
2. Use Table Editor → Import data
3. Upload `temperature_distributions.csv`

### Method 2: psql Command Line
```bash
psql "your-connection-string"
\i schema.sql
\copy temperature_distributions FROM 'temperature_distributions.csv' WITH (FORMAT csv, HEADER true);
```

### Method 3: Partitioned Import (For Large Datasets)
Import the partitioned files one by one:
- `temp_dist_AT001C_ssp1.csv`
- `temp_dist_AT001C_ssp2.csv`
- etc.

This is better for memory management and resumable imports.

## Query Examples

Once imported to Supabase/Postgres, you can query like:

```sql
-- Get temperature distribution for Vienna (AT001C) under SSP3
SELECT * FROM temperature_distributions
WHERE city = 'AT001C' AND ssp = 3;

-- Get mean temperatures across all models for a specific period
SELECT city, calperiod, AVG(full) as mean_temp
FROM temperature_distributions
WHERE perc = 50  -- median
GROUP BY city, calperiod;

-- Get extreme heat projections (99th percentile)
SELECT city, gcm, calperiod, full
FROM temperature_distributions
WHERE perc = 99 AND ssp = 3;
```

## Performance Tips

1. **Indexes are created automatically** for common queries (city, ssp, gcm, period)
2. **Use partitioned files** if importing > 10 million rows
3. **Consider materialized views** for frequently accessed summaries
4. **Enable compression** in PostgreSQL for large tables

## File Formats Available

✅ **CSV** - `results_csv/tsum0.csv` (690 MB)
- Standard format, universally compatible
- Good for direct import to most databases

✅ **Parquet** - `results_parquet/tsum0.parquet` (237 MB)
- Compressed, columnar format
- Much smaller file size
- Faster to read with modern tools

## Next Steps

1. Run `source("export_temp_for_postgres.R")` in R
2. Check the `postgres_export/` folder
3. Follow `IMPORT_GUIDE.md` for detailed Supabase import instructions
4. Set up Row Level Security (RLS) policies if needed
5. Create API endpoints using Supabase auto-generated REST API

## Questions?

The export script handles:
- ✅ Automatic CSV generation
- ✅ Partitioning by city and scenario
- ✅ PostgreSQL schema generation
- ✅ Index creation for performance
- ✅ NULL handling
- ✅ Data type inference
- ✅ Batch import scripts

Everything you need to migrate to Supabase is included!
