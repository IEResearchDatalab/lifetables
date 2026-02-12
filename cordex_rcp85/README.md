# CORDEX EUR-11 Data Processing Pipeline

This directory contains scripts to download and process EURO-CORDEX climate projections for European cities.

## Directory Structure

```
cordex_data/  # NOT COMMITTED — contains raw NetCDF files and processed CSV outputs
├── raw/                          # Downloaded NetCDF files (not in git)
├── processed/                    # Processed CSV output (not in git)
├── logs/                         # Processing logs
├── esgf_credentials.sh          # ESGF login credentials (DO NOT COMMIT)
cordex_rcp85/
├── process_to_csv.R             # Main processing script
├── run_processing.sh            # Processing wrapper
└── README.md                    # This file
```

## Step 1: Download Data

### Prerequisites
- ESGF account (register at https://esg-dn1.nsc.liu.se/)
- `wget`, `screen` installed

### Download Process

1. **Edit credentials** (if not done already):
   ```bash
   nano cordex_data/esgf_credentials.sh
   # Add your ESGF OpenID, username, and password
   chmod 600 cordex_data/esgf_credentials.sh
   ```

2. **Start download**:
   ```bash
   bash cordex_data/interactive_download.sh
   ```

3. **Enter password when prompted**:
   ```bash
   screen -r cordex_download
   # Type your password, then Ctrl+A, D to detach
   ```

4. **Monitor progress**:
   ```bash
   # Count downloaded files (target: 266)
   find cordex_data/raw -name "*.nc" -size +10k | wc -l
   
   # Check disk usage
   du -sh cordex_data/raw/
   
   # View currently downloading file
   ps aux | grep wget | grep -v grep
   ```

### Downloaded Data Specs
- **Project**: CORDEX EUR-11
- **Scenario**: RCP8.5
- **Variable**: tas (near-surface air temperature, daily)
- **Models**: 10 GCM-RCM combinations
- **Period**: 2006-2100
- **Size**: ~50-100 GB

## Step 2: Process to City-Level CSV

### Prerequisites
- R (≥ 4.0)
- R packages: `ncdf4`, `data.table`, `sf`

### Install R (if needed)
```bash
sudo apt update
sudo apt install r-base r-base-dev libnetcdf-dev libudunits2-dev libgdal-dev
```

### Run Processing

```bash
cd /home/daniprec/SHARED/lifetables/cordex_data
bash cordex_rcp85/run_processing.sh
```

The script will:
1. Check dependencies (auto-install missing R packages)
2. Read all NetCDF files in `cordex_data/raw/`
3. Extract temperature for each city (nearest grid point)
4. Convert Kelvin → Celsius
5. Reshape to wide format (one column per model)
6. Output to `cordex_data/processed/city_daily_temperature.csv`

### Output Format

```csv
city,date,MODEL1_RCM1_r1i1p1,MODEL2_RCM2_r1i1p1,...
Amsterdam,2006-01-01,2.3,2.5,...
Amsterdam,2006-01-02,3.1,3.2,...
Athens,2006-01-01,12.4,12.7,...
...
```

**Columns:**
- `city`: City name
- `date`: Date (YYYY-MM-DD)
- One column per GCM-RCM combination (temperature in °C)

### Cities Included

**European Capitals (20):**
Amsterdam, Athens, Berlin, Brussels, Bucharest, Budapest, Copenhagen, Dublin, 
Helsinki, Lisbon, London, Madrid, Oslo, Paris, Prague, Rome, Stockholm, 
Vienna, Warsaw, Zurich

**Romanian Cities (38):**
All cities from the lifetable analysis (RO001C-RO035C): Alba Iulia, Arad, 
Bacau, Baia Mare, Bistrita, Botosani, Braila, Brasov, Buzau, Calarasi, 
Cluj-Napoca, Constanta, Craiova, Deva, Drobeta-Turnu Severin, Focsani, 
Galati, Giurgiu, Iasi, Miercurea Ciuc, Oradea, Piatra Neamt, Pitesti, 
Ploiesti, Ramnicu Valcea, Resita, Satu Mare, Sibiu, Slatina, Slobozia, 
Suceava, Targoviste, Targu Jiu, Targu Mures, Timisoara, Tulcea, Vaslui, Zalau

**Total: 58 cities**

## Processing Time Estimates

- Download: 6-24 hours (depending on ESGF server load)
- Processing: 2-6 hours (depending on CPU)

## Troubleshooting

### Download Issues

**Problem**: Authentication fails
```bash
# Solution: Re-run with interactive mode
bash cordex_rcp85/wget-20260210190800.sh -H -o "YOUR_OPENID" -I "YOUR_USERNAME"
# Enter password when prompted
```

**Problem**: Download stalls
```bash
# Check if still running
ps aux | grep wget-20260210190800.sh

# If stopped, resume (will skip completed files)
bash cordex_rcp85/wget-20260210190800.sh -H -o "YOUR_OPENID" -I "YOUR_USERNAME"
```

### Processing Issues

**Problem**: "Cannot open NetCDF file"
```bash
# Check file integrity
ncdump -h cordex_data/raw/tas_EUR-11_*.nc | head -20

# If corrupt, re-download specific file
```

**Problem**: Missing R packages
```bash
# Install manually
R
> install.packages(c("ncdf4", "data.table", "sf"))
```

## Advanced Usage

### Process Subset of Cities

Edit `cordex_rcp85/process_to_csv.R`, modify the `cities` data.table to include only desired cities.

### Change Output Format

To get long format instead of wide:
```R
# In process_to_csv.R, replace the dcast line with:
# Skip the reshape, output combined_data directly
fwrite(combined_data, output_path)
```

### Extract Specific Time Period

Add filtering before writing output:
```R
# In process_to_csv.R, before fwrite:
wide_data <- wide_data[date >= as.Date("2020-01-01") & date <= as.Date("2050-12-31")]
```

## Data Citation

When using this data, cite:
- EURO-CORDEX: https://www.euro-cordex.net/
- Individual modeling centers (see file metadata)

## Support

For issues with:
- ESGF download: https://esgf.github.io/esgf-user-support/
- CORDEX data: https://euro-cordex.net/contact
- This pipeline: Check logs in `logs/` directory
