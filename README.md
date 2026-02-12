# Climate-Adjusted Life Tables for European Cities

This project incorporates temperature-dependent mortality into projected life tables at the city level, following the methodology described in the accompanying working paper (`main.tex`).

## Quick Start (Reproduce Outputs for Bucharest)

From the project root, run each script in order:

```bash
Rscript bucharest_lifetable_pclm.R          # Table 1 — PCLM life table
Rscript plot_rr_curves.R                    # Figure 1 — RR curves by age group
Rscript plot_rr_poster.R                    # Figure 2 — interpolated RR surface
Rscript plot_bucharest_temp_dist_rr.R       # Figure 3 — temp distribution + RR overlay
Rscript plot_bucharest_temp_evolution.R     # Figure 4 — projected temp evolution
Rscript compute_mortality_multiplier_by_age.R  # Figure 5 — mortality multiplier
Rscript compute_cohort_lifetable_financial.R   # Table 5 — financial impact (EPV)
```

All outputs land in `img/` (figures) and `results_csv/` (tables). The LaTeX table is written to `<city>_lifetable_2023.tex` in the project root.

### Required Data

The following additional files must be located inside the root folder. These files do not come directly from the Zenodo repository and must be downloaded separately.

- `cordex_data/` - CORDEX EUR-11 RCP8.5 projections. Download and process with `cordex_rcp85/` scripts. You will find a README in that folder with instructions.
- `data/2023_temp/` - ERA5 daily mean temperature for 2023. Download with `data/2023_temp/download_era5_2023.py`.
- `data/POP106A.csv` and `data/POP206K.csv` - ToDo.
- `data/2.62.csv` and `data/2.63.csv` - ToDo.


---

## Generating for a Different City

All city-specific parameters are centralised in **`config.R`**. To switch cities, follow the steps below.

### Step 1 — Edit `config.R`

Change the three identifiers at the top of the file:

```r
city_code  <- "ES001C"       # URAU code (Urban Audit)
city_name  <- "Madrid"       # Human-readable name (used in titles, file names)
nuts3_code <- "ES300"        # NUTS 3 code (Eurostat projections)
```

All output paths are derived automatically from `city_name`.

#### Finding the right codes

| Code | Where to look |
| :--- | :--- |
| **URAU code** | Must appear in `data/coefs.csv` (854 cities available). List them with: `Rscript -e "d <- data.table::fread('data/coefs.csv'); print(unique(d[['URAU_CODE']]))"` |
| **NUTS 3 code** | [Eurostat NUTS classification](https://ec.europa.eu/eurostat/web/nuts/background). Match the city's NUTS 3 region. |
| **City name** | Any human-readable label — used for plot titles, file names, and filtering Eurostat data. Must match the region label in the Eurostat EUROPOP2019 xlsx files (column `geo`). |

### Step 2 — Prepare City-Specific Data

Three data inputs are city-specific and must be provided or adapted before running the pipeline:

#### 2a. ERA5 2023 Daily Temperature

Edit `data/2023_temp/download_era5_2023.py` and change the bounding box to cover the new city:

```python
"area": [lat_north, lon_west, lat_south, lon_east]  # e.g. [40.6, -3.9, 40.2, -3.5] for Madrid
```

Also update the output filename prefix to match `city_name` (lowercase):

```python
output_file = f"madrid_2023_{month}.nc"
```

Then run the download (requires a CDS API key — see [CDS registration](https://cds.climate.copernicus.eu/)):

```bash
python data/2023_temp/download_era5_2023.py
```

#### 2b. Eurostat EUROPOP2019 Mortality Projections

The script `data/extract_eurostat_projections.R` reads the Eurostat xlsx files and filters by city name. Ensure these files exist in `data/`:

- `proj_19raasmr3__custom_*.xlsx` — probability of dying (qx) by age, sex, NUTS 3
- `proj_19rp3__custom_*.xlsx` — population by age, sex, NUTS 3

Download them from [Eurostat](https://ec.europa.eu/eurostat/databrowser/view/proj_19raasmr3/default/table) for the target NUTS 3 region. Then run:

```bash
Rscript data/extract_eurostat_projections.R
```

This produces `data/<city>_mortality_projections.csv`.

#### 2c. Baseline Life Table Data (Table 1)

`bucharest_lifetable_pclm.R` currently uses Romanian National Institute of Statistics data (`data/POP106A.csv`, `data/POP206K.csv`). For a different country, you need to supply equivalent 5-year grouped population and death counts and update the data loading section at the top of the script to match the new source format.

### Step 3 — Run the Pipeline

Once the data is in place, run the same scripts as in the Quick Start section. All output file names automatically incorporate the city name from `config.R`.

### Step 4 — Update `main.tex` References

The LaTeX file `main.tex` references output files by name. After switching cities, update the `\input{}` and `\includegraphics{}` paths. The key references are:

| LaTeX command | File produced |
| :--- | :--- |
| `\input{<city>_lifetable_2023.tex}` | Life table (Table 1) |
| `\includegraphics{img/<City>_RR_curves.pdf}` | Figure 1 |
| `\includegraphics{img/rr_poster_comparison_ie.png}` | Figure 2 |
| `\includegraphics{img/<city>_temp_dist_rr.pdf}` | Figure 3 |
| `\includegraphics{img/<city>_temp_evolution.pdf}` | Figure 4 |
| `\includegraphics{img/sample_multi.jpg}` | Figure 5 |

---

## Project Structure

### Configuration
| File | Description |
| :--- | :--- |
| `config.R` | **Central city configuration** — change the target city here |

### Data Generation Pipeline (Masselot et al.)

These numbered scripts generate the underlying epidemiological and climate data. They originate from the replication code for [Masselot et al. (2025)](https://doi.org/10.1038/s41591-024-03452-2) and are kept for reference. Data can also be obtained from the [Zenodo repository](https://doi.org/10.5281/zenodo.14004322).

| Script | Description |
| :--- | :--- |
| `01_pkg_params.R` | Load R libraries and define analysis parameters |
| `02_prep_data.R` | Load and prepare city, demographic, and warming-level data |
| `03_attribution.R` | Main health-impact projection loop across cities and scenarios |
| `04_city_le_comparison.R` | Summarise life-expectancy trends |
| `04_tables.R` | Produce tables from the original article |
| `05_plots.R` | Main-text figures from the original article |
| `05_romania_city_lifetables.R` | Romanian city-level life tables |
| `06_plot_claims.R` | Insurance-risk dashboard plots |
| `06_plot_supp.R` | Extended Data figures |
| `06_vis_lifetables.R` | Life-table visualisations |
| `07_plot_scenarios.R` | Scenario comparison dashboards |
| `08_map_romania.R` | Romania map visualisations |

### Working-Paper Pipeline (main.tex)

These scripts produce the figures, tables, and captions referenced in `main.tex`. Each sources `config.R` for the target city.

| Script | Output for main.tex |
| :--- | :--- |
| `bucharest_lifetable_pclm.R` | Table 1 — baseline life table (`<city>_lifetable_2023.tex`) |
| `plot_rr_curves.R` | Figure 1 — RR curves by age group |
| `plot_rr_poster.R` | Figure 2 — interpolated RR surface |
| `plot_bucharest_temp_dist_rr.R` | Figure 3 — temperature distribution + RR overlay |
| `plot_bucharest_temp_evolution.R` | Figure 4 — projected temperature evolution |
| `compute_mortality_multiplier_by_age.R` | Figure 5 — age-specific mortality multiplier |
| `compute_cohort_lifetable_financial.R` | Table 5 — financial impact (EPV changes) |

### Supporting Files

| File/Folder | Description |
| :--- | :--- |
| `functions/` | Shared R functions (`impact.R`, `isimip3.R`, `lifetable.R`) |
| `data/extract_eurostat_projections.R` | Extract Eurostat EUROPOP2019 projections |
| `data/2023_temp/download_era5_2023.py` | ERA5 daily mean temperature download script |
| `data/coefs.csv` | RR coefficients for 854 European cities (from Masselot et al.) |
| `data/tmeanproj.gz.parquet` | Projected daily temperatures by city, SSP, and GCM |
| `data/` | Input data (downloaded from Zenodo; gitignored) |
| `results_csv/` | Output CSV tables (gitignored) |
| `img/` | Figures for LaTeX (gitignored) |
| `fonts/` | Montserrat font files for IE-styled plots |

---

## Prerequisites

### R packages

```r
install.packages(c(
  "terra", "data.table", "arrow", "dplyr", "tidyr", "readr",
  "dlnm", "splines", "ggplot2", "viridis", "patchwork",
  "png", "grid", "showtext", "sysfonts", "showtextdb",
  "ungroup", "readxl"
))
```

### Python (for ERA5 downloads only)

```bash
pip install cdsapi
```

A valid CDS API key is required — see [CDS registration](https://cds.climate.copernicus.eu/).

### Data

The input data necessary to run the analysis are stored on a dedicated [Zenodo repository](https://doi.org/10.5281/zenodo.14004322). Download the `data.zip` archive and extract the files into a `data` folder in the project directory. This can be performed directly from R (see `01_pkg_params.R`).
