# Climate-Adjusted Life Tables for European Cities

This project incorporates temperature-dependent mortality into projected life tables at the city level, following the methodology described in the accompanying working paper (`main.tex`).

## Changing the Target City

All city-specific parameters are centralised in **`config.R`**. To generate tables, figures, and captions for a different city, edit the three identifiers at the top of that file:

```r
city_code  <- "RO001C"       # URAU code (Urban Audit)
city_name  <- "Bucharest"    # Human-readable name
nuts3_code <- "RO321"        # NUTS 3 code (Eurostat projections)
```

Then re-run the pipeline scripts listed below. All output paths are derived automatically from `city_name`.

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
| `data/` | Input data (downloaded from Zenodo; gitignored) |
| `results_csv/`, `results_parquet/` | Output data (gitignored) |
| `img/` | Figures for LaTeX (gitignored) |
| `fonts/` | Montserrat font files for IE-styled plots |

## Data

The input data necessary to run the analysis are stored on a dedicated [Zenodo repository](https://doi.org/10.5281/zenodo.14004322). Download the `data.zip` archive and extract the files into a `data` folder in the project directory. This can be performed directly from R (see `01_pkg_params.R`).
