# Data

This folder contains all input data used for the health impact projections. The script `00_download_data.R` was used to download and gather all the data, and can be consulted for the source of each data file. The only exception is the temperature projections dataset (`tmeanproj.gz.parquet`) which could not be extracted directly from R.

## Files

| Name | Description |
| :--- | :--- |
| `city_results.csv` | Results of the historical health impact assessment (Masselot et al. 2023 *The Lancet Plan. Health*, https://doi.org/10.1016/S2542-5196(23)00023-2). It contains city metadata that used for plotting and calibration of the demographic projections |
| `coefs.csv` | City and age group specific spline coefficients used to reconstruct the full exposure-response functions |
| `coef_simu.gz.parquet` | City and age group-specific simulations of the spline coefficients. Used for uncertainty assessment |
| `era5series.gz` | Historical series of temperature for each city. Used to calibrate temperature projection series |
| `meta-model.RData` | `mixmeta` model from R that was used to derive the exposure-response functions (see Masselot et al. 2023 *The Lancet Plan. Health*, https://doi.org/10.1016/S2542-5196(23)00023-2) |
| `tmeanproj.gz.parquet` | Daily temperature simulation for the period 1990 to 2099 for the 854 cities and three SSP scenarios from 21 general circulation models (GCM) |
| `warming_years.csv` | Years various global warming levels are reached according for each GCM and SSP |
| `wittgenstein_assr.csv` | Country and age-group specific projections of survival ratio. Later transformed into baseline death rates |
| `wittgenstein_pop.csv` | Country and age-group specific projections of population |
