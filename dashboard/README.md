# Climate–Mortality Pipeline Dashboard

Interactive Shiny dashboard that walks through the climate-adjusted mortality
multiplier pipeline step by step, for **64 cities** (35 Romanian + 30 EU
capitals).

## Quick Start (local)

```r
# From the project root:
shiny::runApp("dashboard")
```

## Data Preparation

The temperature data is filtered from the full 3.1 GB parquet (854 cities)
down to ~233 MB (64 cities). To regenerate:

```r
Rscript dashboard/prep_deploy_data.R
```

This creates `dashboard/data/` with:
- `tmeanproj.gz.parquet` — daily GCM temperatures (233 MB)
- `coefs.csv` — MCC exposure-response coefficients
- `city_lookup.csv` — city names and country codes

## Deploy to shinyapps.io

1. Create an account at <https://www.shinyapps.io> (Professional plan
   recommended for the 233 MB dataset).
2. Configure your credentials:
   ```r
   rsconnect::setAccountInfo(
     name   = "your-account",
     token  = "your-token",
     secret = "your-secret"
   )
   ```
3. Deploy:
   ```r
   source("dashboard/deploy_shinyapps.R")
   ```

## Deploy with Docker (self-hosted)

```bash
cd dashboard
docker build -t climate-dashboard .
docker run -d -p 3838:3838 climate-dashboard
```

Then open <http://localhost:3838>.

## Features

| Step | What it shows |
|------|--------------|
| 1 | Temperature data summary + historical distribution |
| 2 | Exposure–response curves for 5 age groups |
| 3 | Minimum Mortality Temperature (MMT) by age group |
| 4 | Baseline temperature distribution |
| 5 | Target year temperature distributions |
| 6 | Mortality multiplier by single-year age (interactive plot + table) |

### Controls

- **City** — searchable dropdown (35 Romanian cities + 30 EU capitals)
- **Baseline period** — climatological 1990–2019 or a custom single year
- **Scenario** — RCP 2.6 / 4.5 / 7.0
- **RR component** — heat-only, cold-only, or total
- **Target years** — adjustable range slider
