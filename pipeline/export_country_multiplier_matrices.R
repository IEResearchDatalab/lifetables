################################################################################
#
# Export Country-Level Mortality Multiplier Matrices
#
# Generates one CSV per country × RCP scenario where:
#   - rows  = single-year ages (20–100)
#   - columns = years (2025–2099)
#   - values  = total mortality multiplier (multiplier_total)
#
# Output folder: results_csv/country_multiplier_matrices/
# File naming:   {country_name}_{rcp_label}.csv  e.g. austria_rcp70.csv
#
# Input: results_csv/mortality_multiplier_country_rcp_year_age.csv
#        (produced by pipeline/compute_country_multipliers.R)
#
################################################################################

library(data.table)

# ── Country code → full name mapping ─────────────────────────────────────────

country_names <- c(
  AT = "austria",
  BE = "belgium",
  BG = "bulgaria",
  CH = "switzerland",
  CY = "cyprus",
  CZ = "czechia",
  DE = "germany",
  DK = "denmark",
  EE = "estonia",
  EL = "greece",
  ES = "spain",
  FI = "finland",
  FR = "france",
  HR = "croatia",
  HU = "hungary",
  IE = "ireland",
  IT = "italy",
  LT = "lithuania",
  LU = "luxembourg",
  LV = "latvia",
  MT = "malta",
  NL = "netherlands",
  NO = "norway",
  PL = "poland",
  PT = "portugal",
  RO = "romania",
  SE = "sweden",
  SI = "slovenia",
  SK = "slovakia",
  UK = "united_kingdom"
)

# ── RCP scenario label → file suffix mapping ──────────────────────────────────

rcp_suffixes <- c(
  "RCP 2.6" = "rcp26",
  "RCP 4.5" = "rcp45",
  "RCP 7.0" = "rcp70"
)

# ── Load data ─────────────────────────────────────────────────────────────────

cat("Loading mortality_multiplier_country_rcp_year_age.csv ...\n")
dt <- fread("results_csv/mortality_multiplier_country_rcp_year_age.csv")
cat(sprintf("  %s rows, columns: %s\n",
            format(nrow(dt), big.mark = ","),
            paste(names(dt), collapse = ", ")))

# ── Output directory ──────────────────────────────────────────────────────────

out_dir <- "results_csv/country_multiplier_matrices"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
  cat(sprintf("Created directory: %s\n", out_dir))
}

# ── Generate one file per country × RCP ──────────────────────────────────────

countries   <- sort(unique(dt$country))
rcp_labels  <- unique(dt$rcp_scenario)
n_files     <- 0L

for (ccode in countries) {
  cname <- country_names[ccode]
  if (is.na(cname)) {
    warning(sprintf("No name mapping for country code '%s' — skipping", ccode))
    next
  }

  for (rcp in rcp_labels) {
    rsuffix <- rcp_suffixes[rcp]
    if (is.na(rsuffix)) {
      warning(sprintf("No suffix mapping for RCP '%s' — skipping", rcp))
      next
    }

    # Subset: one country × one RCP, total multiplier only, years from 2025
    sub <- dt[country == ccode & rcp_scenario == rcp & year >= 2025L,
              .(age, year, multiplier_total)]

    if (nrow(sub) == 0L) {
      warning(sprintf("No data for %s / %s — skipping", ccode, rcp))
      next
    }

    # Pivot: ages as rows, years as columns
    wide <- dcast(sub, age ~ year, value.var = "multiplier_total")
    setorder(wide, age)

    # Write CSV
    fname <- file.path(out_dir, sprintf("%s_%s.csv", cname, rsuffix))
    fwrite(wide, fname)
    n_files <- n_files + 1L
  }
}

cat(sprintf("\nDone. Wrote %d CSV files to %s/\n", n_files, out_dir))
