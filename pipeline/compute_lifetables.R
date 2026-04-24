################################################################################
#
# Compute Cohort Life Tables and Financial Impact
#
# Refactored orchestration script that sources shared function modules
# instead of defining everything inline.
#
# Depends on: R/utils.R, R/load_data.R, R/load_coefficients.R, R/rr_basis.R,
#             R/epv.R, R/cohort_lifetable.R
# Expects: pipeline/config.R already sourced (city_name, city_code, etc.)
# Expects: pipeline/compute_multipliers.R already run (multiplier data on disk)
#
################################################################################

library(data.table)
library(arrow)
library(dplyr)
library(dlnm)
library(splines)

cat_header(sprintf("COHORT LIFE TABLE: %s (%s)", city_name, city_code))

# --------------------------------------------------------------------------
# The heavy lifting (temperature loading, RR construction, multiplier
# computation) is done by compute_multipliers.R.  This script:
#   1. Loads the saved multipliers
#   2. Loads Eurostat mortality projections
#   3. Builds cohort life tables for each scenario
#   4. Computes EPVs (deferred annuity + whole-life insurance)
#   5. Saves results
# --------------------------------------------------------------------------

# --- Step 1: Load multipliers (already computed) ---
cat_step(1, "Loading precomputed mortality multipliers")

multipliers <- fread(sprintf("results_csv/mortality_multipliers_cohort_%s.csv", city_name_lower))
setkey(multipliers, ssp, adaptation, year, age)
cat(sprintf("  Loaded %d multiplier records\n", nrow(multipliers)))

# --- Step 2: Load Eurostat projected mortality ---
cat_step(2, "Loading Eurostat projected mortality data")

mort_proj <- fread(sprintf("results_csv/mortality_projections_%s.csv", city_name_lower))
mort_proj <- mort_proj[age >= 20]
setkey(mort_proj, year, age)
cat(sprintf("  Loaded: %d records, ages %d-%d, years %d-%d\n",
            nrow(mort_proj),
            min(mort_proj$age), max(mort_proj$age),
            min(mort_proj$year), max(mort_proj$year)))

# --- Step 3: Build cohort life tables ---
cat_step(3, "Building cohort life tables for all scenarios")

lifetables <- list()
for (ssp_val in ssp_codes) {
	for (adapt_lab in adaptation_labels) {
		key <- paste(ssp_val, adapt_lab, sep = "_")
		lifetables[[key]] <- build_cohort_lifetable(
			mort_proj, multipliers, ssp_val, adapt_lab,
			rcp_labels, cohort_start_age, cohort_years, radix
		)
	}
}
cat(sprintf("  Built %d cohort life tables\n", length(lifetables)))

# --- Step 4: Compute actuarial EPVs ---
cat_step(4, "Computing actuarial EPVs")

epv_results <- list()

for (key in names(lifetables)) {
	lt <- lifetables[[key]]
	rcp_lab   <- lt$rcp[1]
	adapt_lab <- lt$adaptation[1]

	# Baseline EPVs
	annuity_base   <- compute_deferred_annuity_epv(lt, "qx_base", interest_rate)
	insurance_base <- compute_insurance_epv(lt, "qx_base", interest_rate)

	# Climate-adjusted EPVs
	annuity_clim   <- compute_deferred_annuity_epv(lt, "qx_clim", interest_rate)
	insurance_clim <- compute_insurance_epv(lt, "qx_clim", interest_rate)

	epv_results[[key]] <- data.table(
		rcp        = rcp_lab,
		adaptation = adapt_lab,
		# Annuity
		annuity_base           = annuity_base,
		annuity_clim           = annuity_clim,
		delta_annuity          = annuity_clim - annuity_base,
		pct_delta_annuity      = pct_delta(annuity_base, annuity_clim),
		# Insurance
		insurance_base         = insurance_base,
		insurance_clim         = insurance_clim,
		delta_insurance        = insurance_clim - insurance_base,
		pct_delta_insurance    = pct_delta(insurance_base, insurance_clim),
		# Reserves (identical to EPVs for single-premium contracts)
		reserve_annuity_base   = annuity_base,
		reserve_annuity_clim   = annuity_clim,
		delta_reserve_annuity  = annuity_clim - annuity_base,
		pct_delta_reserve_annuity = pct_delta(annuity_base, annuity_clim),
		reserve_ins_base       = insurance_base,
		reserve_ins_clim       = insurance_clim,
		delta_reserve_ins      = insurance_clim - insurance_base,
		pct_delta_reserve_ins  = pct_delta(insurance_base, insurance_clim),
		reserve_total_base     = annuity_base + insurance_base,
		reserve_total_clim     = annuity_clim + insurance_clim,
		delta_reserve_total    = (annuity_clim + insurance_clim) - (annuity_base + insurance_base),
		pct_delta_reserve_total = pct_delta(annuity_base + insurance_base,
		                                     annuity_clim + insurance_clim)
	)
}

epv_summary <- rbindlist(epv_results)

cat("\n  EPV Summary:\n")
print(epv_summary[, .(rcp, adaptation,
                       annuity_base     = round(annuity_base, 3),
                       pct_delta_annuity = round(pct_delta_annuity, 3),
                       insurance_base   = round(insurance_base, 4),
                       pct_delta_ins    = round(pct_delta_insurance, 3))])

# --- Step 5: Save outputs ---
cat_step(5, "Saving results")

# Combine all life tables
all_lifetables <- rbindlist(lifetables, idcol = "scenario")
output_lt <- all_lifetables[, .(
	age, year, rcp, adaptation,
	qx_base, qx_clim, mx_base, mx_clim,
	multiplier, lx_base, lx_clim, dx_base, dx_clim
)]

fwrite(output_lt, sprintf("results_csv/cohort_lifetable_climate_%s.csv", city_name_lower))
cat(sprintf("  Saved: results_csv/cohort_lifetable_climate_%s.csv\n", city_name_lower))

fwrite(epv_summary, sprintf("results_csv/financial_impact_summary_%s.csv", city_name_lower))
cat(sprintf("  Saved: results_csv/financial_impact_summary_%s.csv\n", city_name_lower))

# Save multipliers for reference
fwrite(multipliers, sprintf("results_csv/mortality_multipliers_cohort_%s.csv", city_name_lower))
cat(sprintf("  Saved: results_csv/mortality_multipliers_cohort_%s.csv\n", city_name_lower))

cat("\nCohort life table computation complete.\n")
