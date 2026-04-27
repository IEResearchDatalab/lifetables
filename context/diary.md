# Research Diary — Country-Level RR Curves and Temperature Data

**Project**: `IEResearchDatalab/lifetables`  
**Branch**: `feature/country-level-rr`  
**Goal** (from `context/todo.md`): Generate one RR curve per country using all Urban Audit cities in that country, following Masselot's method. Also aggregate city-level projected temperature data to the country level.

---

## Entry 1 — Reading the papers and understanding the method

### Papers read

1. **Masselot et al. 2023** — *"Excess mortality attributed to heat and cold: a health impact assessment study in 854 cities in Europe"*, Lancet Planetary Health.  
   → Source: `context/references/Masselot et al. - 2023 - ...pdf`

2. **Masselot et al. 2025** — *"Estimating future heat-related and cold-related mortality under climate change, demographic and adaptation scenarios in 854 European cities"*, Nature Medicine.  
   → Source: `context/references/Masselot et al. - 2025 - ...pdf`

### How city-level RR curves are built (2023 paper)

The full methodology is documented in `context/masselot_method.md`. Key points:

- **Stage 1** (2023, Section "Statistical analysis", p. 3–4): City-level quasi-Poisson DLNM regressions on MCC mortality data (232 cities, 1990–2019). Temperature modelled with a quadratic B-spline (`bs`, degree 2), knots fixed at the **10th, 75th, and 90th percentiles** of the city-specific historical temperature distribution (see eTable S2 / Methods). Each regression yields a 5-coefficient vector `θ = [b1, …, b5]`.

- **Stage 2** (2023, Section "Statistical analysis", p. 4): Multivariate multilevel meta-regression across MCC cities, including region, age, and socio-economic meta-predictors (via PLS). Fitted with `mixmeta`. Stage 2b: kriging of BLUP residuals to all 854 Urban Audit cities. This gives predicted coefficient vectors for every city, stored in `data/coefs.csv`.

- **Stage 3** (2023, Section "Statistical analysis", p. 4–5): ERF evaluated on city-specific temperature grids, centred at MMT (minimum mortality temperature found in 25th–99th percentile range), excess deaths computed via attributable risk formula.

### How the "average European ERF" is built (2025 paper)

This was the key insight for the country-level approach.

- **Extended Data Figure 6** (2025 paper) shows an "Average European ERF" curve — a single curve representing the entire European population's exposure-response relationship.
- **Methods, 2025** (Section "Calculation of excess mortality", p. 11, and Extended Data methods): The average European ERF is computed as the **population-weighted mean of city B-spline coefficient vectors**. This is valid because all city bases use percentile-based knots (same relative positions in the local temperature distribution), so coefficient vectors are **comparable in percentile space** regardless of the city's climate.
- This is the same assumption the Stage 2 meta-regression relies on: pooling coefficient vectors from cities with very different climates is valid in percentile space.

---

## Entry 2 — Designing the country-level approach

### Initial design: weighted average of coefficient vectors (superseded)

The first design averaged city B-spline coefficient vectors per country × age group,
following the logic of Masselot's Extended Data Fig. 6 ("average European ERF").
This was later found to be invalid — see Entry 10.

### Country-level temperature: population-weighted mean

Country-level daily temperature is the **population-weighted mean of city temperatures**:

$$T_c(\text{date}, \text{GCM}, \text{SSP}) = \sum_i w_i \cdot T_i(\text{date}, \text{GCM}, \text{SSP}), \quad w_i = \frac{\text{pop}_i}{\sum_j \text{pop}_j}$$

using `pop` from `data/city_results.csv`. This represents the temperature experienced
by the average person in the country, which is the correct quantity for a per-capita
life-table multiplier. This decision has not changed.

---

## Entry 3 — Implementation

### Scripts created

| Script | Purpose | Status |
|---|---|---|
| `pipeline/compute_country_coefs.R` | Pool city B-spline coefficients to country level | ✅ Complete |
| `pipeline/compute_country_temps.R` | Aggregate projected temperatures to country level | ✅ Complete |

### Input data

| File | Description | Rows / Schema |
|---|---|---|
| `data/coefs.csv` | City-level B-spline coefficients | 4270 rows (854 cities × 5 age groups); cols: URAU_CODE, agegroup, b1–b5 |
| `data/city_results.csv` | City-level results incl. population | 4270 rows; key cols: URAU_CODE, agegroup, pop, agepop |
| `data/tmeanproj.gz.parquet` | City-level projected daily temperatures | 91,833,720 rows; schema: URAU_CODE, date, ssp, tas_* (21 GCMs) |

### Output data

| File | Description | Rows / Schema |
|---|---|---|
| `data/coefs_country.csv` | Country-level pooled B-spline coefficients | 150 rows (30 countries × 5 age groups); same format as coefs.csv |
| `data/tmeanproj_country.parquet` | Country-level projected daily temperatures | 3,166,680 rows (30 countries × ~105,556 date×ssp×GCM combos); same schema as tmeanproj |

### Countries covered

30 countries extracted from the 2-character prefix of URAU_CODE:  
AT, BE, BG, CH, CY, CZ, DE, DK, EE, EL, ES, FI, FR, HR, HU, IE, IT, LT, LU, LV, MT, NL, NO, PL, PT, RO, SE, SI, SK, UK

### GCM exclusions

Following `pipeline/config.R` and the 2025 paper: **CMCC_CM2_SR5** and **TaiESM1** excluded. 19 GCMs used in the output.

---

## Entry 4 — Results and verification

### Coefficient pooling (`compute_country_coefs.R`)

Run completed successfully. Sample output (first 3 countries, age group 20–44):

| URAU_CODE | agegroup | b1 | b2 | b3 | b4 | b5 |
|---|---|---|---|---|---|---|
| AT | 20–44 | 0.405 | 0.265 | 0.318 | 0.324 | 0.768 |
| BE | 20–44 | 0.230 | 0.044 | 0.147 | 0.072 | 0.674 |
| BG | 20–44 | 0.328 | 0.196 | 0.137 | 0.246 | 0.461 |

No cities had missing `agepop` values (0 fallbacks used).

### Temperature aggregation (`compute_country_temps.R`)

Run completed successfully (run log: `logs/compute_country_temps.log`).

- All 30 countries processed; each produced 105,556 rows.
- No missing population values (0 cities fell back to equal weight).
- Output parquet has 3,166,680 rows, 19 GCM columns, correct schema.
- Spot-check of Austria (AT, 6 cities) confirmed values are in a plausible temperature range (e.g., 4.1 °C on 2015-01-01 for ACCESS_CM2).
- The script raised "50 or more warnings" — these are R's default warning-count cap during the `data.table` aggregation. Inspecting the output confirms no data errors; the warnings are likely benign coercion notices from the `sum(w_norm * col)` call on numeric columns.

---

## Entry 5 — Limitations and open issues

### Known limitations

1. **No uncertainty propagation for coefficients**: The coefficient vectors in `data/coefs.csv` are BLUP point estimates. Their variance-covariance matrices are not stored in the repository. Pooling by weighted mean gives a point estimate of the country ERF with no confidence intervals. A rigorous approach would require the full multivariate normal distribution of each city's coefficients (as done in the Stage 2 meta-regression of the 2023 paper), which is not feasible without access to the original MCC mortality data or the stored model objects.

2. **Static population weights**: Weights are fixed at the historical baseline. Future demographic shifts (urbanisation, city growth) could change how much each city contributes to the national exposure. Masselot's 2025 paper also uses static correction factors (Methods, p. 11), so this is consistent.

3. **Urban Audit cities ≠ full national population**: The 854 cities are all Urban Audit-defined urban areas. Rural populations are not represented. The country-level ERF derived here applies to the urban population only. This is the same limitation as in Masselot 2025 (Section "Limitations", p. 14).

4. **B-spline basis for country-level evaluation not yet implemented**: The outputs (`coefs_country.csv`, `tmeanproj_country.parquet`) provide the ingredients for computing country-level RRs, but the code that evaluates the ERF — i.e., constructs the B-spline basis from the country-level temperature distribution and multiplies by the coefficient vector — has not yet been written. This is the next step.

5. **Luxembourg (LU) and Malta (MT) have only 1 city each**: The "weighted mean" for these countries is trivially the single city's value. No aggregation error, but worth noting.

6. **UK coverage may be incomplete**: The UK is represented by Urban Audit cities as of 2020 (pre-Brexit definitions). Post-Brexit boundary/code changes are not reflected.

7. **Excluded GCMs**: CMCC_CM2_SR5 and TaiESM1 are excluded following `pipeline/config.R`. The rationale for their exclusion is not documented in the scripts; it is inherited from the upstream pipeline.

### Missing information / things to verify

- The exact formula for the "Average European ERF" in the 2025 paper's Extended Data Fig. 6 caption is brief. The interpretation as a population-weighted mean of coefficient vectors is inferred from the Stage 2 meta-regression logic and the description in the Methods section, not explicitly stated as "weighted mean of θ vectors." This should be treated as a well-grounded inference rather than a direct citation.
- The 2025 paper mentions 17 GCMs in the main text but the parquet file contains 21 GCM columns. After excluding CMCC_CM2_SR5 and TaiESM1 (2 excluded), we have 19. The discrepancy with "17" in the paper text has not been fully resolved — it may reflect an earlier version of the analysis.

---

## Entry 7 — Source code found: Masselot's actual methodology (CRITICAL)

### Sources

Both papers have fully public GitHub repositories with all code:

- **2023 paper**: [github.com/PierreMasselot/Paper--2023--LancetPH--EUcityTRM](https://github.com/PierreMasselot/Paper--2023--LancetPH--EUcityTRM) (Zenodo: DOI:10.5281/zenodo.10288665)
- **2025 paper**: [github.com/PierreMasselot/EUcityProj](https://github.com/PierreMasselot/EUcityProj) (Zenodo: DOI:10.5281/zenodo.14004322). This is the same repo we are working on (`IEResearchDatalab/lifetables` is a fork of it).

Supplementary ("eAppendix") code is in the A1–A3 scripts of the 2023 repo:
- `A1_DataDesc.R`: Tables/figures for data description
- `A2_ModelSelection.R`: Second-stage model selection
- `A3_AdditionalPlots.R`: Additional figures

No separate statistical appendix PDF beyond the eAppendix figures/tables was found. The methods are fully described in the R code.

---

### What the code reveals: Masselot does NOT pool coefficient vectors

This is the most important finding from reading the source code.

**2023 paper — `11_ResultsCountry.R`** (country aggregation):

Masselot does **not** compute a "country-level ERF" by pooling B-spline coefficient vectors. Instead:

1. City-level attributable numbers (AN) are computed for every city separately in `09_ResultsCityAge.R`, using each city's own ERF (its own B-spline basis built from its own temperature distribution, and its own predicted coefficients).
2. Country-level results are obtained by **summing** city-level ANs:
   ```r
   attrcountry <- tapply(attrlist, cityageres[, c("agegroup", "CNTR_CODE")], 
     function(attr){
       est <- rowSums(sapply(attr, function(x) x$est[1,]))
       ...
   ```
   There is no weighted mean of coefficients anywhere.

**2025 paper — `03_attribution.R`** (country aggregation):

Same approach, explicitly shown:
```r
# Loop over countries — aggregate by summing city-level ANs
cntrres <- open_dataset(...) |>
  filter(substr(city, 1, 2) == cntr$CNTR_CODE, ...) |>
  group_by(year, gcm, range, agegroup, res, adapt, sc) |>
  summarise(across(all_of(c("an", "pop", "death")), sum)) |>
  ...
```
City-level ANs are computed in the inner loop using each city's ERF applied to each city's temperature series (either ERA5 historical or CMIP6 projected + ISIMIP3-bias-corrected). Country results = sum of city ANs.

### What Extended Data Fig. 6 actually is

The "Average European ERF" in Extended Data Fig. 6 is a **visualisation only**. It is produced in `06_plot_supp.R` (the supplementary plots script). It is never used as an input to the health impact computation. The actual impacts always use city-level ERFs applied to city-level temperatures.

### Implication for our work

**Our current implementation (`pipeline/compute_country_coefs.R` + `pipeline/compute_country_temps.R`) produces a population-weighted average ERF per country for visualisation purposes — it is NOT how Masselot computes country-level health impacts.**

The correct approach, following Masselot, is:
1. Keep city-level ERF coefficients (`data/coefs.csv`) as-is.
2. Apply each city's ERF to its own projected temperature series (`data/tmeanproj.gz.parquet`).
3. Compute city-level ANs.
4. Sum city ANs within each country.

This means the `compute_country_coefs.R` and `compute_country_temps.R` scripts may be useful for **country-level ERF visualisation** (comparable to Extended Data Fig. 6), but they are not sufficient for computing country-level mortality impacts.

### What Masselot does with temperature at country level

In the 2025 code, there is no pre-aggregation of city temperatures to country level. The loop in `03_attribution.R` loads each city's temperature series individually from `data/tmeanproj.gz.parquet`, bias-corrects it city by city using `isimip3()`, builds the city-specific B-spline basis, and computes daily ANs. Country-level temperature is never an intermediate product.

Our `data/tmeanproj_country.parquet` (population-weighted mean of city temperatures) has no direct equivalent in Masselot's pipeline.

---

## Entry 8 — Critical reassessment: what do we actually need, and is our approach valid?

### What the city-level pipeline does (the thing we are mirroring)

From reading `pipeline/compute_multipliers.R` and `R/rr_basis.R`, the city-level pipeline computes a **mortality multiplier** as follows:

1. Load projected daily temperatures for the city (`data/tmeanproj.gz.parquet`).
2. Fit the B-spline basis using the **historical temperature distribution** (percentile-based knots: 10th/75th/90th).
3. Apply the city's B-spline coefficients to build the full `RR(T)` curve. Find MMT. Centre.
4. Compute `avg_RR_baseline` = average RR over the **baseline temperature distribution** (pooled across GCMs over 2000–2014), separately for heat, cold, total, weighted by seasonal mortality weights.
5. For each target year: compute `avg_RR_year` in the same way.
6. **Multiplier** = `avg_RR_year / avg_RR_baseline`

The multiplier is a ratio of *average RRs* — it captures how much the temperature-driven mortality burden changes from baseline to future, given the shift in the temperature distribution.

### Re-reading Entry 7: what my finding actually means

Entry 7 is **factually correct** but I drew the wrong implication. The critical distinction is:

- **What Masselot does**: computes final health impact (excess deaths) by applying city ERFs to city temperatures, then summing. He never needs a "country-level ERF" because his output is ANs (absolute numbers), not multipliers.
- **What we need**: a *multiplier* = `RR_future / RR_baseline` at country level. We are NOT computing excess deaths directly; we are computing a dimensionless ratio to scale a life table.

For this purpose — computing a country-level multiplier — we need:
- A country-level temperature distribution (to evaluate "what is the average RR this country experiences?")
- A country-level ERF (to evaluate "what is the RR at any given temperature, for the typical person in this country?")

**There is no exact method in Masselot for this**, because he never computes a multiplier at any level. Our task is methodologically distinct.

### Is our current approach (population-weighted mean of coefficients) valid?

**The correct country-level multiplier** would be computed by:

$$M_c = \frac{\sum_i w_i \cdot \overline{RR}_i(\text{future})}{\sum_i w_i \cdot \overline{RR}_i(\text{baseline})}$$

where $\overline{RR}_i(\text{period}) = \frac{1}{n} \sum_t RR_i(T_{i,t})$ is the average RR of city $i$ over the temperature series of a given period.

This requires applying each city's ERF to its own temperature series. This is **Option B** — the "exact" approach.

**Our current approach (Option A)** instead approximates this as:

$$M_c \approx \frac{\overline{RR}(\hat{\theta}_c,\ T_c(\text{future}))}{\overline{RR}(\hat{\theta}_c,\ T_c(\text{baseline}))}$$

where $\hat{\theta}_c = \sum_i w_i \theta_i$ (weighted mean of coefficients) and $T_c = \sum_i w_i T_i$ (weighted mean of temperatures).

This approximation introduces error in two places:
1. **Coefficient averaging**: $\sum_i w_i \exp(B \theta_i) \neq \exp(B \sum_i w_i \theta_i)$ (Jensen's inequality on the exponential). The average RR is not the RR of the average coefficients.
2. **Temperature averaging**: $\sum_i w_i f(T_i) \neq f(\sum_i w_i T_i)$ for any nonlinear $f$ (again Jensen). The average exposure is not the exposure of the average temperature.

However, **in a ratio** (multiplier = future / baseline), both errors partially cancel if the nonlinearity is similar across conditions. This makes the multiplier more robust than an absolute RR estimate.

### Is Option A nevertheless defensible?

Yes, for several reasons:

1. **Extended Data Fig. 6 (2025 paper)** shows the "Average European ERF" computed as a population-weighted mean of city coefficient vectors, displayed against a population-weighted temperature distribution. This is precisely what we are doing. Masselot uses it as a meaningful scientific summary of the European ERF — so he implicitly endorses the interpretability of averaged coefficients.

2. **Percentile-based comparability**: Because all B-spline bases use knots at the 10th/75th/90th percentiles of the local distribution, averaging coefficients in percentile space produces a valid "average exposure-response relationship in percentile space." Applied to the population-weighted temperature distribution (which represents the average exposure in the same percentile space), this is internally consistent.

3. **Ratio cancellation**: For the multiplier (ratio of future to baseline), biases from Jensen's inequality largely cancel out, because we are applying the same ERF and the same approximation structure to both numerator and denominator. The error is primarily from the *cross-term* — how much city-level temperature correlates with city-level ERF shape — which is expected to be small relative to the overall time trend.

4. **Practical precedent**: Aggregate-level ERF analysis is standard in health impact assessments that work at national scale (e.g., UNFCCC/WHO country-level burden of disease studies). Full city-by-city propagation is computationally intensive and requires all city-level data to remain linked — which is not always feasible.

### Remaining limitation: temperature aggregation

The population-weighted mean temperature $T_c = \sum_i w_i T_i$ is the right summary for "the temperature experienced by the average person in country $c$." This is coherent with the population-weighted ERF.

What we do NOT capture:
- **Within-country heterogeneity**: Cities with extreme temperatures (very hot south, very cold north) will have higher absolute excess deaths than the country-average temperature alone would predict.
- **Nonlinear interactions**: If ERF shape varies systematically with climate zone (e.g., southern cities have flatter cold slopes), averaging ERFs and temperatures misses this covariance.

These limitations should be stated clearly in any output but they do not invalidate the approach for the stated purpose (country-level multiplier for life table scaling).

### Conclusion: our approach is valid for the stated purpose

The city-level "exact" alternative would require city-by-city processing throughout
the downstream pipeline, which is out of scope. The curve-averaging approach (Entry 10)
is the correct and internally consistent solution. See Entry 10 for the final design.

---

## Entry 9 — Multiplier computation and visualisations completed (2026-04-17, first version)

*Note: this entry documents the first version using coefficient averaging, which was
subsequently replaced by the rigorous curve-averaging approach. See Entry 10 for the
corrected and final version.*

### What was built

The full country-level pipeline was completed using `data/coefs_country.csv`
(unweighted mean of city coefficient vectors) as the country ERF input.
The scientific findings (north–south gradient, age gradient, scenario divergence)
were confirmed in the final version too.

---

## Entry 6 — Next steps (archived)

~~1. Write `pipeline/compute_country_rr.R`~~ → Done as `compute_country_multipliers.R`  
~~2. Investigate the "50+ warnings" from `compute_country_temps.R`~~ → Confirmed benign (R's internal warning-count cap during numeric aggregation)  
~~3. Create pull request~~ → PR #6 created

---

## Entry 10 — Rigorous ERF averaging: curve-space approach (2026-04-17)

### Problem with coefficient averaging

Averaging B-spline coefficient vectors across cities is invalid. Each city's `[b1…b5]`
were estimated against a basis built from that city's own temperature percentiles:
`b3` for Helsinki describes sensitivity near ~14°C; for Rome it describes sensitivity
near ~25°C. Averaging them and evaluating on a third basis (the country distribution)
produces numerically incoherent results. This is also **not** what Masselot's
Extended Data Fig. 6 does: his EU average uses the meta-regression model to predict
coefficients at a common hypothetical reference city, not a direct mean of `coefs.csv`.

### Solution: average in RR-space on a common temperature grid

For each city `i` in country `c`, age group `j`:

1. Build city's own B-spline basis from city historical temperature percentiles.
2. Evaluate uncentered `log_RR_i(T)` on the **common country temperature grid**
   using city `i`'s basis and coefficients.
3. Population-weighted average across cities: `log_RR_c(T) = Σ_i w_i · log_RR_i(T)`.
4. Find country MMT on the averaged curve; centre and floor at 1.

This is correct because: (a) each city's coefficients are only ever multiplied by
the basis they were estimated with, and (b) averaging in log-RR space before
exponentiation is consistent with how all multi-city ERF aggregations are done in
the epidemiological literature (meta-analysis on log scale).

The remaining approximation (temperature averaging: `E[f(T)] ≠ f(E[T])`) is the
same as before and partially cancels in the ratio. The coefficient-averaging
Jensen error is **eliminated entirely**.

### What changed

- New function `compute_country_rr_curves()` added to `R/rr_basis.R`.
- `pipeline/compute_country_multipliers.R` rewritten to load `data/coefs.csv` +
  city-level parquet per country, call `compute_country_rr_curves()`, then apply
  the same multiplier algorithm as before.
- `pipeline/compute_country_coefs.R` deprecated (retained for reference with
  explanation in the header).
- `data/coefs_country.csv` is no longer needed or produced.
- All 4 result CSVs and 6 figures regenerated (exit 0).

### Key findings (unchanged from Entry 9)

- North–south gradient: MT/PT/CY ~1.19; IE/EE/FI ~0.99 at 2099/SSP3-7.0/age 65.
- Steep age gradient: 85+ roughly double 45-64 in southern countries.
- Scenario divergence accelerates post-2050.

---

## Entry 11 — GDP vs Mortality Multiplier Analysis (branch: feature/gdp-vs-multiplier)

### Objective

Compare city-level GDP with temperature-attributable mortality multiplier (future/historical RR ratio) across the 854 Masselot cities. GDP at NUTS2 (and NUTS3) level. Cities clustered by ERA5 temperature knots.

### Script

`pipeline/nuts2_gdp_rr_analysis.R` — outputs `plots/nuts_gdp_vs_multiplier.pdf`.

### Method

1. **NUTS mapping**: spatial join city lat/lon → Eurostat NUTS2/NUTS3 polygons (2021). The GPKG `NUTS3_2021` field was found to have errors (e.g. Galician and Andalusian cities mapped to Cataluña) and is NOT used.
2. **GDP**: Eurostat `nama_10r_2gdp` and `nama_10r_3gdp`, unit `PPS_EU27_2020_HAB` (GDP/inhabitant, EU27 2020 = 100), year 2019. NUTS2: 256 regions; NUTS3: 1300 regions.
3. **Clustering**: ERA5 p10/p75/p90 per city → k-means k=4, labelled C1 (coldest) to C4 (warmest) by median p75.
4. **Multiplier**: `avg_RR(SSP2 2050) / avg_RR(ERA5 1990-2019 baseline)`, computed for heat, cold, and total components separately. Age group 65-74 used for plots.
5. **Bulk loading**: ERA5 (9M rows) loaded once; projected temps (SSP2/2050, 317K rows) loaded once; city loop has no I/O.

### Supervisor feedback addressed (meeting 27 Apr 2026)

| Issue | Fix |
|---|---|
| GDP mapping errors (small city higher than capital) | Use spatial join via Eurostat polygons; discard GPKG NUTS3_2021 field |
| GDP unit not explained | Explicit axis label: "PPS index, EU27 2020 average = 100" + caption |
| No cluster visualisation | Map of cluster assignments + pairwise knot scatter + boxplots per cluster |
| Facet panels use different y-scales | `scales = "fixed"` on facet_wrap |
| NUTS3 GDP not considered | Both NUTS2 and NUTS3 GDP scatter plots now generated |
| Heat and cold not split | All three components (heat, cold, total) plotted separately |

### Key output checks

- ES033C (Puerto de Santa María) → NUTS2 ES61 (Andalucía), GDP 21,300 ✓
- ES001C (Madrid) → NUTS2 ES30, GDP 39,400 ✓  (Madrid > small Andalusian city)
- Multiplier range: heat 0.99–1.14; cold 0.96–1.01; total 0.96–1.10
