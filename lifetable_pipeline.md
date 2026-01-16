# Pipeline: City-Level Life Table Generation

This document outlines the computational pipeline used to generate and analyze single-age life tables for European cities under climate change scenarios.

## 1. Pipeline Overview

The workflow integrates demographic projections with climate impact assessments to estimate the effect of temperature-related mortality on life expectancy.

### Workflow Diagram
`Data Prep & Parameters` $\rightarrow$ `Attribution Modeling` $\rightarrow$ **`Life Table Construction`** $\rightarrow$ `Visualization`

---

## 2. Detailed Steps

### Step 1: Configuration and Parameters (`01_pkg_params.R`)
*   **Goal**: Define global settings.
*   **Details**: Sets the study period (2010–2100), age groups, and loads necessary R libraries. Defines the "Reference Period" for historical comparison.

### Step 2: Data Preparation (`02_prep_data.R`)
*   **Goal**: Prepare baseline demographic data.
*   **Inputs**:
    *   **Wittgenstein Centre**: National-level population and survival rates (5-year steps).
    *   **Urban Audit**: Historical city-level population structures.
*   **Process**:
    *   Standardizes age groups.
    *   Aligns time periods.
*   **Output**: Cleaned demographic baseline datasets.

### Step 3: Attribution Modeling (`03_attribution.R`)
*   **Goal**: Calculate the number of deaths attributable to climate (heat and cold).
*   **Method**:
    *   Uses **ISIMIP3** temperature projections (bias-corrected).
    *   Applies Exposure-Response Functions (ERFs) derived from historical city data.
    *   Computes **Attributable Numbers (AN)** for future periods under various SSPs (1, 2, 3, 5).
*   **Output**: `results_parquet/city_period.parquet` (Contains $AN$ for Heat, Cold, and Net).

### Step 4: Life Table Construction (`05_romania_city_lifetables.R`)
*   **Goal**: The core integration script. Generates full life tables.
*   **Process**:
    1.  **Downscaling**: Scales national demographic data to the city level using historical ratios ($Pop_{factor}$, $Death_{factor}$).
    2.  **Interpolation**: Expands 5-year age group data (e.g., 20-24) into single years (20, 21, 22, 23, 24) using uniform distribution.
    3.  **Scenario Merging**: Joins the Baseline Demography with the Climate Attribution ($AN$).
    4.  **Mortality Adjustment**:
        *   Calculates Baseline Rates ($m_x^{no\_cc}$) using **Logarithmic Transformation** of Survival Probability (Constant Force of Mortality assumption) for standard intervals.
            $$ m_x^{base} = -\frac{1}{n} \ln\left(1 - \frac{Deaths_{obs}}{Pop_{obs}}\right) $$
            *Note: For the open-ended interval ($85+$) or high-mortality groups where $n \cdot m_x \ge 1$, the linear rate ($D/P$) is used.*
        *   Adds Climate Excess Rates ($AN_{clim} / Pop$) to get Adjusted Rates ($m_x^{cc}$).
            $$ m_x^{cc} = m_x^{base} + \frac{AN_{clim}}{Pop} $$
    5.  **Actuarial Calculation (from Rate to Indicators)**:
        *   **Convert to Probability ($q_x$)**: Transform central rate to probability.
            $$ _nq_x = \frac{n \cdot _nm_x}{1 + (n - _na_x) \cdot _nm_x} $$
        *   **Survivor Function ($l_x$)**: Start with cohort $l_0 = 100,000$. Calculate survivors at next age.
            $$ l_{x+n} = l_x \times (1 - _nq_x) $$
        *   **Deaths ($d_x$)**: Derive the number of deaths in the interval.
            $$ _nd_x = l_x - l_{x+n} $$
        *   **Person-Years ($L_x$)**: Calculate total time lived by the cohort in the interval.
            $$ _nL_x = n \cdot l_{x+n} + _na_x \cdot _nd_x $$
        *   **Total Future Years ($T_x$)**: Sum $L_x$ from age $x$ to the end of life.
            $$ T_x = \sum_{i=x}^{\omega} \ _nL_i $$
        *   **Life Expectancy ($e_x$)**: Compute average future years.
            $$ e_x = \frac{T_x}{l_x} $$
*   **Output**: `results_csv/romania_city_lifetables_full.csv` (The Master Dataset).

### Step 5: Summary and Visualization
*   **Summary (`04_city_le_comparison.R`)**:
    *   Aggregates the complex master dataset into a simple summary table showing Life Expectancy trends and losses.
    *   Output: `results_csv/city_life_expectancy_summary.csv`.
*   **Visualization (`06_vis_lifetables.R`)**:
    *   Generates publication-ready plots:
        1.  $e_0$ Loss over Time (Line plot).
        2.  Survival Curve ($l_x$) shifts (Line plot).
        3.  Mortality Risk Ratios (Ratio plot).
    *   Output: `plots/*.png`.

---

## 3. Data Flow Summary

| Stage | Input File | Key Operation | Output File |
| :--- | :--- | :--- | :--- |
| **0. Input** | `wittgenstein_pop.csv` | Raw input | N/A |
| **1. Attribution** | Temperature Projections | Impact Modeling | `city_period.parquet` |
| **2. Core Gen** | `city_period.parquet` + Demography | **Lifetable Method** | `romania_city_lifetables_full.csv` |
| **3. Analysis** | `romania_city_lifetables_full.csv` | $e_0$ Extraction | `city_life_expectancy_summary.csv` |
| **4. Viz** | `romania_city_lifetables_full.csv` | `ggplot2` | `plots/*.png` |
