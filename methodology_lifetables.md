---
title: "Methodology: City-Level Single-Age Life Table Generation"
geometry: margin=2cm
header-includes:
 - \usepackage{float}
 - \floatplacement{figure}{H}
---

# Methodology: City-Level Single-Age Life Table Generation

## 1. Objective
To generate single-age life tables (ages 0–100) for Romanian cities under two distinct scenarios:

1.  **No-CC (Baseline)**: Mortality rates based on demographic projections only.
2.  **CC (Climate Change)**: Mortality rates adjusted for projected heat/cold-related excess deaths.

## 2. Input Data Sources

| Source | Description | Granularity |
| :--- | :--- | :--- |
| **Wittgenstein Centre** | Demographic projections (Population, Age-Specific Survival Rates). | Country Level, 5-year intervals. |
| **Urban Audit** | Historical city-level population and mortality data. | City Level, Broad Age Groups. |
| **Impact Assessment** | Projected Attributable Numbers ($AN$) for heat/cold mortality. | City Level, aggregated by period/SSP. |

## 3. Statistical Process

### 3.1. Downscaling (Country $\to$ City)
Since demographic projections differ between national averages and specific urban centers, we apply a scaling factor derived from historical data.

$$ Pop_{city}(t) = Pop_{country}(t) \times \left( \frac{Pop_{city}(t_{hist})}{Pop_{country}(t_{hist})} \right) $$

For mortality, a similar scaling factor is applied to deaths to ensure the life table reflects the specific healthy/unhealthy nature of the city relative to the national average.

### 3.2. Single-Age Expansion (Interpolation)
Input data is provided in 5-year age groups ($0-4, 5-9, \dots$). To generate a Life Table, we expand this to single years of age ($0, 1, 2, \dots$).

*   **Method**: Uniform Distribution (Step Function).
*   **Assumption**: For an age group spanning $n$ years (e.g., 5 years), the population and deaths are distributed equally across each single year $i$ within that group.

$$ Pop_i = \frac{Pop_{group}}{n}, \quad Deaths_i = \frac{Deaths_{group}}{n} $$

### 3.3. Scenario Integration

#### Scenario A: No Climate Change (Baseline)
The baseline mortality rate ($m_x$) is derived purely from the downscaled demographic projections.

$$ m_x^{no\_cc} = \frac{Deaths_x}{Pop_x} $$

#### Scenario B: With Climate Change (CC)
We add the specific **Climate Attributable Number** ($AN_{clim}$) to the baseline deaths.

*   **Assumption for Youth**: For ages $0-19$, we assume **zero** additional climate mortality ($AN_{clim} = 0$).
*   **Adults ($20+$)**: $AN_{clim}$ is distributed across relevant ages.

The adjusted mortality rate is:
$$ m_x^{cc} = \frac{Deaths_x + AN_{clim, x}}{Pop_x} = m_x^{no\_cc} + \text{ExcessRate}_x $$

*(Note: While Rates ($m_x$) are additive, Life Expectancy ($e_x$) is not subtractive due to non-linearities in survival).*

## 4. Life Table Construction (Actuarial Method)

We assume a cohort size (radix) of $l_0 = 100,000$. In the formulas below, $n$ represents the width of the age interval (e.g., $n=1$ for single year, $n=5$ for 5-year groups).

| Metric | Symbol | Formula / Method | Description | Units |
| :--- | :--- | :--- | :--- | :--- |
| **Exposure** | $_nK_x$ | Observed / Input | Person-years of exposure to risk (denominator for rates). | Person-Years |
| **Deaths** | $_nD_x$ | Observed / Input | Count of observed deaths in the population. | Persons |
| **Mortality Rate** | $_nm_x$ | $_nm_x = \frac{_nD_x}{_nK_x}$ | Central death rate observed in the population. | Deaths per person-year |
| **Person-Years Factor** | $_na_x$ | Typically $n/2$ | Average years lived in interval by those who die. | Years |
| **Probability of Death** | $_nq_x$ | $_nq_x = \frac{n \cdot _nm_x}{1 + (n - _na_x) \cdot _nm_x}$ | Probability of dying between age $x$ and $x+n$. | Probability (0 to 1) |
| **Probability of Survival** | $_np_x$ | $_np_x = 1 - _nq_x$ | Probability of surviving from $x$ to $x+n$. | Probability (0 to 1) |
| **Survivors** | $l_x$ | $l_{x+n} = l_x \times _np_x$ | Number of people surviving to exact age $x$. | Persons |
| **Life Table Deaths** | $_nd_x$ | $_nd_x = l_x - l_{x+n}$ | Number of deaths in the hypothetical cohort. | Persons |
| **Person-Years** | $_nL_x$ | $_nL_x = n \cdot l_{x+n} + _na_x \cdot _nd_x$ | Total years lived by the cohort between $x$ and $x+n$. | Person-Years |
| **Total Future Years** | $T_x$ | $T_x = \sum_{i=x}^{\omega} \ _nL_i$ | Total years lived by survivors from age $x$ until extinction. | Person-Years |
| **Life Expectancy** | $e_x$ | $e_x = \frac{T_x}{l_x}$ | Average years remaining for a person of age $x$. | Years |

### 4.1. Key Observation on $T_x$
Users may notice that $T_x$ differs between scenarios at young ages ($0-19$) even if $m_x$ is identical.

*   **Zero Impact Assumption (0-19)**: All mortality variables ($m_x, q_x, l_x, d_x$) are identical between scenarios for ages 0-19. We assume no climate effect for this cohort due to data limitations.
*   **Baseline Year (2010)**: For the baseline year 2010, there is no difference between the CC and No-CC scenarios across any age group.
*   **Reason for $T_x$ Divergence**: $T_x$ is a cumulative sum of **future** person-years ($T_0 = L_0 + L_1 + \dots + L_{100}$).
*   **Effect**: Climate change affects adults ($20+$), changing $L_{20}, L_{21}, \dots$. This change propagates backward to $T_0$, altering Life Expectancy at Birth ($e_0$) even if infant mortality rates remain unchanged.

### 4.2. Key Observation on $d_x$ vs Attributable Numbers

A common intuition is that the number of deaths in the climate scenario ($d_x^{cc}$) should equal the baseline deaths ($d_x^{no\_cc}$) plus the attributable deaths ($AN$).

$$ d_x^{cc} \neq d_x^{no\_cc} + AN_{clim} $$
$$ d_x^{cc} \neq d_x^{no\_cc} + (AN_{full} - AN_{demo}) $$

This inequality holds true for two fundamental reasons:

1.  **Cohort Dynamics (The "Harvesting" Effect)**:
    *   In a life table, death is a one-time event effectively removing an individual from the "at risk" population ($l_x$).
    *   Climate change modifies mortality risks. If mortality increases at earlier ages (e.g., due to heat), the cohort size ($l_x^{cc}$) shrinks faster than the baseline ($l_x^{no\_cc}$). Conversely, reduced cold stress could decrease mortality.
    *   By the time the cohort reaches older ages (e.g., 85), there are significantly fewer survivors in the high-impact scenario. Even if the *risk* of death ($q_x$) is higher, applying it to a much smaller population ($l_x$) can result in **fewer** absolute deaths ($d_x$) compared to the baseline.
    *   *Simply put: People who die early from heat cannot die later from old age. You cannot merely "add" deaths; you shift the distribution of death to younger ages.*

2.  **Scale Mismatch**:
    *   **$AN$ (Attributable Number)** is calculated on the **Real Population**, which varies in size (e.g., 2 million people).
    *   **$d_x$ (Life Table Deaths)** is calculated on a **Hypothetical Cohort** (Radix = 100,000).
    *   These are two different universes. To compare them, one would first need to scale $AN$ down to the life table radix, but even then, point #1 (Survivor bias) prevents simple addition.

**Correct Relationship**:
The relationship is additive only at the level of the **Rate ($m_x$)**, not the Count ($d_x$).

$$ m_x^{cc} \approx m_x^{no\_cc} + \frac{AN_{clim}}{Pop_{real}} $$

## 5. Metadata columns
The final output includes Attributable Numbers broken down by specific drivers for validation:

*   `an_clim`: Deaths strictly due to changing climate (Delta Temperature).
*   `an_demo`: Deaths due to demographic changes (Aging population).
*   `an_full`: Total attributable burden vs reference period.

## 6. Outputs and Visualization

The analysis pipeline produces tabular summaries and graphical visualizations to audit the impact of climate change on urban longevity.

### 6.1. Full Life Table Dataset (`results_csv/romania_city_lifetables_full.csv`)
This is the primary output file containing single-age life tables (ages 0-100) for every combination of city, scenario, period, and adaptation level.

**Key Columns:**
*   `mx_no_cc` / `mx_cc`: Mortality rates (Baseline vs. Climate Change).
*   `lx_no_cc` / `lx_cc`: Number of survivors at age $x$.
*   `ex_no_cc` / `ex_cc`: Life expectancy at age $x$.
*   `an_clim`, `an_demo`, `an_full`: Attributable deaths for that specific single age (distributed from broad age groups).

### 6.2. Life Expectancy Summary (`results_csv/city_life_expectancy_summary.csv`)
A unified table tracking the evolution of life expectancy ($e_0$) every 5 years for each city and scenario.

| Column | Description |
| :--- | :--- |
| `e0_baseline` | Life expectancy in the No-CC scenario (based on shared socioeconomic pathways). |
| `e0_climate` | Life expectancy after adjusting for climate-attributable mortality. |
| `loss_years` | The gap between baseline and climate scenarios ($e_0^{no\_cc} - e_0^{cc}$). |
| `pct_change` | Relative impact ($\frac{Diff}{Baseline}$). |

### 6.3. Visualizations (`plots/`)

#### A. Life Expectancy Loss Over Time

*   **Metric**: Months of life lost ($Loss_{years} \times 12$).
*   **Interpretation**: Traces the growing burden of climate change from 2010 to 2100.
*   **Note on Variability**: "Jumps" or fluctuations in the trend line often reflect internal climate variability and the bias-correction blocks used in the ISIMIP3 climate models (warming years vs. calendar years).

![Life Expectancy Loss Over Time](plots/01_e0_loss_over_time.png)

#### B. Survival Curve Shift ($l_x$)

*   **Metric**: Number of survivors out of 100,000 at each age.
*   **Interpretation**: Visually demonstrates the "Harvesting Effect." The CC curve typically dips below the No-CC curve in adulthood/old age, indicating premature mortality.

![Survival Curve Shift](plots/02_survival_curve_2090.png)

#### C. Relative Risk Ratio

*   **Metric**: Ratio of Mortality Rates ($m_x^{cc} / m_x^{no\_cc}$).
*   **Interpretation**: Shows the age-specific intensity of the climate impact. A ratio > 1.0 indicates increased risk. Spikes often occur in elderly populations where absolute mortality is highest.

![Relative Risk Ratio](plots/03_mortality_risk_ratio.png)
