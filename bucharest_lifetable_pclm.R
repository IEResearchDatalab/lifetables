# ==============================================================================
# City-Level Single-Year Life Table using PCLM Ungrouping (2023)
# ==============================================================================
# This script computes a single-year life table using the Penalized Composite
# Link Model (PCLM) to disaggregate 5-year grouped data.
#
# Method: Rizzi, S., Gampe, J., & Eilers, P. H. (2015). Efficient estimation 
# of smooth distributions from coarsely grouped data. American Journal of 
# Epidemiology, 182(2), 138-147.
#
# R Package: ungroup (https://cran.r-project.org/package=ungroup)
# ==============================================================================

library(dplyr)
library(tidyr)
library(readr)

# Load ungroup package for PCLM
library(ungroup)

# Helper function for string concatenation
`%+%` <- function(a, b) paste0(a, b)

# Load city configuration
source("config.R")

# ==============================================================================
# STEP 0: Load Bucharest Data
# ==============================================================================

# --- Load Bucharest Population (POP106A) ---
pop_raw <- read_csv("data/POP106A.csv", show_col_types = FALSE)
names(pop_raw) <- c("age_group", "region", "population")

bucharest_pop <- pop_raw %>%
  filter(region == "Bucharest Municipality") %>%
  select(age_group, population)

# --- Load Bucharest Deaths (POP206K) ---
deaths_raw <- read_csv("data/POP206K.csv", show_col_types = FALSE)
names(deaths_raw) <- c("age_group", "region", "deaths")

bucharest_deaths <- deaths_raw %>%
  filter(region == "Bucharest Municipality") %>%
  select(age_group, deaths)

# --- Load Yearbook Benchmarks for comparison ---
yearbook_lx <- read_csv("data/2.62.csv", show_col_types = FALSE)
names(yearbook_lx) <- c("age", "lx_male", "lx_female")
yearbook_lx <- yearbook_lx %>%
  mutate(
    age = as.numeric(age),
    lx_male = as.numeric(lx_male),
    lx_female = as.numeric(lx_female),
    lx_combined = (lx_male + lx_female) / 2
  )

yearbook_ex <- read_csv("data/2.63.csv", show_col_types = FALSE)
names(yearbook_ex) <- c("age", "ex_male", "ex_female")
yearbook_ex <- yearbook_ex %>%
  mutate(
    age = as.numeric(age),
    ex_male = as.numeric(ex_male),
    ex_female = as.numeric(ex_female),
    ex_combined = (ex_male + ex_female) / 2
  )

# ==============================================================================
# STEP 1: Parse age groups and prepare 5-year grouped data
# ==============================================================================

parse_age_group <- function(ag) {
  ag <- trimws(ag)
  if (ag == "Total") return(list(start = NA, end = NA, single = NA))
  if (ag == "0 years") return(list(start = 0, end = 0, single = 0))
  if (grepl("^[0-9]+ year$", ag)) {
    a <- as.numeric(gsub(" year", "", ag))
    return(list(start = a, end = a, single = a))
  }
  if (grepl("^[0-9]+ years$", ag)) {
    a <- as.numeric(gsub(" years", "", ag))
    return(list(start = a, end = a, single = a))
  }
  if (grepl("^[0-9]+-[ ]?[0-9]+ years$", ag)) {
    m <- regmatches(ag, regexec("^([0-9]+)-[ ]?([0-9]+)", ag))[[1]]
    return(list(start = as.numeric(m[2]), end = as.numeric(m[3]), single = NA))
  }
  if (grepl("^85 years and over$", ag)) {
    return(list(start = 85, end = 110, single = NA))
  }
  return(list(start = NA, end = NA, single = NA))
}

# Apply parsing to population
bucharest_pop$parsed <- lapply(bucharest_pop$age_group, parse_age_group)
bucharest_pop$age_start <- sapply(bucharest_pop$parsed, function(x) x$start)
bucharest_pop$age_end <- sapply(bucharest_pop$parsed, function(x) x$end)
bucharest_pop$single_age <- sapply(bucharest_pop$parsed, function(x) x$single)

# Apply parsing to deaths
bucharest_deaths$parsed <- lapply(bucharest_deaths$age_group, parse_age_group)
bucharest_deaths$age_start <- sapply(bucharest_deaths$parsed, function(x) x$start)
bucharest_deaths$age_end <- sapply(bucharest_deaths$parsed, function(x) x$end)
bucharest_deaths$single_age <- sapply(bucharest_deaths$parsed, function(x) x$single)

# ==============================================================================
# STEP 2: Extract 5-year grouped data for PCLM
# ==============================================================================

# Get 5-year age group population
pop_5yr <- bucharest_pop %>%
  filter(!is.na(age_start), is.na(single_age), age_end - age_start == 4) %>%
  select(age_start, age_end, population) %>%
  arrange(age_start)

# Get 5-year age group deaths
deaths_5yr <- bucharest_deaths %>%
  filter(!is.na(age_start), is.na(single_age), age_end - age_start == 4) %>%
  select(age_start, age_end, deaths) %>%
  arrange(age_start)

# Get 85+ open interval
pop_85plus <- bucharest_pop %>%
  filter(age_group == "85 years and over") %>%
  pull(population)

deaths_85plus <- bucharest_deaths %>%
  filter(age_group == "85 years and over") %>%
  pull(deaths)

cat("=== 5-Year Grouped Data ===\n")
cat("\nPopulation:\n")
print(pop_5yr)
cat("\nDeaths:\n")
print(deaths_5yr)
cat(sprintf("\n85+ Population: %d\n", pop_85plus))
cat(sprintf("85+ Deaths: %d\n", deaths_85plus))

# ==============================================================================
# STEP 3: Prepare data for PCLM
# ==============================================================================

# PCLM needs:
# - y: vector of counts (deaths or population)
# - x: vector of lower bounds of age intervals
# - nlast: width of the last open interval (or use open.last)

# Create vectors for deaths
deaths_vec <- c(deaths_5yr$deaths, deaths_85plus)
pop_vec <- c(pop_5yr$population, pop_85plus)

# Age interval lower bounds
x_ages <- c(pop_5yr$age_start, 85)

# Age interval widths (5 for closed intervals, larger for open 85+)
# We'll assume 85+ covers ages 85-100 (16 years)
nlast <- 16  # ages 85-100

cat("\n=== PCLM Input Vectors ===\n")
cat("Age intervals:", paste(x_ages, collapse = ", "), "\n")
cat("Deaths:", paste(deaths_vec, collapse = ", "), "\n")
cat("Population:", paste(pop_vec, collapse = ", "), "\n")

# ==============================================================================
# STEP 4: Apply PCLM to ungroup deaths and population
# ==============================================================================

cat("\n=== Applying PCLM Ungrouping ===\n")

# Define the output age range (single years 0-100)
out_ages <- 0:100

# PCLM for deaths
# The pclm function estimates a smooth distribution from grouped counts
cat("Ungrouping deaths...\n")
pclm_deaths <- pclm(
  x = x_ages,           # lower bounds of intervals
  y = deaths_vec,       # grouped counts
  nlast = nlast,        # width of last interval
  out.step = 1          # output step size (single years)
)

# PCLM for population (exposure)
cat("Ungrouping population...\n")
pclm_pop <- pclm(
  x = x_ages,
  y = pop_vec,
  nlast = nlast,
  out.step = 1
)

# Extract ungrouped estimates
deaths_single <- pclm_deaths$fitted
pop_single <- pclm_pop$fitted

# Create data frame with single-year estimates
ungrouped_data <- data.frame(
  age = 0:(length(deaths_single) - 1),
  deaths_pclm = deaths_single,
  pop_pclm = pop_single
)

# Calculate single-year death rates
ungrouped_data <- ungrouped_data %>%
  mutate(mx = deaths_pclm / pop_pclm)

cat("\n=== PCLM Ungrouped Data (First 30 ages) ===\n")
print(head(ungrouped_data, 30))

cat("\n=== PCLM Ungrouped Data (Ages 60-85) ===\n")
print(filter(ungrouped_data, age >= 60, age <= 85))

# Verify totals match
cat("\n=== Verification: Grouped vs Ungrouped Totals ===\n")
cat(sprintf("Original grouped deaths: %d\n", sum(deaths_vec)))
cat(sprintf("PCLM ungrouped deaths:   %.0f\n", sum(deaths_single)))
cat(sprintf("Original grouped pop:    %d\n", sum(pop_vec)))
cat(sprintf("PCLM ungrouped pop:      %.0f\n", sum(pop_single)))

# ==============================================================================
# STEP 5: Convert mx to qx (probability of death)
# ==============================================================================

ungrouped_data <- ungrouped_data %>%
  mutate(
    # ax: fraction of year lived by those dying
    ax = ifelse(age == 0 & mx < 0.01, 0.1, 0.5),
    # qx: probability of dying
    qx = mx / (1 + (1 - ax) * mx),
    qx = pmin(qx, 1)  # Cap at 1
  )

# ==============================================================================
# STEP 6: Build the Complete Life Table
# ==============================================================================

build_lifetable <- function(data, radix = 100000) {
  
  df <- data %>%
    filter(!is.na(mx), mx > 0) %>%
    arrange(age)
  
  n <- nrow(df)
  
  # Initialize columns
  df$lx <- NA_real_
  df$dx <- NA_real_
  df$Lx <- NA_real_
  df$Tx <- NA_real_
  df$ex <- NA_real_
  
  # Start with radix
  df$lx[1] <- radix
  
  # Calculate survivors and deaths
  for (i in 1:(n-1)) {
    df$dx[i] <- df$lx[i] * df$qx[i]
    df$lx[i+1] <- df$lx[i] - df$dx[i]
  }
  
  # Last age group - assume everyone dies
  df$dx[n] <- df$lx[n]
  
  # Calculate person-years lived
  for (i in 1:n) {
    if (i == 1) {
      df$Lx[i] <- df$lx[i+1] + df$ax[i] * df$dx[i]
    } else if (i < n) {
      df$Lx[i] <- df$lx[i+1] + 0.5 * df$dx[i]
    } else {
      # Open interval
      if (df$mx[i] > 0) {
        df$Lx[i] <- df$lx[i] / df$mx[i]
      } else {
        df$Lx[i] <- 0
      }
    }
  }
  
  # Calculate total years remaining
  df$Tx[n] <- df$Lx[n]
  for (i in (n-1):1) {
    df$Tx[i] <- df$Tx[i+1] + df$Lx[i]
  }
  
  # Calculate life expectancy
  df$ex <- df$Tx / df$lx
  
  return(df)
}

# Build life table
bucharest_lifetable <- build_lifetable(ungrouped_data)

cat("\n=== Bucharest PCLM Life Table 2023 (ages 0-20) ===\n")
print(bucharest_lifetable %>% select(age, mx, qx, lx, dx, Lx, ex) %>% head(21))

cat("\n=== Bucharest PCLM Life Table 2023 (ages 60-85) ===\n")
print(bucharest_lifetable %>% select(age, mx, qx, lx, dx, Lx, ex) %>% filter(age >= 60, age <= 85))

# ==============================================================================
# STEP 7: Calibration Check Against Yearbook Benchmarks
# ==============================================================================

cat("\n" %+% strrep("=", 70) %+% "\n")
cat("CALIBRATION CHECK: PCLM vs Yearbook 2024 Benchmarks\n")
cat(strrep("=", 70) %+% "\n")

benchmark_ages <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)

comparison <- bucharest_lifetable %>%
  filter(age %in% benchmark_ages) %>%
  select(age, lx, ex) %>%
  left_join(yearbook_lx %>% select(age, lx_combined), by = "age") %>%
  left_join(yearbook_ex %>% select(age, ex_combined), by = "age") %>%
  mutate(
    lx_diff = lx - lx_combined,
    ex_diff = ex - ex_combined
  )

cat("\n--- Life Table Comparison (PCLM vs Yearbook) ---\n")
cat("Age | Computed lx | Yearbook lx | Diff | Computed ex | Yearbook ex | Diff\n")
cat(strrep("-", 80), "\n")
for (i in 1:nrow(comparison)) {
  cat(sprintf("%3d | %10.0f | %10.0f | %+6.0f | %11.2f | %11.2f | %+5.2f\n",
              comparison$age[i],
              comparison$lx[i],
              comparison$lx_combined[i],
              comparison$lx_diff[i],
              comparison$ex[i],
              comparison$ex_combined[i],
              comparison$ex_diff[i]))
}

cat("\n" %+% strrep("=", 70) %+% "\n")
cat("NOTE: PCLM uses penalized B-splines to smooth the ungrouping.\n")
cat("The Yearbook uses 2021-2023 period data (includes COVID years).\n")
cat(strrep("=", 70) %+% "\n")

# ==============================================================================
# STEP 8: Save Results
# ==============================================================================

output <- bucharest_lifetable %>%
  select(age, deaths_pclm, pop_pclm, mx, ax, qx, lx, dx, Lx, Tx, ex) %>%
  arrange(age)

write_csv(output, sprintf("results_csv/%s_lifetable_2023_pclm.csv", city_name_lower))
cat(sprintf("\n✓ Full PCLM life table saved to: results_csv/%s_lifetable_2023_pclm.csv\n", city_name_lower))

# Summary table
summary_ages <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)

summary_table <- output %>%
  filter(age %in% summary_ages) %>%
  select(age, qx, lx, ex)

write_csv(summary_table, sprintf("results_csv/%s_lifetable_2023_pclm_summary.csv", city_name_lower))
cat(sprintf("✓ Summary table saved to: results_csv/%s_lifetable_2023_pclm_summary.csv\n", city_name_lower))

# ==============================================================================
# Final Summary
# ==============================================================================

cat("\n" %+% strrep("=", 70) %+% "\n")
cat("BUCHAREST 2023 LIFE TABLE (PCLM METHOD) - FINAL RESULTS\n")
cat(strrep("=", 70) %+% "\n\n")

cat("Life Expectancy at Birth (e0):\n")
cat(sprintf("  Both Sexes: %.2f years\n\n", bucharest_lifetable$ex[1]))

cat("Life Expectancy at Age 65:\n")
cat(sprintf("  Both Sexes: %.2f years\n", bucharest_lifetable$ex[bucharest_lifetable$age == 65]))

cat("\nInfant Mortality Rate (q0 × 1000):\n")
cat(sprintf("  Both Sexes: %.2f per 1,000\n", bucharest_lifetable$qx[1] * 1000))

cat("\nProbability of Dying Between Ages 15 and 60 (45q15):\n")
lx_15 <- bucharest_lifetable$lx[bucharest_lifetable$age == 15]
lx_60 <- bucharest_lifetable$lx[bucharest_lifetable$age == 60]
q_15_60 <- (lx_15 - lx_60) / lx_15
cat(sprintf("  Both Sexes: %.2f%%\n", q_15_60 * 100))

cat("\n" %+% strrep("=", 70) %+% "\n")

# ==============================================================================
# Compare with previous method (proportional adjustment)
# ==============================================================================

prev_file <- sprintf("results_csv/%s_lifetable_2023.csv", city_name_lower)
if (file.exists(prev_file)) {
  cat("\n=== COMPARISON: PCLM vs Proportional Adjustment Method ===\n\n")
  
  prev_method <- read_csv(prev_file, show_col_types = FALSE)
  
  compare_methods <- bucharest_lifetable %>%
    filter(age %in% benchmark_ages) %>%
    select(age, lx_pclm = lx, ex_pclm = ex) %>%
    left_join(
      prev_method %>% filter(age %in% benchmark_ages) %>% select(age, lx_prop = lx, ex_prop = ex),
      by = "age"
    ) %>%
    mutate(
      lx_diff = lx_pclm - lx_prop,
      ex_diff = ex_pclm - ex_prop
    )
  
  cat("Age | PCLM lx | Prop.Adj lx | Diff | PCLM ex | Prop.Adj ex | Diff\n")
  cat(strrep("-", 76), "\n")
  for (i in 1:nrow(compare_methods)) {
    cat(sprintf("%3d | %7.0f | %11.0f | %+5.0f | %7.2f | %11.2f | %+5.2f\n",
                compare_methods$age[i],
                compare_methods$lx_pclm[i],
                compare_methods$lx_prop[i],
                compare_methods$lx_diff[i],
                compare_methods$ex_pclm[i],
                compare_methods$ex_prop[i],
                compare_methods$ex_diff[i]))
  }
}

# ==============================================================================
# STEP 9: Generate LaTeX Table for main.tex
# ==============================================================================

cat("\n=== Generating LaTeX Table ===\n")

# Select representative ages for the table
table_ages <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)

tex_data <- bucharest_lifetable %>%
  filter(age %in% table_ages) %>%
  select(age, mx, qx, lx, dx, Lx, ex) %>%
  arrange(age)

# Build LaTeX table
tex_file <- sprintf("%s_lifetable_2023.tex", city_name_lower)

tex_lines <- c(
  sprintf("\\begin{table}[htb]"),
  sprintf("\\centering"),
  sprintf("\\caption{Baseline life table for the city of %s (2023), using the Penalised Composite Link Model (PCLM) method for single-year disaggregation. Selected ages shown. Radix $\\ell_0 = 100{,}000$. Life expectancy at birth: $e_0 = %.2f$ years.}", city_name, bucharest_lifetable$ex[1]),
  sprintf("\\label{tab:%s-lifetable-2023}", city_name_lower),
  "\\vspace{5mm}",
  "\\begin{tabular}{rSSSSS}",
  "\\toprule",
  "\\textbf{Age $x$} & {$m_x$} & {$q_x$} & {$\\ell_x$} & {$d_x$} & {$e_x$} \\\\",
  "\\midrule"
)

for (i in 1:nrow(tex_data)) {
  row <- tex_data[i, ]
  tex_lines <- c(tex_lines, sprintf(
    "%d & %.5f & %.5f & %.0f & %.0f & %.2f \\\\",
    row$age, row$mx, row$qx, row$lx, row$dx, row$ex
  ))
}

tex_lines <- c(tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(tex_lines, tex_file)
cat(sprintf("✓ LaTeX table saved to: %s\n", tex_file))
