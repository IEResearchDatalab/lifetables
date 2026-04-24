################################################################################
#
# Verify PDF Tables 3 & 4
#
# Reproduce the EPV tables from VIG_climate_life_tables_v3.pdf using
# existing precomputed mortality multipliers and Eurostat projections.
#
# PDF specification:
#   Table 3: ä_{60,2025} and A_{60,2025} at i = 0%, 2%, 4%, RCPs 2.6/4.5/7.0
#   Table 4: ä_{x,t} and A_{x,t} at i = 0%, ages 20/40/60, t = 2025/2050
#   Adaptation = 0% throughout
#
# Requirements: results_csv/bucharest_mortality_multipliers_cohort.csv
#               data/bucharest_mortality_projections.csv
#
################################################################################

library(data.table)

cat("=" , strrep("=", 70), "\n")
cat("VERIFY PDF TABLES 3 & 4\n")
cat("=" , strrep("=", 70), "\n\n")

# --------------------------------------------------------------------------
# 1. Load data
# --------------------------------------------------------------------------

cat("Loading data...\n")

# Multipliers: year × age × ssp × adaptation
mult <- fread("results_csv/bucharest_mortality_multipliers_cohort.csv")
setkey(mult, ssp, adaptation, year, age)
cat(sprintf("  Multipliers: %d rows, SSPs: %s\n",
            nrow(mult), paste(sort(unique(mult$ssp)), collapse = ", ")))

# Eurostat mortality projections
mort <- fread("data/bucharest_mortality_projections.csv")
mort <- mort[age >= 0]
setkey(mort, year, age)
cat(sprintf("  Mortality projections: years %d-%d, ages %d-%d\n",
            min(mort$year), max(mort$year), min(mort$age), max(mort$age)))

# --------------------------------------------------------------------------
# 2. Helper: mx_to_qx
# --------------------------------------------------------------------------
mx_to_qx <- function(mx, ax) {
  mx / (1 + (1 - ax) * mx)
}

# --------------------------------------------------------------------------
# 3. Build a cohort life table
#    Follow an individual aged x at year t along the diagonal
#    (x, t), (x+1, t+1), ..., (100, t+100-x)
# --------------------------------------------------------------------------
build_cohort_lt <- function(start_age, start_year, ssp_val,
                            adapt_lab = "0%", omega = 100) {

  ages  <- start_age:omega
  years <- start_year:(start_year + omega - start_age)

  lt <- data.table(age = ages, year = years)

  # ---- Merge baseline mortality (extend beyond 2100 with 2100 values) ----
  max_mort_year <- max(mort$year)  # 2100
  lt <- merge(lt, mort[, .(year, age, qx, mx, ax)], by = c("year", "age"), all.x = TRUE)

  # For years beyond max available, use the last year's mortality
  if (any(is.na(lt$qx))) {
    for (i in which(is.na(lt$qx))) {
      a <- lt$age[i]
      fallback <- mort[year == max_mort_year & age == a]
      if (nrow(fallback) > 0) {
        lt$qx[i] <- fallback$qx[1]
        lt$mx[i] <- fallback$mx[1]
        lt$ax[i] <- fallback$ax[1]
      } else {
        lt$qx[i] <- 1; lt$mx[i] <- 10; lt$ax[i] <- 0.5  # force death at omega+
      }
    }
  }

  # ---- Merge multipliers (extend beyond 2099 with 2099 values) ----
  max_mult_year <- max(mult$year)  # 2099
  sub_mult <- mult[ssp == ssp_val & adaptation == adapt_lab, .(year, age, multiplier)]
  lt <- merge(lt, sub_mult, by = c("year", "age"), all.x = TRUE)

  # For years beyond max available, use the last year's multiplier
  if (any(is.na(lt$multiplier))) {
    for (i in which(is.na(lt$multiplier))) {
      a <- lt$age[i]
      fallback_m <- sub_mult[year == max_mult_year & age == a]
      if (nrow(fallback_m) > 0) {
        lt$multiplier[i] <- fallback_m$multiplier[1]
      } else {
        lt$multiplier[i] <- 1
      }
    }
  }

  lt <- lt[order(age)]

  # Climate-adjusted mortality
  lt[, mx_clim := mx * multiplier]
  lt[, qx_clim := mx_to_qx(mx_clim, ax)]
  lt[qx > 1, qx := 1]
  lt[qx_clim > 1, qx_clim := 1]

  return(lt)
}

# --------------------------------------------------------------------------
# 4. Whole-life annuity-due: ä_x = sum_{k=0}^{n-1} v^k * k_p_x
# --------------------------------------------------------------------------
whole_life_annuity <- function(lt, qx_col = "qx", i = 0.0) {
  v <- 1 / (1 + i)
  n <- nrow(lt)
  px <- 1 - lt[[qx_col]]
  kpx <- cumprod(c(1, px[-n]))  # k_p_x, k = 0..n-1
  k <- 0:(n - 1)
  sum(v^k * kpx)
}

# --------------------------------------------------------------------------
# 5. Whole-life insurance: A_x = sum_{k=0}^{n-1} v^{k+1} * k_p_x * q_{x+k}
# --------------------------------------------------------------------------
whole_life_insurance <- function(lt, qx_col = "qx", i = 0.0) {
  v <- 1 / (1 + i)
  n <- nrow(lt)
  qx <- lt[[qx_col]]
  px <- 1 - qx
  kpx <- cumprod(c(1, px[-n]))
  sum(v^(1:n) * kpx * qx)
}

# --------------------------------------------------------------------------
# 6. Reproduce Table 3: age 60, year 2025, i = 0%/2%/4%
# --------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n")
cat("TABLE 3: EPVs for age 60, year 2025\n")
cat(strrep("=", 70), "\n\n")

ssp_map <- c("1" = "RCP 2.6", "2" = "RCP 4.5", "3" = "RCP 7.0")
rates   <- c(0.00, 0.02, 0.04)

table3_rows <- list()

for (rate in rates) {
  # Baseline (no climate adjustment)
  lt_base <- build_cohort_lt(60, 2025, ssp_val = "1")
  a_base  <- whole_life_annuity(lt_base, "qx", rate)
  A_base  <- whole_life_insurance(lt_base, "qx", rate)

  row_a <- data.table(quantity = "annuity", i = sprintf("%.0f%%", rate * 100),
                       Base = a_base)
  row_A <- data.table(quantity = "insurance", i = sprintf("%.0f%%", rate * 100),
                       Base = A_base)

  for (ssp_val in names(ssp_map)) {
    lt <- build_cohort_lt(60, 2025, ssp_val)
    a_clim <- whole_life_annuity(lt, "qx_clim", rate)
    A_clim <- whole_life_insurance(lt, "qx_clim", rate)
    set(row_a, j = ssp_map[ssp_val], value = a_clim)
    set(row_A, j = ssp_map[ssp_val], value = A_clim)
  }

  table3_rows <- c(table3_rows, list(row_a, row_A))
}

table3 <- rbindlist(table3_rows, fill = TRUE)
print(table3, digits = 3)

# PDF reference values (Table 3)
cat("\nPDF reference (Table 3):\n")
cat("                     Base     2.6      4.5      7.0      8.5\n")
cat("annuity  0%        24.596   24.587   24.585   24.584   24.474\n")
cat("annuity  2%        18.934   18.930   18.929   18.928   18.862\n")
cat("annuity  4%        15.139   15.136   15.136   15.135   15.094\n")
cat("insurance 0%        0.966    0.966    0.966    0.967    0.968\n")
cat("insurance 2%        0.614    0.614    0.614    0.614    0.616\n")
cat("insurance 4%        0.411    0.411    0.411    0.411    0.413\n")

# --------------------------------------------------------------------------
# 7. Reproduce Table 4: i = 0%, ages 20/40/60, years 2025/2050
# --------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n")
cat("TABLE 4: EPVs at i = 0%, by age and year\n")
cat(strrep("=", 70), "\n\n")

ages_t4  <- c(20, 40, 60)
years_t4 <- c(2025, 2050)

table4_rows <- list()

for (x in ages_t4) {
  for (t in years_t4) {
    lt_base <- build_cohort_lt(x, t, ssp_val = "1")
    a_base <- whole_life_annuity(lt_base, "qx", 0)
    A_base <- whole_life_insurance(lt_base, "qx", 0)

    row_a <- data.table(quantity = "annuity", x = x, t = t, Base = a_base)
    row_A <- data.table(quantity = "insurance", x = x, t = t, Base = A_base)

    for (ssp_val in names(ssp_map)) {
      lt <- build_cohort_lt(x, t, ssp_val)
      a_clim <- whole_life_annuity(lt, "qx_clim", 0)
      A_clim <- whole_life_insurance(lt, "qx_clim", 0)
      set(row_a, j = ssp_map[ssp_val], value = a_clim)
      set(row_A, j = ssp_map[ssp_val], value = A_clim)
    }

    table4_rows <- c(table4_rows, list(row_a, row_A))
  }
}

table4 <- rbindlist(table4_rows, fill = TRUE)
print(table4, digits = 4)

# PDF reference values (Table 4)
cat("\nPDF reference (Table 4, i = 0%):\n")
cat("           x    t      Base     2.6      4.5      7.0      8.5\n")
cat("annuity   20  2025   67.657   67.642   67.600   67.466   67.336\n")
cat("annuity   20  2050   70.084   70.062   70.027   69.804   69.785\n")
cat("annuity   40  2025   45.294   45.280   45.261   45.217   45.091\n")
cat("annuity   40  2050   48.946   48.929   48.888   48.724   48.626\n")
cat("annuity   60  2025   24.596   24.587   24.585   24.584   24.474\n")
cat("annuity   60  2050   28.351   28.338   28.312   28.243   28.125\n")
cat("insurance 20  2025    0.922    0.922    0.923    0.929    0.930\n")
cat("insurance 20  2050    0.909    0.910    0.911    0.919    0.918\n")
cat("insurance 40  2025    0.947    0.947    0.948    0.950    0.953\n")
cat("insurance 40  2050    0.916    0.916    0.917    0.924    0.925\n")
cat("insurance 60  2025    0.966    0.966    0.966    0.967    0.968\n")
cat("insurance 60  2050    0.935    0.935    0.936    0.939    0.943\n")

cat("\nDone.\n")

# --------------------------------------------------------------------------
# 8. Detailed comparison: Table 4 deltas (our code vs PDF)
# --------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n")
cat("DETAILED DELTA COMPARISON (Table 4, annuity, i = 0%)\n")
cat(strrep("=", 70), "\n\n")

# PDF values for comparison (RCPs 2.6, 4.5, 7.0 only — no 8.5 in our data)
pdf_annuity <- data.table(
  x    = c(20, 20, 40, 40, 60, 60),
  t    = c(2025, 2050, 2025, 2050, 2025, 2050),
  Base = c(67.657, 70.084, 45.294, 48.946, 24.596, 28.351),
  RCP26 = c(67.642, 70.062, 45.280, 48.929, 24.587, 28.338),
  RCP45 = c(67.600, 70.027, 45.261, 48.888, 24.585, 28.312),
  RCP70 = c(67.466, 69.804, 45.217, 48.724, 24.584, 28.243)
)

our_annuity <- table4[quantity == "annuity"]

cat(sprintf("%-6s %-6s | %-10s %-10s %-10s | %-10s %-10s %-10s\n",
            "x", "t", "Our d2.6", "Our d4.5", "Our d7.0",
            "PDF d2.6", "PDF d4.5", "PDF d7.0"))
cat(strrep("-", 80), "\n")

for (i in 1:nrow(pdf_annuity)) {
  row_ours <- our_annuity[x == pdf_annuity$x[i] & t == pdf_annuity$t[i]]
  our_d26 <- row_ours[["RCP 2.6"]] - row_ours$Base
  our_d45 <- row_ours[["RCP 4.5"]] - row_ours$Base
  our_d70 <- row_ours[["RCP 7.0"]] - row_ours$Base
  pdf_d26 <- pdf_annuity$RCP26[i] - pdf_annuity$Base[i]
  pdf_d45 <- pdf_annuity$RCP45[i] - pdf_annuity$Base[i]
  pdf_d70 <- pdf_annuity$RCP70[i] - pdf_annuity$Base[i]
  cat(sprintf("%-6d %-6d | %+10.4f %+10.4f %+10.4f | %+10.4f %+10.4f %+10.4f\n",
              pdf_annuity$x[i], pdf_annuity$t[i],
              our_d26, our_d45, our_d70, pdf_d26, pdf_d45, pdf_d70))
}

cat("\nRatio (our delta / PDF delta):\n")
for (i in 1:nrow(pdf_annuity)) {
  row_ours <- our_annuity[x == pdf_annuity$x[i] & t == pdf_annuity$t[i]]
  our_d26 <- row_ours[["RCP 2.6"]] - row_ours$Base
  our_d45 <- row_ours[["RCP 4.5"]] - row_ours$Base
  our_d70 <- row_ours[["RCP 7.0"]] - row_ours$Base
  pdf_d26 <- pdf_annuity$RCP26[i] - pdf_annuity$Base[i]
  pdf_d45 <- pdf_annuity$RCP45[i] - pdf_annuity$Base[i]
  pdf_d70 <- pdf_annuity$RCP70[i] - pdf_annuity$Base[i]
  cat(sprintf("  x=%d t=%d: RCP2.6 %.1fx  RCP4.5 %.1fx  RCP7.0 %.1fx\n",
              pdf_annuity$x[i], pdf_annuity$t[i],
              our_d26 / pdf_d26, our_d45 / pdf_d45, our_d70 / pdf_d70))
}

# --------------------------------------------------------------------------
# 9. Save comparison to CSV
# --------------------------------------------------------------------------
fwrite(table3, "results_csv/verify_table3.csv")
fwrite(table4, "results_csv/verify_table4.csv")
cat("\nSaved: results_csv/verify_table3.csv, results_csv/verify_table4.csv\n")

