################################################################################
# Precompute RR surfaces, vcov matrices, and basis info per city
#
# This runs ONCE at prep time and saves everything the dashboard needs
# to compute multipliers + delta-method CIs instantly (no MC loop).
#
# Outputs in dashboard/data/precomputed/:
#   rr_surfaces.rds   — list by city: rr_single_age matrix, temp_seq, mmt, etc.
#   coef_vcov.rds     — list by city: empirical vcov from 1000 simulation draws
#
# Run after prep_deploy_data.R:
#   Rscript dashboard/precompute_rr.R
################################################################################

library(data.table)
library(arrow)
library(dplyr)
library(dlnm)
library(splines)

cat("=== Precomputing RR surfaces + vcov for dashboard ===\n\n")

# ---------- Config ----------
varfun    <- "bs"
vardegree <- 2
varper    <- c(10, 75, 90)
agelabs   <- c("20-44", "45-64", "65-74", "75-84", "85+")
age_midpoints <- c(32.5, 55, 70, 80, 92.5)
age_range     <- 20:100
gcmexcl <- c("CMCC_CM2_SR5", "TaiESM1")

# ---------- Load data ----------
data_dir   <- "dashboard/data"
temp_path  <- file.path(data_dir, "tmeanproj")
coefs_all  <- fread(file.path(data_dir, "coefs.csv"))

# Full simulation data (1000 draws) for vcov extraction
sim_ds <- open_dataset("data/coef_simu.gz.parquet")

# City list
city_lookup <- fread(file.path(data_dir, "city_lookup.csv"))
keep_codes  <- city_lookup$URAU_CODE

out_dir <- file.path(data_dir, "precomputed")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---------- 1. Compute empirical vcov from simulation draws ----------
cat("Step 1: Computing empirical vcov from 1000 simulation draws...\n")

vcov_list <- list()

for (cc in keep_codes) {
  sim_city <- sim_ds |>
    filter(URAU_CODE == cc) |>
    collect() |>
    as.data.table()

  vcov_city <- list()
  for (ag in agelabs) {
    sim_ag <- sim_city[agegroup == ag, .(b1, b2, b3, b4, b5)]
    if (nrow(sim_ag) < 10) {
      vcov_city[[ag]] <- matrix(0, 5, 5)
      next
    }
    vcov_city[[ag]] <- cov(as.matrix(sim_ag))
  }
  vcov_list[[cc]] <- vcov_city
}
cat(sprintf("  Computed vcov for %d cities × 5 age groups\n", length(vcov_list)))

saveRDS(vcov_list, file.path(out_dir, "coef_vcov.rds"))
cat(sprintf("  Saved: coef_vcov.rds (%.1f KB)\n",
            file.size(file.path(out_dir, "coef_vcov.rds")) / 1024))

# ---------- 2. Precompute RR surfaces per city ----------
cat("\nStep 2: Precomputing RR surfaces per city...\n")

ds <- open_dataset(temp_path)
rr_list <- list()

for (ci in seq_along(keep_codes)) {
  cc <- keep_codes[ci]

  # Load historical data for basis construction
  proj_data <- ds |>
    filter(URAU_CODE == cc) |>
    collect() |>
    as.data.table()
  proj_data[, year := year(date)]

  gcm_cols <- names(proj_data)[grepl("^tas_", names(proj_data))]
  gcm_cols <- gcm_cols[!gsub("tas_", "", gcm_cols) %in% gcmexcl]

  hist_temps <- unlist(proj_data[ssp == "hist", ..gcm_cols], use.names = FALSE)
  hist_temps <- hist_temps[!is.na(hist_temps)]

  varknots <- quantile(hist_temps, varper / 100, na.rm = TRUE)
  varbound <- range(hist_temps, na.rm = TRUE)
  argvar   <- list(fun = varfun, degree = vardegree,
                   knots = varknots, Bound = varbound)

  temp_seq <- seq(varbound[1], varbound[2], by = 0.1)
  n_temp   <- length(temp_seq)
  basis    <- do.call(onebasis, c(list(x = temp_seq), argvar))

  coefs_city <- coefs_all[URAU_CODE == cc]

  # RR matrix (n_temp × 5 age groups) + centered basis for delta method
  rr_matrix  <- matrix(NA, nrow = n_temp, ncol = 5)
  mmt_vec    <- numeric(5)
  # Centered basis: basis(t) - basis(mmt) per age group, needed for delta method
  cen_basis_list <- list()

  for (i in seq_along(agelabs)) {
    ag <- agelabs[i]
    cr <- coefs_city[agegroup == ag]
    if (nrow(cr) == 0) next
    coefs <- as.numeric(cr[, .(b1, b2, b3, b4, b5)])

    log_rr <- basis %*% coefs
    ind <- temp_seq >= quantile(temp_seq, 0.25) &
           temp_seq <= quantile(temp_seq, 0.99)
    mmt <- temp_seq[ind][which.min(log_rr[ind])]
    mmt_vec[i] <- mmt

    cenvec <- do.call(onebasis, c(list(x = mmt), argvar))
    cen_basis <- sweep(basis, 2, cenvec)   # basis - basis(mmt), n_temp × 5
    log_rr_c <- cen_basis %*% coefs
    rr_matrix[, i] <- pmax(exp(as.vector(log_rr_c)), 1)

    cen_basis_list[[ag]] <- cen_basis
  }
  names(mmt_vec) <- agelabs

  # Interpolate RR to single-year ages
  rr_single_age <- matrix(NA, nrow = n_temp, ncol = length(age_range))
  colnames(rr_single_age) <- age_range
  for (t_idx in seq_len(n_temp)) {
    rr_single_age[t_idx, ] <- approx(age_midpoints, rr_matrix[t_idx, ],
                                      xout = age_range, rule = 2)$y
  }
  mmt_single_age <- approx(age_midpoints, mmt_vec,
                            xout = age_range, rule = 2)$y

  rr_list[[cc]] <- list(
    temp_seq       = temp_seq,
    varknots       = varknots,
    varbound       = varbound,
    rr_matrix      = rr_matrix,
    rr_single_age  = rr_single_age,
    mmt_vec        = mmt_vec,
    mmt_single_age = mmt_single_age,
    cen_basis      = cen_basis_list,   # list of 5 matrices (n_temp × 5)
    gcm_cols       = gcm_cols
  )

  rm(proj_data, hist_temps); gc()
  if (ci %% 10 == 0 || ci == length(keep_codes))
    cat(sprintf("  %d / %d cities done\n", ci, length(keep_codes)))
}

saveRDS(rr_list, file.path(out_dir, "rr_surfaces.rds"))
cat(sprintf("  Saved: rr_surfaces.rds (%.1f MB)\n",
            file.size(file.path(out_dir, "rr_surfaces.rds")) / 1024^2))

cat("\nPrecomputation complete.\n")
