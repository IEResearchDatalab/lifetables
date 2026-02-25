################################################################################
#
# Interactive Pipeline Dashboard
# ================================
# Step-by-step walk-through of the climate-adjusted mortality multiplier
# pipeline.  The user selects a city, baseline period, and RCP scenario;
# the app runs each stage live and displays intermediate diagnostics.
#
# Launch:
#   shiny::runApp("dashboard")
#
################################################################################

library(shiny)
library(shinydashboard)
library(bslib)
library(data.table)
library(arrow)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

# ---------------------------------------------------------------------------
# 0. One-time data loading (before Shiny starts)
# ---------------------------------------------------------------------------

# Resolve data directory: prefer local data/ (deployment), fallback ../data/ (dev)
if (file.exists("data/coefs.csv")) {
  data_dir <- "data"
} else {
  data_dir <- "../data"
}

# Temperature dataset: partitioned by URAU_CODE (Hive-style) or single file
temp_path <- file.path(data_dir, "tmeanproj")   # partitioned directory
if (!dir.exists(temp_path)) {
  temp_path <- file.path(data_dir, "tmeanproj.gz.parquet")  # fallback: single file
}

# City lookup table
if (file.exists(file.path(data_dir, "city_lookup.csv"))) {
  city_lookup <- fread(file.path(data_dir, "city_lookup.csv"))
} else {
  city_lookup <- fread(file.path(data_dir, "city_results.csv"))[
    , .(URAU_CODE, LABEL, CNTR_CODE, cntr_name)
  ] |> unique()
  city_lookup[, display := paste0(LABEL, " (", cntr_name, ")")]
}
setorder(city_lookup, cntr_name, LABEL)

# Coefficients (point estimates)
coefs_all <- fread(file.path(data_dir, "coefs.csv"))

# Precomputed RR surfaces + vcov (from precompute_rr.R)
precomp_dir <- file.path(data_dir, "precomputed")
if (dir.exists(precomp_dir)) {
  rr_surfaces <- readRDS(file.path(precomp_dir, "rr_surfaces.rds"))
  coef_vcov   <- readRDS(file.path(precomp_dir, "coef_vcov.rds"))
  has_precomputed <- TRUE
} else {
  rr_surfaces <- NULL
  coef_vcov   <- NULL
  has_precomputed <- FALSE
}

# Keep only cities that have both coefs AND temperature data
avail_codes <- unique(coefs_all$URAU_CODE)
city_lookup <- city_lookup[URAU_CODE %in% avail_codes]

# Named vector for selectize input
city_choices <- setNames(city_lookup$URAU_CODE, city_lookup$display)

# Age group config (same as config.R)
agebreaks    <- c(20, 45, 65, 75, 85, Inf)
agelabs      <- c("20-44", "45-64", "65-74", "75-84", "85+")
age_midpoints <- c(32.5, 55, 70, 80, 92.5)
age_range     <- 20:100

# GCM exclusions (already removed at prep stage for partitioned data,
# but kept here for fallback / local dev with full parquet)
gcmexcl <- c("CMCC_CM2_SR5", "TaiESM1")

# SSP / RCP map
rcp_map <- c("1" = "RCP 2.6 / SSP1", "2" = "RCP 4.5 / SSP2", "3" = "RCP 7.0 / SSP3")

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Climate–Mortality Pipeline", titleWidth = 310),
  
  dashboardSidebar(
    width = 310,
    
    tags$div(style = "padding: 10px 15px;",
      tags$h4("Parameters", style = "color:#fff; margin-top:0"),
      
      selectizeInput("city", "City",
                     choices  = city_choices,
                     selected = "RO001C",
                     options  = list(maxOptions = 900,
                                    placeholder = "Type a city name...")),
      
      radioButtons("baseline_type", "Baseline Period",
                   choices = c("1990–2019 (climatological)" = "clim",
                               "Single year" = "single"),
                   selected = "clim"),
      
      conditionalPanel(
        "input.baseline_type == 'single'",
        numericInput("baseline_year", "Year", value = 2019, min = 1981, max = 2025)
      ),
      
      selectInput("rcp", "Scenario",
                  choices  = rcp_map,
                  selected = "RCP 7.0 / SSP3"),
      
      selectInput("component", "RR Component",
                  choices  = c("Heat only" = "heat",
                               "Cold only" = "cold",
                               "Total (heat + cold)" = "total"),
                  selected = "heat"),
      
      sliderInput("target_years", "Target Years",
                  min = 2030, max = 2099, value = c(2050, 2099),
                  step = 1, sep = ""),
      
      tags$hr(style = "border-color:#555"),
      
      actionButton("run_pipeline", "Run Pipeline",
                   icon = icon("play"),
                   style = "width:100%; background-color:#3c8dbc; color:white;
                            font-weight:bold; font-size:15px; padding: 10px;"),
      
      tags$br(), tags$br(),
      tags$p(style = "color:#aaa; font-size:11px; padding:0 5px;",
             "Click 'Run Pipeline' after choosing parameters.",
             "Each pipeline step is shown in the main panel.")
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f9f9f9; }
      .box-header { padding-bottom: 5px; }
      .shiny-notification { position: fixed; top: 60px; right: 20px; width: 380px; }
    "))),
    
    fluidRow(
      # Step 1: Data loading
      box(title = "Step 1: Temperature Data", width = 12,
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          verbatimTextOutput("step1_log"),
          plotlyOutput("step1_temp_hist", height = "260px")
      )
    ),
    fluidRow(
      # Step 2: RR curves
      box(title = "Step 2: Exposure–Response Curves", width = 6,
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          plotlyOutput("step2_rr_curves", height = "340px")
      ),
      # Step 3: MMT summary
      box(title = "Step 3: MMT by Age Group", width = 6,
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          DTOutput("step3_mmt_table"),
          tags$br(),
          verbatimTextOutput("step3_log")
      )
    ),
    fluidRow(
      # Step 4: Baseline RR
      box(title = "Step 4: Baseline Temperature Distribution", width = 6,
          status = "warning", solidHeader = TRUE, collapsible = TRUE,
          plotlyOutput("step4_baseline_hist", height = "260px"),
          verbatimTextOutput("step4_log")
      ),
      # Step 5: Target year RR
      box(title = "Step 5: Target Year Temperatures", width = 6,
          status = "warning", solidHeader = TRUE, collapsible = TRUE,
          plotlyOutput("step5_target_hist", height = "260px"),
          verbatimTextOutput("step5_log")
      )
    ),
    fluidRow(
      # Step 6: Multipliers
      box(title = "Step 6: Mortality Multiplier by Age", width = 12,
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          plotlyOutput("step6_multiplier_plot", height = "380px"),
          tags$br(),
          DTOutput("step6_multiplier_table")
      )
    )
  )
)

# ---------------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Reactive values for pipeline state
  rv <- reactiveValues(
    gcm_cols       = NULL,
    hist_temp_bins = NULL,
    rr_matrix      = NULL,
    rr_single_age  = NULL,
    mmt_vec        = NULL,
    mmt_single_age = NULL,
    temp_seq       = NULL,
    varknots       = NULL,
    varbound       = NULL,
    baseline_hist  = NULL,
    target_hists   = NULL,
    shared_xlim    = NULL,
    multipliers    = NULL,
    log_step1      = "",
    log_step3      = "",
    log_step4      = "",
    log_step5      = "",
    ran            = FALSE
  )
  
  # ------ Main pipeline trigger ------
  observeEvent(input$run_pipeline, {
    
    req(input$city)
    
    city_code <- input$city
    city_row  <- city_lookup[URAU_CODE == city_code]
    city_label <- if (nrow(city_row)) city_row$display[1] else city_code
    
    # Determine SSP code from selected RCP
    ssp_val <- names(rcp_map)[rcp_map == input$rcp]
    component <- input$component
    
    # Determine target years
    target_years <- unique(c(input$target_years[1],
                             round(mean(input$target_years)),
                             input$target_years[2]))
    
    # Baseline config
    if (input$baseline_type == "clim") {
      bl_period <- 1990:2019
      bl_label  <- "1990–2019"
    } else {
      bl_period <- input$baseline_year
      bl_label  <- as.character(input$baseline_year)
    }
    
    withProgress(message = paste("Running pipeline for", city_label),
                 value = 0, max = 6, {
    
    # ================================================================
    # STEP 1: Load temperature data (lazy — only metadata collected here)
    # ================================================================
    incProgress(1, detail = "Loading temperature data...")
    
    ds <- open_dataset(temp_path)
    
    # Collect ONLY this city's data (partition-pruned → reads ~4 MB)
    proj_data <- ds %>%
      dplyr::filter(URAU_CODE == city_code) %>%
      dplyr::collect() %>%
      as.data.table()
    
    proj_data[, year := year(date)]
    proj_data[, doy := as.integer(format(date, "%j"))]
    proj_data[doy > 365, doy := 365L]
    
    gcm_cols <- names(proj_data)[grepl("^tas_", names(proj_data))]
    gcm_cols <- gcm_cols[!gsub("tas_", "", gcm_cols) %in% gcmexcl]
    
    # Do NOT store proj_data in rv — use it within this pipeline block only
    rv$gcm_cols  <- gcm_cols
    
    n_gcm <- length(gcm_cols)
    yr_range <- range(proj_data$year)
    ssp_list <- unique(proj_data$ssp)
    
    rv$log_step1 <- sprintf(
      "City: %s (%s)\nRows: %s\nGCMs: %d\nYear range: %d – %d\nSSPs: %s",
      city_label, city_code,
      format(nrow(proj_data), big.mark = ","),
      n_gcm, yr_range[1], yr_range[2],
      paste(ssp_list, collapse = ", ")
    )
    
    # Pre-compute Step 1 histogram (historical temps, all GCMs pooled)
    hist_data <- proj_data[ssp == "hist"]
    hist_temps <- unlist(hist_data[, ..gcm_cols], use.names = FALSE)
    hist_temps <- hist_temps[!is.na(hist_temps)]
    rv$hist_temp_bins <- hist(hist_temps, breaks = 80, plot = FALSE)
    rm(hist_data); gc()
    
    # ================================================================
    # STEP 2 & 3: RR curves + MMT (precomputed or computed on-the-fly)
    # ================================================================
    incProgress(1, detail = "Loading RR curves...")
    
    if (has_precomputed && city_code %in% names(rr_surfaces)) {
      # ---- Use precomputed surfaces (instant) ----
      pc <- rr_surfaces[[city_code]]
      rr_matrix      <- pc$rr_matrix
      rr_single_age  <- pc$rr_single_age
      mmt_vec        <- pc$mmt_vec
      mmt_single_age <- pc$mmt_single_age
      temp_seq       <- pc$temp_seq
      varknots       <- pc$varknots
      varbound       <- pc$varbound
      cen_basis_list <- pc$cen_basis
    } else {
      # ---- Fallback: compute from scratch (needs dlnm) ----
      coefs_city <- coefs_all[URAU_CODE == city_code]
      hist_data <- proj_data[ssp == "hist"]
      hist_temps_b <- unlist(hist_data[, ..gcm_cols], use.names = FALSE)
      hist_temps_b <- hist_temps_b[!is.na(hist_temps_b)]
      
      varper <- c(10, 75, 90)
      varknots <- quantile(hist_temps_b, varper / 100, na.rm = TRUE)
      varbound <- range(hist_temps_b, na.rm = TRUE)
      argvar   <- list(fun = "bs", degree = 2, knots = varknots, Bound = varbound)
      
      temp_seq <- seq(varbound[1], varbound[2], by = 0.1)
      n_t <- length(temp_seq)
      basis <- do.call(dlnm::onebasis, c(list(x = temp_seq), argvar))
      
      rr_matrix  <- matrix(NA, nrow = n_t, ncol = length(agelabs))
      mmt_vec    <- numeric(length(agelabs))
      cen_basis_list <- list()
      
      for (i in seq_along(agelabs)) {
        ag <- agelabs[i]
        coef_row <- coefs_city[agegroup == ag]
        if (nrow(coef_row) == 0) next
        coefs_v <- as.numeric(coef_row[, .(b1, b2, b3, b4, b5)])
        
        log_rr <- basis %*% coefs_v
        ind <- temp_seq >= quantile(temp_seq, 0.25) & temp_seq <= quantile(temp_seq, 0.99)
        mmt <- temp_seq[ind][which.min(log_rr[ind])]
        mmt_vec[i] <- mmt
        
        cenvec <- do.call(dlnm::onebasis, c(list(x = mmt), argvar))
        cen_basis <- sweep(basis, 2, cenvec)
        rr_matrix[, i] <- pmax(exp(as.vector(cen_basis %*% coefs_v)), 1)
        cen_basis_list[[ag]] <- cen_basis
      }
      names(mmt_vec) <- agelabs
      
      rr_single_age <- matrix(NA, nrow = n_t, ncol = length(age_range))
      colnames(rr_single_age) <- age_range
      for (t_idx in seq_len(n_t)) {
        rr_single_age[t_idx, ] <- approx(age_midpoints, rr_matrix[t_idx, ],
                                          xout = age_range, rule = 2)$y
      }
      mmt_single_age <- approx(age_midpoints, mmt_vec, xout = age_range, rule = 2)$y
      rm(hist_data, hist_temps_b); gc()
    }
    
    rv$rr_matrix     <- rr_matrix
    rv$rr_single_age <- rr_single_age
    rv$mmt_vec       <- mmt_vec
    rv$mmt_single_age <- mmt_single_age
    rv$temp_seq      <- temp_seq
    rv$varknots      <- varknots
    rv$varbound      <- varbound
    
    n_temp <- length(temp_seq)
    
    rv$log_step3 <- sprintf(
      "Temp range: %.1f to %.1f °C\nKnots: %.1f, %.1f, %.1f °C\nSource: %s",
      varbound[1], varbound[2],
      varknots[1], varknots[2], varknots[3],
      if (has_precomputed && city_code %in% names(rr_surfaces)) "precomputed" else "on-the-fly"
    )
    
    # ================================================================
    # STEP 4: Baseline RR
    # ================================================================
    incProgress(1, detail = "Computing baseline RR...")
    
    # Helper: fast temp → index
    temp_to_idx <- function(t) {
      pmax(1, pmin(n_temp, round((t - varbound[1]) / 0.1) + 1))
    }
    
    bl_hist <- proj_data[ssp == "hist" & year %in% bl_period]
    bl_proj <- proj_data[ssp %in% c("1","2","3") & year %in% bl_period & year > 2014]
    
    bl_temps <- c(unlist(bl_hist[, ..gcm_cols], use.names = FALSE),
                  unlist(bl_proj[, ..gcm_cols], use.names = FALSE))
    bl_doys  <- c(rep(bl_hist$doy, length(gcm_cols)),
                  rep(bl_proj$doy, length(gcm_cols)))
    valid_bl <- !is.na(bl_temps)
    bl_temps <- bl_temps[valid_bl]
    bl_doys  <- bl_doys[valid_bl]
    
    rv$log_step4 <- sprintf(
      "Baseline period: %s\nPooled temperatures: %s\nMean: %.2f °C\nRange: %.1f to %.1f °C",
      bl_label,
      format(length(bl_temps), big.mark = ","),
      mean(bl_temps), min(bl_temps), max(bl_temps)
    )
    
    # Store histogram bins instead of raw data to save memory
    bl_hist_data <- hist(bl_temps, breaks = 80, plot = FALSE)
    rv$baseline_hist <- bl_hist_data
    
    # Track global x range for synced axes (will be updated after Step 5)
    shared_xmin <- min(bl_temps)
    shared_xmax <- max(bl_temps)
    
    # Compute baseline average RR per age
    compute_avg_rr <- function(temps, doys, comp, rr_sa = rr_single_age,
                               mmt_sa = mmt_single_age) {
      idx <- temp_to_idx(temps)
      rr_vals <- rr_sa[idx, , drop = FALSE]
      
      if (comp != "total") {
        for (j in seq_along(age_range)) {
          mmt_j <- mmt_sa[j]
          if (comp == "heat") {
            rr_vals[temps <= mmt_j, j] <- 1
          } else {
            rr_vals[temps > mmt_j, j]  <- 1
          }
        }
      }
      colMeans(rr_vals)
    }
    
    rr_baseline <- compute_avg_rr(bl_temps, bl_doys, component)
    
    # --- Per-GCM baseline RR (for temperature uncertainty) ---
    bl_data <- rbind(proj_data[ssp == "hist" & year %in% bl_period],
                     proj_data[ssp %in% c("1","2","3") & year %in% bl_period & year > 2014])
    bl_rr_by_gcm <- list()
    for (gc in gcm_cols) {
      gt <- bl_data[[gc]]
      gd <- bl_data$doy
      v  <- !is.na(gt)
      bl_rr_by_gcm[[gc]] <- compute_avg_rr(gt[v], gd[v], component)
    }
    
    # Keep pooled baseline temps for ERF sim (needed in Step 6)
    bl_all_temps <- unlist(bl_data[, ..gcm_cols], use.names = FALSE)
    bl_all_temps <- bl_all_temps[!is.na(bl_all_temps)]
    
    rm(bl_hist, bl_proj, bl_data, bl_temps, bl_doys); gc()
    
    # ================================================================
    # STEP 5: Target year temperatures
    # ================================================================
    incProgress(1, detail = "Pooling target-year temperatures...")
    
    target_temps_list <- list()
    target_doys_list  <- list()
    target_info <- character()
    
    for (yr in target_years) {
      yd <- proj_data[ssp == ssp_val & year == yr]
      temps_yr <- c(); doys_yr <- c()
      for (gc in gcm_cols) {
        temps_yr <- c(temps_yr, yd[[gc]])
        doys_yr  <- c(doys_yr, yd$doy)
      }
      v <- !is.na(temps_yr)
      target_temps_list[[as.character(yr)]] <- temps_yr[v]
      target_doys_list[[as.character(yr)]]  <- doys_yr[v]
      target_info <- c(target_info,
        sprintf("%d: %s values, mean %.2f °C",
                yr, format(sum(v), big.mark = ","), mean(temps_yr[v])))
    }
    
    # Store histogram bins per target year instead of raw temps
    rv$target_hists <- lapply(target_temps_list, function(t)
      hist(t, breaks = 60, plot = FALSE))
    rv$log_step5 <- paste(target_info, collapse = "\n")
    
    # Update shared x range to include target temps
    for (yr in names(target_temps_list)) {
      shared_xmin <- min(shared_xmin, min(target_temps_list[[yr]]))
      shared_xmax <- max(shared_xmax, max(target_temps_list[[yr]]))
    }
    rv$shared_xlim <- c(shared_xmin - 1, shared_xmax + 1)
    
    # ================================================================
    # STEP 6: Compute multipliers
    # ================================================================
    incProgress(1, detail = "Computing mortality multipliers...")
    
    results_list <- list()
    for (yr in target_years) {
      yr_c <- as.character(yr)
      rr_yr <- compute_avg_rr(target_temps_list[[yr_c]],
                              target_doys_list[[yr_c]],
                              component)
      mult <- rr_yr / rr_baseline
      
      # ----- Temperature uncertainty: per-GCM multipliers -----
      yd <- proj_data[ssp == ssp_val & year == yr]
      gcm_mults <- matrix(NA, nrow = length(gcm_cols), ncol = length(age_range))
      for (gi in seq_along(gcm_cols)) {
        gc <- gcm_cols[gi]
        gt <- yd[[gc]]; gd <- yd$doy; v <- !is.na(gt)
        if (sum(v) == 0) next
        rr_g <- compute_avg_rr(gt[v], gd[v], component)
        gcm_mults[gi, ] <- rr_g / bl_rr_by_gcm[[gc]]
      }
      # GCM variance (across 19 models)
      var_gcm <- apply(gcm_mults, 2, var, na.rm = TRUE)
      
      # ----- ERF uncertainty: delta method (analytical) -----
      var_erf <- rep(0, length(age_range))
      
      if (has_precomputed && city_code %in% names(coef_vcov) &&
          exists("cen_basis_list") && length(cen_basis_list) > 0) {
        
        vcov_city <- coef_vcov[[city_code]]
        tgt_temps <- target_temps_list[[yr_c]]
        var_mult_groups <- numeric(length(agelabs))
        
        for (ai in seq_along(agelabs)) {
          ag <- agelabs[ai]
          if (is.null(cen_basis_list[[ag]]) || is.null(vcov_city[[ag]])) next
          
          cb <- cen_basis_list[[ag]]   # n_temp × 5 centered basis
          V  <- vcov_city[[ag]]        # 5 × 5 vcov matrix
          coefs_ag <- as.numeric(coefs_all[URAU_CODE == city_code & agegroup == ag,
                                           .(b1, b2, b3, b4, b5)])
          
          compute_grad <- function(temps, comp_filt) {
            idx <- temp_to_idx(temps)
            log_rr <- cb[idx, , drop = FALSE] %*% coefs_ag
            rr_vals <- pmax(exp(as.vector(log_rr)), 1)
            if (comp_filt != "total") {
              mmt_ag <- mmt_vec[ai]
              mask <- if (comp_filt == "heat") temps > mmt_ag else temps <= mmt_ag
              rr_vals[!mask] <- 1
              cb_masked <- cb[idx, , drop = FALSE]
              cb_masked[!mask, ] <- 0
              grad <- colMeans(rr_vals * cb_masked)
            } else {
              grad <- colMeans(rr_vals * cb[idx, , drop = FALSE])
            }
            list(avg_rr = mean(rr_vals), grad = grad)
          }
          
          tgt_res <- compute_grad(tgt_temps, component)
          bl_res  <- compute_grad(bl_all_temps, component)
          M <- tgt_res$avg_rr / bl_res$avg_rr
          grad_M <- tgt_res$grad / bl_res$avg_rr - M * bl_res$grad / bl_res$avg_rr
          var_mult_groups[ai] <- max(as.numeric(t(grad_M) %*% V %*% grad_M), 0)
        }
        
        var_erf <- approx(age_midpoints, var_mult_groups,
                          xout = age_range, rule = 2)$y
      }
      
      # ----- Combined uncertainty: GCM + ERF variances summed -----
      se_total <- sqrt(var_gcm + var_erf)
      ci_lo <- mult - 1.96 * se_total
      ci_hi <- mult + 1.96 * se_total
      
      results_list[[length(results_list) + 1]] <- data.table(
        year       = yr,
        age        = age_range,
        avg_rr     = rr_yr,
        baseline_rr = rr_baseline,
        multiplier = mult,
        ci_lo      = ci_lo,
        ci_hi      = ci_hi
      )
    }
    rv$multipliers <- rbindlist(results_list)
    
    # Free the large proj_data table — everything is computed
    rm(proj_data, target_temps_list, target_doys_list, bl_all_temps); gc()
    
    incProgress(1, detail = "Done!")
    rv$ran <- TRUE
    
    }) # end withProgress
  })
  
  # ------ OUTPUTS ------
  
  # Step 1 log
  output$step1_log <- renderText({ rv$log_step1 })
  
  # Step 1 histogram
  output$step1_temp_hist <- renderPlotly({
    req(rv$hist_temp_bins)
    
    h <- rv$hist_temp_bins
    dt <- data.table(mid = h$mids, count = h$counts)
    bw <- diff(h$breaks[1:2])
    
    p <- ggplot(dt, aes(x = mid, y = count)) +
      geom_col(fill = "#3c8dbc", alpha = 0.7, width = bw * 0.95) +
      labs(x = "Temperature (°C)", y = "Count",
           title = "Historical daily temperature distribution",
           subtitle = "All GCMs pooled") +
      theme_minimal(base_size = 11)
    ggplotly(p, tooltip = c("x", "y")) %>% layout(margin = list(t = 50))
  })
  
  # Step 2 RR curves
  output$step2_rr_curves <- renderPlotly({
    req(rv$rr_matrix, rv$temp_seq, rv$mmt_vec)
    
    ts <- rv$temp_seq
    rr_dt <- rbindlist(lapply(seq_along(agelabs), function(i) {
      data.table(temp = ts, rr = rv$rr_matrix[, i], age_group = agelabs[i])
    }))
    
    p <- ggplot(rr_dt, aes(x = temp, y = rr, color = age_group)) +
      geom_line(linewidth = 0.6) +
      geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.4) +
      scale_color_viridis_d(name = "Age group") +
      labs(x = "Temperature (°C)", y = "Relative Risk (RR)",
           title = "Exposure–Response Curves by Age Group") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom")
    ggplotly(p) %>% layout(margin = list(t = 50))
  })
  
  # Step 3 MMT table
  output$step3_mmt_table <- renderDT({
    req(rv$mmt_vec)
    mmt_dt <- data.table(
      `Age Group` = agelabs,
      `Midpoint`  = age_midpoints,
      `MMT (°C)`  = round(rv$mmt_vec, 1)
    )
    datatable(mmt_dt, rownames = FALSE,
              options = list(dom = "t", pageLength = 5,
                             columnDefs = list(list(className = "dt-center",
                                                    targets = "_all"))))
  })
  output$step3_log <- renderText({ rv$log_step3 })
  
  # Step 4 baseline histogram
  output$step4_baseline_hist <- renderPlotly({
    req(rv$baseline_hist, rv$shared_xlim)
    h <- rv$baseline_hist
    dt <- data.table(mid = h$mids, count = h$counts)
    bw <- diff(h$breaks[1:2])
    
    p <- ggplot(dt, aes(x = mid, y = count)) +
      geom_col(fill = "#e6550d", alpha = 0.75, width = bw * 0.95) +
      coord_cartesian(xlim = rv$shared_xlim) +
      labs(x = "Temperature (°C)", y = "Count",
           title = "Baseline temperature distribution") +
      theme_minimal(base_size = 11)
    ggplotly(p, tooltip = c("x", "y")) %>% layout(margin = list(t = 50))
  })
  output$step4_log <- renderText({ rv$log_step4 })
  
  # Step 5 target histograms (overlaid)
  output$step5_target_hist <- renderPlotly({
    req(rv$target_hists, rv$shared_xlim)
    
    dt_list <- lapply(names(rv$target_hists), function(yr) {
      h <- rv$target_hists[[yr]]
      data.table(mid = h$mids, count = h$counts, year = yr)
    })
    dt <- rbindlist(dt_list)
    bw <- diff(rv$target_hists[[1]]$breaks[1:2])
    
    p <- ggplot(dt, aes(x = mid, y = count, fill = year)) +
      geom_col(alpha = 0.5, position = "identity", width = bw * 0.95) +
      coord_cartesian(xlim = rv$shared_xlim) +
      scale_fill_brewer(palette = "Set1", name = "Year") +
      labs(x = "Temperature (°C)", y = "Count",
           title = "Target year temperature distributions") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom")
    ggplotly(p) %>% layout(margin = list(t = 50))
  })
  output$step5_log <- renderText({ rv$log_step5 })
  
  # Step 6 multiplier plot with uncertainty band
  output$step6_multiplier_plot <- renderPlotly({
    req(rv$multipliers)
    
    dt <- copy(rv$multipliers)
    dt[, year := factor(year)]
    
    p <- ggplot(dt, aes(x = age, y = multiplier, color = year, fill = year)) +
      geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
      geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
                  alpha = 0.15, linewidth = 0, color = NA,
                  show.legend = FALSE) +
      geom_line(linewidth = 0.8) +
      scale_color_brewer(palette = "Dark2", name = "Year") +
      scale_fill_brewer(palette = "Dark2", name = "Year") +
      scale_x_continuous(breaks = seq(20, 100, 10)) +
      labs(x = "Age", y = "Mortality Multiplier",
           title = "Mortality Multiplier by Single-Year Age",
           subtitle = "Band: 95% CI (GCM spread + ERF uncertainty, delta method)") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(margin = list(t = 60))
  })
  
  # Step 6 table: key ages with uncertainty intervals
  output$step6_multiplier_table <- renderDT({
    req(rv$multipliers)
    summary_ages <- c(20, 30, 40, 50, 60, 65, 70, 75, 80, 85, 90, 95, 100)
    dt <- rv$multipliers[age %in% summary_ages]
    
    # Format multiplier with 95% CI
    dt[, mult_fmt := sprintf("%.4f [%.4f, %.4f]", multiplier, ci_lo, ci_hi)]
    dt_wide <- dcast(dt, age ~ year, value.var = "mult_fmt")
    setnames(dt_wide, "age", "Age")
    
    yr_cols <- names(dt_wide)[names(dt_wide) != "Age"]
    for (yc in yr_cols) setnames(dt_wide, yc, paste0(yc, " [95% CI]"))
    
    datatable(dt_wide, rownames = FALSE,
              options = list(dom = "t", pageLength = 15,
                             columnDefs = list(list(className = "dt-center",
                                                    targets = "_all"))))
  })
}

# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------
shinyApp(ui, server)
