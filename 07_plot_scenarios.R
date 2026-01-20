library(data.table)
library(ggplot2)
library(patchwork)
library(scales)
library(arrow)
library(dplyr)

# Setup output directory
if(!dir.exists("figures/scenarios")) dir.create("figures/scenarios", recursive = TRUE)

# --------------------------------------------------------
# 1. Load Data
# --------------------------------------------------------
message("Loading Data...")
files <- list.files("results_csv/individual_cities", pattern = "lifetables.csv", full.names = TRUE)
dt_list <- list()

# Load all data, but filter for Adapt 0% (we compare SSPs, not adaptation strategies for now)
pb <- txtProgressBar(min = 0, max = length(files), style = 3)
for(i in seq_along(files)){
  d <- fread(files[i])
  # Keep all SSPs, but filter Adapt 0%
  d <- d[adapt == "0%"]
  if(nrow(d) > 0) dt_list[[i]] <- d
  setTxtProgressBar(pb, i)
}
close(pb)
lt_data <- rbindlist(dt_list)

# Load Parquet for Burden Data (Heat/Cold)
ro_codes <- unique(lt_data$URAU_CODE)
pq_data <- open_dataset("results_parquet/city_period.parquet") |> collect() |> setDT()
# Filter for RO cities, Adapt 0%, Full scenario
burden_data_all <- pq_data[city %in% ro_codes & adapt == "0%" & sc == "full"]

# --------------------------------------------------------
# Compute consistent Y-axis ranges across all SSPs for:
# - excess death rates (per 100k)
# - life expectancy at birth (Bucharest)
# - cumulative total deaths
# - burden shift (annual cold/heat deaths)
# These will be applied to each SSP plot to ensure identical y-axes.
# --------------------------------------------------------
expand_limits <- function(minv, maxv){
  if(is.na(minv) || is.na(maxv)) return(NULL)
  if(maxv - minv < 1e-8){
    minv <- minv - 0.5
    maxv <- maxv + 0.5
  }
  range <- maxv - minv
  minv <- minv - 0.05 * range
  maxv <- maxv + 0.05 * range
  return(c(minv, maxv))
}

# Prepare pop_est needed for rate calculations
if("mx_no_cc" %in% names(lt_data) && "deaths_no_cc" %in% names(lt_data)){
  lt_data[, pop_est := ifelse(mx_no_cc > 0, deaths_no_cc / mx_no_cc, 0)]
} else {
  lt_data[, pop_est := NA_real_]
}

# Excess rate (all ages + elderly)
ex_all <- NULL
if(!all(is.na(lt_data$pop_est))){
  ex_all <- lt_data[, .(excess_rate = (sum(deaths_cc - deaths_no_cc, na.rm=TRUE) / sum(pop_est, na.rm=TRUE)) * 100000), by = .(period, ssp)]
  ex_elder <- lt_data[age >= 85, .(excess_rate = (sum(deaths_cc - deaths_no_cc, na.rm=TRUE) / sum(pop_est, na.rm=TRUE)) * 100000), by = .(period, ssp)]
  ex_combo <- rbind(ex_all, ex_elder)
  if(nrow(ex_combo) > 0){
    excess_y_min <- min(ex_combo$excess_rate, na.rm = TRUE)
    excess_y_max <- max(ex_combo$excess_rate, na.rm = TRUE)
    excess_y_limits <- expand_limits(excess_y_min, excess_y_max)
  } else {
    excess_y_limits <- NULL
  }
} else {
  excess_y_limits <- NULL
}

# Life expectancy at birth for Bucharest (age 0)
if(all(c("URAU_CODE", "age", "ex_no_cc", "ex_cc") %in% names(lt_data))){
  le_buc <- lt_data[URAU_CODE == "RO001C" & age == 0, .(ex_no_cc, ex_cc, ssp, period)]
  if(nrow(le_buc) > 0){
    le_vals <- c(le_buc$ex_no_cc, le_buc$ex_cc)
    le_y_limits <- expand_limits(min(le_vals, na.rm = TRUE), max(le_vals, na.rm = TRUE))
  } else {
    le_y_limits <- NULL
  }
} else {
  le_y_limits <- NULL
}

# Cumulative total deaths
if(all(c("deaths_cc", "deaths_no_cc") %in% names(lt_data))){
  annual_excess_all <- lt_data[, .(excess = sum(deaths_cc - deaths_no_cc, na.rm=TRUE)), by = .(ssp, period)]
  setorder(annual_excess_all, ssp, period)
  annual_excess_all[, period_excess := excess * 5]
  annual_excess_all[, cumulative := cumsum(period_excess), by = ssp]
  if(nrow(annual_excess_all) > 0){
    cum_y_limits <- expand_limits(min(annual_excess_all$cumulative, na.rm = TRUE), max(annual_excess_all$cumulative, na.rm = TRUE))
  } else {
    cum_y_limits <- NULL
  }
} else {
  cum_y_limits <- NULL
}

# Burden shift limits (cold/heat annual deaths)
if(all(c("period", "range", "an_est") %in% names(burden_data_all))){
  burden_agg_all <- burden_data_all[, .(deaths = sum(an_est, na.rm=TRUE)), by = .(period, range, ssp)]
  if(nrow(burden_agg_all) > 0){
    burden_y_limits <- expand_limits(min(burden_agg_all$deaths, na.rm = TRUE), max(burden_agg_all$deaths, na.rm = TRUE))
  } else {
    burden_y_limits <- NULL
  }
} else {
  burden_y_limits <- NULL
}

# --------------------------------------------------------
# Function to Generate Dashboard for a Specific SSP
# --------------------------------------------------------
generate_dashboard_for_ssp <- function(target_ssp) {
  
  message(sprintf("Generating Dashboard for SSP%d...", target_ssp))
  
  # Filter Data for this SSP
  ssp_lt_data <- lt_data[ssp == target_ssp]
  ssp_burden_data <- burden_data_all[ssp == target_ssp]
  
  if(nrow(ssp_lt_data) == 0) {
    message("No data for SSP", target_ssp)
    return(NULL)
  }
  
  # --- Plot 1: Elderly Risk ---
  ssp_lt_data[, pop_est := ifelse(mx_no_cc > 0, deaths_no_cc / mx_no_cc, 0)]
  
  calc_annual_rate <- function(dt_subset){
    dt_subset[, .(
      excess_rate = (sum(deaths_cc - deaths_no_cc) / sum(pop_est)) * 100000
    ), by = period]
  }
  
  rate_all <- calc_annual_rate(ssp_lt_data)
  rate_elderly <- calc_annual_rate(ssp_lt_data[age >= 85])
  
  rate_all[, Group := "Average (All Ages)"]
  rate_elderly[, Group := "Elderly (85+)"]
  plot_data_1 <- rbind(rate_all, rate_elderly)
  
  p1 <- ggplot(plot_data_1, aes(x = period, y = excess_rate, color = Group)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = c("Average (All Ages)" = "grey50", "Elderly (85+)" = "#d73027")) +
    scale_x_continuous(breaks = seq(2020, 2095, 10), minor_breaks = seq(2020, 2095, 5), limits = c(2020, 2095)) +
    labs(
      title = "Projected Escalation of Elderly Mortality Risk",
      subtitle = sprintf("Annual Excess Death Risk per 100,000 people (SSP%d-7.0)", target_ssp),
      y = "Excess Deaths / 100k",
      x = "Year"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  # Apply global excess rate limits if available
  if(exists("excess_y_limits") && !is.null(excess_y_limits)){
    p1 <- p1 + coord_cartesian(ylim = excess_y_limits)
  }
  
  # --- Plot 2: Bucharest LE ---
  bucharest_le <- ssp_lt_data[URAU_CODE == "RO001C" & age == 0, .(period, ex_no_cc, ex_cc)]
  drop_2095 <- bucharest_le[period == 2095, ex_no_cc - ex_cc]
  drop_months <- drop_2095 * 12
  
  p2 <- ggplot(bucharest_le, aes(x = period)) +
    geom_ribbon(aes(ymin = ex_cc, ymax = ex_no_cc), fill = "#fee0d2", alpha = 0.5) +
    geom_line(aes(y = ex_no_cc, color = "Baseline (No Climate Change)"), linewidth = 1) +
    geom_line(aes(y = ex_cc, color = "With Climate Change"), linewidth = 1, linetype = "dashed") +
    annotate("text", x = 2080, y = mean(c(bucharest_le[period==2095]$ex_cc, bucharest_le[period==2095]$ex_no_cc)), 
             label = sprintf("-%.1f months\nstructural loss", drop_months), 
             color = "#d73027", fontface = "bold", hjust = 0) +
    scale_color_manual(values = c("Baseline (No Climate Change)" = "steelblue", "With Climate Change" = "#d73027")) +
    scale_x_continuous(limits = c(2010, 2100)) +
    labs(
      title = "Structural Degradation of Life Expectancy",
      subtitle = sprintf("Bucharest (Capital City) under SSP%d", target_ssp),
      y = "Life Expectancy at Birth (Years)",
      x = "Year",
      color = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  # Apply global life-expectancy limits if available
  if(exists("le_y_limits") && !is.null(le_y_limits)){
    p2 <- p2 + coord_cartesian(ylim = le_y_limits)
  }
  
  # --- Plot 3: Cumulative Excess ---
  annual_excess <- ssp_lt_data[, .(excess = sum(deaths_cc - deaths_no_cc)), by = period]
  setorder(annual_excess, period)
  annual_excess[, period_excess := excess * 5]
  annual_excess[, cumulative := cumsum(period_excess)]
  
  p3 <- ggplot(annual_excess, aes(x = period, y = cumulative)) +
    geom_area(fill = "#d73027", alpha = 0.7) +
    geom_line(color = "#a50f15") +
    scale_y_continuous(labels = label_comma()) +
    scale_x_continuous(limits = c(2010, 2100)) +
    labs(
      title = "Cumulative Excess Deaths in Romanian Cities",
      subtitle = sprintf("Projection 2010-2099 (SSP%d)", target_ssp),
      y = "Total Cumulative Deaths",
      x = "Year"
    ) +
    annotate("text", x = 2030, y = max(annual_excess$cumulative)*0.9, 
             label = sprintf("Total: >%s deaths", comma(max(annual_excess$cumulative, na.rm=T))), 
             hjust = 0, size = 5, fontface = "bold") +
    theme_minimal()
  # Apply global cumulative-deaths limits if available
  if(exists("cum_y_limits") && !is.null(cum_y_limits)){
    p3 <- p3 + coord_cartesian(ylim = cum_y_limits)
  }
  
  # --- Plot 4: Burden Shift ---
  burden_agg <- ssp_burden_data[, .(deaths = sum(an_est)), by = .(period, range)]
  burden_agg <- burden_agg[range %in% c("cold", "heat")]
  
  p4 <- ggplot(burden_agg, aes(x = period, y = deaths, fill = range)) +
    geom_bar(stat = "identity", position = "stack", width = 4) +
    scale_fill_manual(values = c("cold" = "#4575b4", "heat" = "#d73027"), 
                      labels = c("cold" = "Cold-Related", "heat" = "Heat-Related")) +
    scale_x_continuous(breaks = seq(2010, 2095, 10), minor_breaks = seq(2010, 2095, 5)) +
    coord_cartesian(xlim = c(2010, 2095)) +
    labs(
      title = "The Shift in Mortality Burden",
      subtitle = sprintf("SSP%d Scenario", target_ssp),
      y = "Annual Temperature-Related Deaths",
      x = "Year",
      fill = "Cause"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  # Apply global burden limits if available
  if(exists("burden_y_limits") && !is.null(burden_y_limits)){
    p4 <- p4 + coord_cartesian(ylim = burden_y_limits)
  }
  
  # --- Combine and Save ---
  combined_plot <- (p1 + p2) / (p4 + p3) + 
    plot_annotation(
      title = sprintf("Climate Change Impact on Mortality Risk (SSP%d)", target_ssp),
      caption = sprintf("Source: EUcityProj / Wittgenstein Centre Data (SSP%d, 0%% Adaptation)", target_ssp),
      theme = theme(plot.title = element_text(size = 16, face = "bold"))
    )
  
  ggsave(sprintf("figures/scenarios/dashboard_SSP%d.png", target_ssp), combined_plot, width = 12, height = 10, bg = "white")
}

# --------------------------------------------------------
# Execute Loop for All Available SSPs
# --------------------------------------------------------
ssp_list <- unique(lt_data$ssp)
message("Found SSPs: ", paste(ssp_list, collapse = ", "))

for(ssp_val in ssp_list) {
  generate_dashboard_for_ssp(ssp_val)
}

# --------------------------------------------------------
# BONUS: Comparison Chart (Elderly Risk across SSPs)
# --------------------------------------------------------
message("Generating Comparison Chart...")

comp_lt <- lt_data[age >= 85]
comp_lt[, pop_est := ifelse(mx_no_cc > 0, deaths_no_cc / mx_no_cc, 0)]
comp_rate <- comp_lt[, .(
    excess_rate = (sum(deaths_cc - deaths_no_cc) / sum(pop_est)) * 100000
), by = .(period, ssp)]

p_comp <- ggplot(comp_rate, aes(x = period, y = excess_rate, color = factor(ssp))) +
  geom_line(linewidth = 1.2) +
  scale_color_brewer(palette = "Dark2", name = "SSP Scenario", labels = function(x) paste0("SSP", x)) +
  scale_x_continuous(breaks = seq(2020, 2095, 10)) +
  labs(
    title = "Comparison of Elderly Mortality Risk by Scenario",
    subtitle = "Excess Death Risk per 100,000 people (Ages 85+)",
    y = "Excess Deaths / 100k",
    x = "Year"
  ) +
  theme_minimal()

ggsave("figures/scenarios/comparison_elderly_risk.png", p_comp, width = 10, height = 6, bg = "white")

message("All scenarios generated in figures/scenarios/")
