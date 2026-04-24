#!/usr/bin/env Rscript
library(data.table)
library(ggplot2)
library(scales)
library(arrow)
library(dplyr)

if(!dir.exists("figures/scenarios")) dir.create("figures/scenarios", recursive = TRUE)

message("Loading data for overlap plots...")
# Load individual city lifetables if available
files <- list.files("results_csv/individual_cities", pattern = "lifetables.csv", full.names = TRUE)
if(length(files) > 0){
  dt_list <- list()
  for(i in seq_along(files)){
    d <- fread(files[i])
    d <- d[adapt == "0%"]
    if(nrow(d) > 0) dt_list[[i]] <- d
  }
  lt_data <- rbindlist(dt_list)
} else {
  # fallback: build synthetic lt_data from city_period.csv
  cp <- fread("results_csv/city_period.csv")
  setDT(cp)
  cp <- cp[sc == "full" & adapt == "0%" & range == "tot"]
  cp[, total_deaths := fifelse(af_est > 0, an_est / af_est, NA_real_)]
  cp[, pop_est := fifelse(!is.na(total_deaths) & rate_est > 0, total_deaths / rate_est, NA_real_)]
  cp[, deaths_cc := total_deaths]
  cp[, deaths_no_cc := fifelse(!is.na(total_deaths), total_deaths - an_est, NA_real_)]
  cp[, mx_no_cc := fifelse(!is.na(deaths_no_cc) & !is.na(pop_est) & pop_est > 0, deaths_no_cc / pop_est, NA_real_)]
  cp[, age := as.integer(sub('^(\\d+).*', '\\1', agegroup))]
  cp[, URAU_CODE := city]
  cp[, ssp := as.integer(ssp)]
  cp[, period := as.integer(period)]
  lt_data <- cp[, .(URAU_CODE, ssp, period, adapt, age, deaths_cc, deaths_no_cc, mx_no_cc, an_est, af_est, rate_est)]
}

# burden data
if(file.exists("results_parquet/city_period.parquet")){
  pq_data <- open_dataset("results_parquet/city_period.parquet") |> collect() |> setDT()
} else {
  pq_data <- fread("results_csv/city_period.csv") |> setDT()
}
ro_codes <- unique(lt_data$URAU_CODE)
burden_data_all <- pq_data[city %in% ro_codes & adapt == "0%" & sc == "full"]

# Helper: expand limits
expand_limits <- function(minv, maxv){
  if(is.na(minv) || is.na(maxv)) return(NULL)
  if(maxv - minv < 1e-8){ minv <- minv - 0.5; maxv <- maxv + 0.5 }
  r <- maxv - minv
  c(minv - 0.05 * r, maxv + 0.05 * r)
}

# Compute overall limits (reuse logic from main script)
if("mx_no_cc" %in% names(lt_data) && "deaths_no_cc" %in% names(lt_data)){
  lt_data[, pop_est := ifelse(mx_no_cc > 0, deaths_no_cc / mx_no_cc, NA_real_)]
} else {
  lt_data[, pop_est := NA_real_]
}

ex_combo <- NULL
if(!all(is.na(lt_data$pop_est))){
  ex_all <- lt_data[, .(excess_rate = (sum(deaths_cc - deaths_no_cc, na.rm=TRUE) / sum(pop_est, na.rm=TRUE)) * 100000), by = .(period, ssp)]
  ex_elder <- lt_data[age >= 85, .(excess_rate = (sum(deaths_cc - deaths_no_cc, na.rm=TRUE) / sum(pop_est, na.rm=TRUE)) * 100000), by = .(period, ssp)]
  ex_combo <- rbind(ex_all, ex_elder)
}
excess_y_limits <- if(!is.null(ex_combo) && nrow(ex_combo)>0) expand_limits(min(ex_combo$excess_rate, na.rm=TRUE), max(ex_combo$excess_rate, na.rm=TRUE)) else NULL

le_y_limits <- NULL
if(all(c("URAU_CODE","age","ex_no_cc","ex_cc") %in% names(lt_data))){
  le_buc <- lt_data[URAU_CODE == "RO001C" & age == 0, .(ex_no_cc, ex_cc, ssp, period)]
  if(nrow(le_buc) > 0){ le_vals <- c(le_buc$ex_no_cc, le_buc$ex_cc); le_y_limits <- expand_limits(min(le_vals, na.rm=TRUE), max(le_vals, na.rm=TRUE)) }
}

cum_limits <- NULL
if(all(c("deaths_cc","deaths_no_cc") %in% names(lt_data))){
  annual_excess_all <- lt_data[, .(excess = sum(deaths_cc - deaths_no_cc, na.rm=TRUE)), by = .(ssp, period)]
  setorder(annual_excess_all, ssp, period)
  annual_excess_all[, period_excess := excess * 5]
  annual_excess_all[, cumulative := cumsum(period_excess), by = ssp]
  if(nrow(annual_excess_all)>0) cum_limits <- expand_limits(min(annual_excess_all$cumulative, na.rm=TRUE), max(annual_excess_all$cumulative, na.rm=TRUE))
}

burden_y_limits <- NULL
if(all(c("period","range","an_est") %in% names(burden_data_all))){
  bd_all <- burden_data_all[, .(deaths = sum(an_est, na.rm=TRUE)), by = .(period, range, ssp)]
  if(nrow(bd_all)>0) burden_y_limits <- expand_limits(min(bd_all$deaths, na.rm=TRUE), max(bd_all$deaths, na.rm=TRUE))
}

# Colors
col_ssp1 <- "#1b9e77" # green
col_ssp2 <- "#377eb8" # blue
col_ssp3 <- "#d73027" # red

# Helper to compute excess time series per ssp
get_excess_ts <- function(ssp_val){
  dt <- lt_data[ssp == ssp_val]
  if(nrow(dt)==0) return(NULL)
  dt[, pop_est_local := ifelse(mx_no_cc>0, deaths_no_cc / mx_no_cc, NA_real_)]
  # average (all ages)
  ts_all <- dt[, .(excess_rate = (sum(deaths_cc - deaths_no_cc, na.rm=TRUE) / sum(pop_est_local, na.rm=TRUE)) * 100000), by = period]
  ts_all[, Group := 'Average (All Ages)']
  # elderly (85+)
  dt_el <- dt[age >= 85]
  if(nrow(dt_el)>0){
    ts_el <- dt_el[, .(excess_rate = (sum(deaths_cc - deaths_no_cc, na.rm=TRUE) / sum(pop_est_local, na.rm=TRUE)) * 100000), by = period]
  } else {
    ts_el <- copy(ts_all)
    ts_el[, excess_rate := NA_real_]
  }
  ts_el[, Group := 'Elderly (85+)']
  ts <- rbind(ts_el, ts_all, use.names=TRUE, fill=TRUE)
  ts[, ssp_label := paste0('SSP', ssp_val)]
  setorder(ts, period, Group)
  ts
}

# Excess deaths plots
ssps_present <- sort(unique(lt_data$ssp))
# Ensure at least 1,2,3 in order
ssps_to_use <- intersect(c(1,2,3), ssps_present)

# 1) SSP1 only
if(1 %in% ssps_to_use){
  ts1 <- get_excess_ts(1)
  # plot elderly strong, average paler
  p <- ggplot(ts1, aes(x=period, y=excess_rate, group=Group)) +
    geom_line(data=ts1[Group=='Elderly (85+)'], aes(y=excess_rate), color=col_ssp1, linewidth=1.2) +
    geom_point(data=ts1[Group=='Elderly (85+)'], aes(y=excess_rate), color=col_ssp1, size=2) +
    geom_line(data=ts1[Group=='Average (All Ages)'], aes(y=excess_rate), color=scales::alpha(col_ssp1, 0.45), linewidth=1) +
    geom_point(data=ts1[Group=='Average (All Ages)'], aes(y=excess_rate), color=scales::alpha(col_ssp1, 0.45), size=1.5) +
    labs(title="Excess Deaths - SSP1", y="Excess Deaths / 100k", x="Year") +
    scale_x_continuous(breaks=seq(2020,2095,10)) + theme_minimal()
  if(!is.null(excess_y_limits)) p <- p + coord_cartesian(ylim = excess_y_limits)
  ggsave("figures/scenarios/excess_deaths_SSP1.png", p, width=8, height=5)
}

# 2) SSP1 + SSP2
if(all(c(1,2) %in% ssps_to_use)){
  ts1 <- get_excess_ts(1); ts2 <- get_excess_ts(2)
  ts1[, ssp_label := 'SSP1']; ts2[, ssp_label := 'SSP2']
  comb <- rbind(ts1, ts2)
  # plot elderly solid, average paler
  p <- ggplot() +
    geom_line(data=comb[Group=='Elderly (85+)'], aes(x=period, y=excess_rate, color=ssp_label), linewidth=1.2) +
    geom_point(data=comb[Group=='Elderly (85+)'], aes(x=period, y=excess_rate, color=ssp_label), size=2) +
    geom_line(data=comb[Group=='Average (All Ages)'], aes(x=period, y=excess_rate, color=ssp_label), linewidth=1) +
    geom_point(data=comb[Group=='Average (All Ages)'], aes(x=period, y=excess_rate, color=ssp_label), size=1.5) +
    scale_color_manual(values=c('SSP1'=col_ssp1, 'SSP2'=col_ssp2)) +
    labs(title="Excess Deaths - SSP1 & SSP2", y="Excess Deaths / 100k", x="Year") +
    scale_x_continuous(breaks=seq(2020,2095,10)) + theme_minimal()
  if(!is.null(excess_y_limits)) p <- p + coord_cartesian(ylim = excess_y_limits)
  ggsave("figures/scenarios/excess_deaths_SSP2.png", p, width=8, height=5)
}

# 3) SSP1 + SSP2 + SSP3
if(all(c(1,2,3) %in% ssps_to_use)){
  ts1 <- get_excess_ts(1); ts2 <- get_excess_ts(2); ts3 <- get_excess_ts(3)
  ts1[, ssp_label := 'SSP1']; ts2[, ssp_label := 'SSP2']; ts3[, ssp_label := 'SSP3']
  comb <- rbind(ts1, ts2, ts3)
  p <- ggplot() +
    geom_line(data=comb[Group=='Elderly (85+)'], aes(x=period, y=excess_rate, color=ssp_label), linewidth=1.2) +
    geom_point(data=comb[Group=='Elderly (85+)'], aes(x=period, y=excess_rate, color=ssp_label), size=2) +
    geom_line(data=comb[Group=='Average (All Ages)'], aes(x=period, y=excess_rate, color=ssp_label), linewidth=1) +
    geom_point(data=comb[Group=='Average (All Ages)'], aes(x=period, y=excess_rate, color=ssp_label), size=1.5) +
    scale_color_manual(values=c('SSP1'=col_ssp1, 'SSP2'=col_ssp2, 'SSP3'=col_ssp3)) +
    labs(title="Excess Deaths - SSP1,2,3", y="Excess Deaths / 100k", x="Year") +
    scale_x_continuous(breaks=seq(2020,2095,10)) + theme_minimal()
  if(!is.null(excess_y_limits)) p <- p + coord_cartesian(ylim = excess_y_limits)
  ggsave("figures/scenarios/excess_deaths_SSP3.png", p, width=8, height=5)
}

# Life expectancy plots (Bucharest age 0)
plot_le_for_ssps <- function(ssp_list, out_file, colors){
  layers <- list()
  for(i in seq_along(ssp_list)){
    s <- ssp_list[i]
    sub <- lt_data[ssp==s & URAU_CODE == "RO001C" & age==0]
    if(nrow(sub)==0) next
    sub[, ssp_label := paste0('SSP', s)]
    layers[[i]] <- sub
  }
  if(length(layers)==0) return()
  df <- rbindlist(layers, fill=TRUE)
  p <- ggplot()
  # ribbons
  for(i in seq_along(ssp_list)){
    s <- ssp_list[i]; col <- colors[i]
    sub <- df[ssp_label==paste0('SSP', s)]
    if(nrow(sub)==0) next
    if(all(c('ex_cc','ex_no_cc') %in% names(sub))){
      p <- p + geom_ribbon(data=sub, aes(x=period, ymin=ex_cc, ymax=ex_no_cc), fill=col, alpha=0.25)
      p <- p + geom_line(data=sub, aes(x=period, y=ex_no_cc), color=col, linewidth=1)
      p <- p + geom_line(data=sub, aes(x=period, y=ex_cc), color=col, linetype='dashed', linewidth=1)
    }
  }
  p <- p + labs(title=paste0('Life Expectancy at Birth - ', paste0('SSP', paste(ssp_list, collapse=','))), y='Life expectancy (years)', x='Year') + theme_minimal()
  if(!is.null(le_y_limits)) p <- p + coord_cartesian(ylim = le_y_limits)
  ggsave(out_file, p, width=8, height=5)
}

# SSP1 only LE
if(1 %in% ssps_to_use) plot_le_for_ssps(c(1), 'figures/scenarios/life_expectancy_SSP1.png', c(col_ssp1))
# SSP1+2
if(all(c(1,2) %in% ssps_to_use)) plot_le_for_ssps(c(1,2), 'figures/scenarios/life_expectancy_SSP2.png', c(col_ssp1, col_ssp2))
# SSP1+2+3
if(all(c(1,2,3) %in% ssps_to_use)) plot_le_for_ssps(c(1,2,3), 'figures/scenarios/life_expectancy_SSP3.png', c(col_ssp1, col_ssp2, col_ssp3))

# Burden shift: one file per SSP showing only that SSP (as requested)
for(s in c(1,2,3)){
  if(!(s %in% ssps_to_use)) next
  bd <- burden_data_all[ssp==s & range %in% c('cold','heat'), .(deaths = sum(an_est, na.rm=TRUE)), by=.(period, range)]
  if(nrow(bd)==0) next
  p <- ggplot(bd, aes(x=period, y=deaths, fill=range)) + geom_bar(stat='identity', position='stack', width=4) +
    scale_fill_manual(values=c('cold'='#4575b4','heat'='#d73027')) +
    labs(title=paste0('Shift in Mortality Burden - SSP', s), y='Annual deaths', x='Year') + theme_minimal()
  if(!is.null(burden_y_limits)) p <- p + coord_cartesian(ylim = burden_y_limits)
  ggsave(sprintf('figures/scenarios/burden_shift_SSP%d.png', s), p, width=8, height=5)
}

# Cumulative deaths: overlay similar to excess
get_cumulative_ts <- function(ssp_val){
  dt <- lt_data[ssp==ssp_val]
  if(nrow(dt)==0) return(NULL)
  ann <- dt[, .(excess = sum(deaths_cc - deaths_no_cc, na.rm=TRUE)), by=.(period)]
  setorder(ann, period)
  ann[, period_excess := excess * 5]
  ann[, cumulative := cumsum(period_excess)]
  ann
}

# SSP1 only
if(1 %in% ssps_to_use){
  c1 <- get_cumulative_ts(1)
  p <- ggplot(c1, aes(x=period, y=cumulative)) + geom_area(fill=col_ssp1, alpha=0.6) + geom_line(color=col_ssp1, linewidth=1) + labs(title='Cumulative Deaths - SSP1', y='Cumulative deaths', x='Year') + theme_minimal()
  if(!is.null(cum_limits)) p <- p + coord_cartesian(ylim = cum_limits)
  ggsave('figures/scenarios/cumulative_deaths_SSP1.png', p, width=8, height=5)
}
# SSP1+2
if(all(c(1,2) %in% ssps_to_use)){
  c1 <- get_cumulative_ts(1); c2 <- get_cumulative_ts(2)
  c1[, ssp:='SSP1']; c2[, ssp:='SSP2']
  comb <- rbind(c1, c2)
  # Use filled areas per SSP with transparency so fills are visible
  p <- ggplot() +
    geom_area(data=c1, aes(x=period, y=cumulative), fill=col_ssp1, alpha=0.35) +
    geom_line(data=c1, aes(x=period, y=cumulative), color=col_ssp1, linewidth=1) +
    geom_area(data=c2, aes(x=period, y=cumulative), fill=col_ssp2, alpha=0.25) +
    geom_line(data=c2, aes(x=period, y=cumulative), color=col_ssp2, linewidth=1) +
    labs(title='Cumulative Deaths - SSP1 & SSP2', y='Cumulative deaths', x='Year') + theme_minimal()
  if(!is.null(cum_limits)) p <- p + coord_cartesian(ylim = cum_limits)
  ggsave('figures/scenarios/cumulative_deaths_SSP2.png', p, width=8, height=5)
}
# SSP1+2+3
if(all(c(1,2,3) %in% ssps_to_use)){
  c1 <- get_cumulative_ts(1); c2 <- get_cumulative_ts(2); c3 <- get_cumulative_ts(3)
  c1[, ssp:='SSP1']; c2[, ssp:='SSP2']; c3[, ssp:='SSP3']
  comb <- rbind(c1, c2, c3)
  p <- ggplot() +
    geom_area(data=c1, aes(x=period, y=cumulative), fill=col_ssp1, alpha=0.30) + geom_line(data=c1, aes(x=period, y=cumulative), color=col_ssp1, linewidth=1) +
    geom_area(data=c2, aes(x=period, y=cumulative), fill=col_ssp2, alpha=0.25) + geom_line(data=c2, aes(x=period, y=cumulative), color=col_ssp2, linewidth=1) +
    geom_area(data=c3, aes(x=period, y=cumulative), fill=col_ssp3, alpha=0.2) + geom_line(data=c3, aes(x=period, y=cumulative), color=col_ssp3, linewidth=1) +
    labs(title='Cumulative Deaths - SSP1,2,3', y='Cumulative deaths', x='Year') + theme_minimal()
  if(!is.null(cum_limits)) p <- p + coord_cartesian(ylim = cum_limits)
  ggsave('figures/scenarios/cumulative_deaths_SSP3.png', p, width=8, height=5)
}

message('Overlap plots saved to figures/scenarios/')
