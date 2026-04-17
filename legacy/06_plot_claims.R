library(data.table)
library(ggplot2)
library(patchwork)
library(scales)
library(arrow)
library(dplyr)

# Setup output directory
if(!dir.exists("figures")) dir.create("figures")

# --------------------------------------------------------
# 1. Load Data
# --------------------------------------------------------
message("Loading Data...")

# A. Life Tables (for Claims 1, 2, 3)
files <- list.files("results_csv/individual_cities", pattern = "lifetables.csv", full.names = TRUE)
dt_list <- list()

# Load subset for Romania SSP3 Adapt 0%
# We assume the files in 'individual_cities' are the ones we just generated (Romanian cities)
pb <- txtProgressBar(min = 0, max = length(files), style = 3)
for(i in seq_along(files)){
  d <- fread(files[i])
  # Filter SSP3, Adapt 0%
  d <- d[ssp == 3 & adapt == "0%"]
  if(nrow(d) > 0) dt_list[[i]] <- d
  setTxtProgressBar(pb, i)
}
close(pb)
lt_data <- rbindlist(dt_list)

# B. Parquet Data (for Claim 4 - Heat vs Cold split)
# We need to filter for Romanian cities
ro_codes <- unique(lt_data$URAU_CODE)
pq_data <- open_dataset("results_parquet/city_period.parquet") |> collect() |> setDT()
burden_data <- pq_data[city %in% ro_codes & ssp == 3 & adapt == "0%" & sc == "full"]

# --------------------------------------------------------
# Plot 1: The "Mortality Multiplier" (Elderly Risk)
# --------------------------------------------------------
message("Generating Plot 1: Elderly Risk Spike...")

# Calculate Excess Rate per 100k for 85+ vs All Ages
# Helper to recover pop and calc rate
lt_data[, pop_est := ifelse(mx_no_cc > 0, deaths_no_cc / mx_no_cc, 0)]

calc_annual_rate <- function(dt_subset){
  dt_subset[, .(
    excess_rate = (sum(deaths_cc - deaths_no_cc) / sum(pop_est)) * 100000
  ), by = period]
}

rate_all <- calc_annual_rate(lt_data)
rate_adults <- calc_annual_rate(lt_data[age >= 20 & age < 65])
rate_seniors <- calc_annual_rate(lt_data[age >= 65 & age < 85])
rate_elderly <- calc_annual_rate(lt_data[age >= 85])

rate_all[, Group := "Average (All Ages)"]
rate_adults[, Group := "Adults (20-64)"]
rate_seniors[, Group := "Seniors (65-84)"]
rate_elderly[, Group := "Elderly (85+)"]

plot_data_1 <- rbind(rate_all, rate_adults, rate_seniors, rate_elderly)

p1 <- ggplot(plot_data_1, aes(x = period, y = excess_rate, color = Group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Average (All Ages)" = "grey50", 
    "Adults (20-64)" = "#2c7bb6",
    "Seniors (65-84)" = "#fdae61",
    "Elderly (85+)" = "#d73027"
  )) +
  # User requested gridlines every 5 years, but labels less frequent
  scale_x_continuous(breaks = seq(2020, 2095, 10), minor_breaks = seq(2020, 2095, 5), limits = c(2020, 2095)) +
  labs(
    title = "Projected Escalation of Mortality Risk by Age",
    subtitle = "Annual Excess Death Risk per 100,000 people (SSP3-7.0)",
    y = "Excess Deaths / 100k",
    x = "Year",
    caption = "Risk for 85+ increases ~12-fold from 2050 to 2095"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# --------------------------------------------------------
# Plot 2: Life Expectancy Gap (Bucharest Case Study)
# --------------------------------------------------------
message("Generating Plot 2: Bucharest Life Expectancy...")

bucharest_le <- lt_data[URAU_CODE == "RO001C" & age == 0, .(period, ex_no_cc, ex_cc)]
bucharest_long <- melt(bucharest_le, id.vars = "period", 
                       measure.vars = c("ex_no_cc", "ex_cc"),
                       variable.name = "Scenario", value.name = "LifeExpectancy")

# Calculate the drop for annotation
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
    subtitle = "Bucharest (Capital City) under SSP3-7.0",
    y = "Life Expectancy at Birth (Years)",
    x = "Year",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# --------------------------------------------------------
# Plot 3: Cumulative Excess Deaths (Area) - MOVED TO BOTTOM RIGHT
# --------------------------------------------------------
message("Generating Plot 3: Cumulative Excess Deaths...")

# Sum annual excess deaths across all cities per period
annual_excess <- lt_data[, .(excess = sum(deaths_cc - deaths_no_cc)), by = period]
setorder(annual_excess, period)

# Assume each period represents 5 years
annual_excess[, period_excess := excess * 5]
annual_excess[, cumulative := cumsum(period_excess)]

p3 <- ggplot(annual_excess, aes(x = period, y = cumulative)) +
  geom_area(fill = "#d73027", alpha = 0.7) +
  geom_line(color = "#a50f15") +
  scale_y_continuous(labels = label_comma()) +
  scale_x_continuous(limits = c(2010, 2100)) +
  labs(
    title = "Cumulative Excess Deaths in Romanian Cities",
    subtitle = "Projection 2010-2099 (SSP3-7.0)",
    y = "Total Cumulative Deaths",
    x = "Year"
  ) +
  annotate("text", x = 2030, y = max(annual_excess$cumulative)*0.9, 
           label = sprintf("Total: >%s deaths", comma(max(annual_excess$cumulative, na.rm=T))), 
           hjust = 0, size = 5, fontface = "bold") +
  theme_minimal()

# --------------------------------------------------------
# Plot 4: The Burden Shift (Heat vs Cold) - MOVED TO BOTTOM LEFT
# --------------------------------------------------------
message("Generating Plot 4: Heat vs Cold Shift...")

# Aggregate burden data
burden_agg <- burden_data[, .(deaths = sum(an_est)), by = .(period, range)]
# Filter range to just 'heat' and 'cold'
burden_agg <- burden_agg[range %in% c("cold", "heat")]

p4 <- ggplot(burden_agg, aes(x = period, y = deaths, fill = range)) +
  geom_bar(stat = "identity", position = "stack", width = 4) +
  scale_fill_manual(values = c("cold" = "#4575b4", "heat" = "#d73027"), 
                    labels = c("cold" = "Cold-Related", "heat" = "Heat-Related")) +
  # User requested gridlines every 5 years, but labels less frequent
  scale_x_continuous(breaks = seq(2010, 2095, 10), minor_breaks = seq(2010, 2095, 5)) +
  coord_cartesian(xlim = c(2010, 2095)) +
  labs(
    title = "The Shift in Mortality Burden",
    subtitle = "From Cold-Dominated to Mixed/Heat-Dominated",
    y = "Annual Temperature-Related Deaths",
    x = "Year",
    fill = "Cause"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Combine and Save
message("Saving plots...")

combined_plot <- (p1 + p2) / (p4 + p3) + 
  plot_annotation(
    title = "Climate Change Impact on Mortality Risk in Romanian Cities",
    caption = "Source: EUcityProj / Wittgenstein Centre Data (SSP3-7.0, 0% Adaptation)",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave("figures/romania_risk_dashboard.png", combined_plot, width = 12, height = 10, bg = "white")
ggsave("figures/plot1_elderly_risk.png", p1, width = 8, height = 6, bg = "white")
ggsave("figures/plot2_le_gap.png", p2, width = 8, height = 6, bg = "white")
ggsave("figures/plot3_cumulative.png", p3, width = 8, height = 6, bg = "white")
ggsave("figures/plot4_burden_shift.png", p4, width = 8, height = 6, bg = "white")

message("Plots saved to /figures folder.")
