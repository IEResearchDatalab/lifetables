
# 06_vis_lifetables.R
# Visualize the impact of climate change on Life Tables

library(data.table)
library(ggplot2)
library(patchwork) # For combining plots

# Create plots directory if missing
if(!dir.exists("plots")) dir.create("plots")

# 1. Load Data
dt <- fread("results_csv/romania_city_lifetables_full.csv")

# Filter for Bucharest (RO001C) and a high emission scenario (SSP3) for dramatic effect
city_code <- "RO001C" 
target_ssp <- 3
target_adapt <- "0%" # Worst case adaptation

city_dt <- dt[URAU_CODE == city_code & ssp == target_ssp & adapt == target_adapt]
city_label <- unique(city_dt$LABEL)

message("Generating plots for: ", city_label)

# ==============================================================================
# PLOT 1: Life Expectancy Loss (e0) over Time
# ==============================================================================
# Extract e0 (Life Expectancy at birth, age 0)
e0_dt <- city_dt[age == 0]
e0_dt[, loss_years := ex_no_cc - ex_cc]
e0_dt[, loss_months := loss_years * 12]

p1 <- ggplot(e0_dt, aes(x = period, y = loss_months)) +
  geom_line(color = "#D55E00", size = 1.2) +
  geom_point(color = "#D55E00", size = 2) +
  theme_minimal() +
  labs(
    title = paste("Lost Life Expectancy due to Climate Change:", city_label),
    subtitle = paste0("Scenario: SSP", target_ssp, " | Adapt: ", target_adapt, " (Difference in months)"),
    y = "Months of Life Lost (e0)",
    x = "Year"
  ) +
  theme(plot.title = element_text(face="bold"))

ggsave("plots/01_e0_loss_over_time.png", p1, width = 8, height = 5)


# ==============================================================================
# PLOT 2: Survival Curve Shift (lx) in 2090
# ==============================================================================
target_year <- 2090
lx_dt <- city_dt[period == target_year]

# Melt for ggplot
lx_long <- melt(lx_dt, id.vars = "age", measure.vars = c("lx_no_cc", "lx_cc"), 
                variable.name = "Scenario", value.name = "Survivors")

# Rename levels
lx_long[, Scenario := factor(Scenario, 
                             levels = c("lx_no_cc", "lx_cc"), 
                             labels = c("No Climate Change", "With Climate Change"))]

p2 <- ggplot(lx_long, aes(x = age, y = Survivors, color = Scenario, linetype = Scenario)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("No Climate Change" = "#009E73", "With Climate Change" = "#D55E00")) +
  theme_minimal() +
  labs(
    title = paste("Survival Curve Shift in", target_year, "-", city_label),
    subtitle = paste("Survivors per 100,000 born | SSP", target_ssp),
    y = "Survivors (lx)",
    x = "Age"
  ) +
  coord_cartesian(xlim = c(50, 100)) + # Focus on older ages where the gap is visible
  theme(legend.position = "bottom")

ggsave("plots/02_survival_curve_2090.png", p2, width = 8, height = 5)


# ==============================================================================
# PLOT 3: Relative Mortality Risk (mx Ratio)
# ==============================================================================
# Calculate Ratio
lx_dt[, risk_ratio := mx_cc / mx_no_cc]

p3 <- ggplot(lx_dt[age >= 20], aes(x = age, y = risk_ratio)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
  geom_line(color = "#CC79A7", size = 1.2) +
  theme_minimal() +
  labs(
    title = paste("Relative Mortality Risk Increase in", target_year),
    subtitle = "Ratio of Mortality Rates (With CC / No CC)",
    y = "Risk Ratio (1.0 = No Change)",
    x = "Age"
  )

ggsave("plots/03_mortality_risk_ratio.png", p3, width = 8, height = 5)

message("Plots saved to /plots folder.")
