################################################################################
# 
# Relative Risk Curves with Confidence Intervals
# Bucharest - All Age Groups
# Style consistent with IE LaTeX template (ieblue.sty)
#
################################################################################

# Load required packages
library(data.table)
library(dplyr)
library(dlnm)
library(splines)
library(ggplot2)
library(arrow)
library(showtext)

# Register Montserrat font (consistent with IE LaTeX template)
font_add("Montserrat", 
         regular = "fonts/Montserrat-Regular.ttf",
         bold = "fonts/Montserrat-Bold.ttf",
         italic = "fonts/Montserrat-Italic.ttf",
         bolditalic = "fonts/Montserrat-BoldItalic.ttf")
showtext_auto()

#------------------------------------------------------------------------------
# IE LaTeX Template Colors
#------------------------------------------------------------------------------

ie_colors <- list(
  OceanBlue = "#000066",
  ElectricBlue = "#0000db",
  SeaBlue = "#47bfff",
  TechGreen = "#6DC201",
  TechBlack = "#000024"
)

# Color palette for age groups (using IE blues + complementary)
age_colors <- c(
  "20-44" = "#47bfff",   # SeaBlue (lightest - youngest)
  "45-64" = "#0000db",   # ElectricBlue
  "65-74" = "#000066",   # OceanBlue
  "75-84" = "#6DC201",   # TechGreen
  "85+"   = "#000024"    # TechBlack (darkest - oldest)
)

#------------------------------------------------------------------------------
# Load city configuration
#------------------------------------------------------------------------------
source("config.R")

#------------------------------------------------------------------------------
# Load data
#------------------------------------------------------------------------------

cat("Loading data...\n")

# Load coefficients and covariance matrices
coefs_all <- fread("data/coefs.csv")
coefs_city <- coefs_all[URAU_CODE == city_code]

# Load temperature distribution
tdist <- read_parquet("data/era5series.gz.parquet") |>
  subset(URAU_CODE == city_code & year(date) %between% histrange) |>
  reframe(perc = predper, tmean = quantile(era5landtmean, predper / 100, na.rm = TRUE))

varknots <- subset(tdist, perc %in% varper, tmean, drop = TRUE)
varbound <- range(tdist$tmean)
temp_range <- tdist$tmean

cat(sprintf("Temperature range: %.1f°C to %.1f°C\n", min(temp_range), max(temp_range)))

#------------------------------------------------------------------------------
# Compute RR curves with confidence intervals
#------------------------------------------------------------------------------

cat("Computing relative risk curves...\n")

# Note: Variance-covariance matrices not available in coefs.csv
# Confidence intervals would require the original model fits or meta-model
# For now, we compute point estimates only

# Arguments for basis function
argvar <- list(fun = varfun, degree = vardegree, knots = varknots, Bound = varbound)

# Temperature sequence for smooth curves
n_temp <- 200
temp_seq <- seq(min(temp_range), max(temp_range), length.out = n_temp)

# Create basis matrix
basis <- do.call(onebasis, c(list(x = temp_seq), argvar))

# Compute RR for each age group
rr_list <- list()

for (i in seq_along(agelabs)) {
  age <- agelabs[i]
  
  # Get coefficients for this age group
  coef_row <- coefs_city[agegroup == age]
  
  if (nrow(coef_row) == 0) {
    cat(sprintf("Warning: No coefficients for age group %s, skipping\n", age))
    next
  }
  
  # Extract coefficient vector
  coefs <- as.numeric(coef_row[, .(b1, b2, b3, b4, b5)])
  
  # Compute log-RR
  log_rr <- basis %*% coefs
  
  # Find MMT in 25-99 percentile range
  ind <- temp_seq >= quantile(temp_seq, 0.25) & temp_seq <= quantile(temp_seq, 0.99)
  mmt <- temp_seq[ind][which.min(log_rr[ind])]
  
  # Center at MMT
  cenvec <- do.call(onebasis, c(list(x = mmt), argvar))
  log_rr_centered <- log_rr - drop(cenvec %*% coefs)
  
  # Compute RR
  rr <- exp(log_rr_centered)
  
  rr_list[[age]] <- data.table(
    agegroup = factor(age, levels = agelabs),
    temperature = temp_seq,
    RR = as.vector(rr),
    MMT = mmt
  )
  
  cat(sprintf("  %s: MMT = %.1f°C, Max RR (cold) = %.2f, Max RR (heat) = %.2f\n", 
              age, mmt, max(rr[temp_seq < mmt]), max(rr[temp_seq > mmt])))
}

rr_data <- rbindlist(rr_list)

#------------------------------------------------------------------------------
# Create IE-styled theme
#------------------------------------------------------------------------------

theme_ie <- function(base_size = 36) {
  theme_minimal(base_size = base_size, base_family = "Montserrat") +
    theme(
      # Text colors
      text = element_text(color = ie_colors$OceanBlue, family = "Montserrat"),
      axis.text = element_text(color = ie_colors$OceanBlue, size = base_size - 8),
      axis.title = element_text(color = ie_colors$OceanBlue, face = "bold", size = base_size + 4),
      
      # Grid
      panel.grid.major = element_line(color = "#E0E0E0", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      
      # Legend
      legend.position = "right",
      legend.title = element_text(face = "bold", size = base_size),
      legend.text = element_text(size = base_size - 8),
      legend.key.height = unit(0.8, "cm"),
      
      # Plot margins
      plot.margin = margin(10, 15, 10, 10),
      
      # No background
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

#------------------------------------------------------------------------------
# Plot 1: All age groups on single plot
#------------------------------------------------------------------------------

cat("Creating combined RR curves plot...\n")

p_combined <- ggplot(rr_data, aes(x = temperature, y = RR, color = agegroup)) +
  # RR curves
  geom_line(linewidth = 0.8) +
  # Reference line at RR = 1
  geom_hline(yintercept = 1, linetype = "dashed", color = ie_colors$OceanBlue, alpha = 0.5) +
  # MMT markers
  geom_point(
    data = rr_data[, .(MMT = unique(MMT)), by = agegroup],
    aes(x = MMT, y = 1),
    shape = 18, size = 3
  ) +
  # Scales
  scale_color_manual(values = age_colors, name = "Age group") +
  scale_x_continuous(breaks = seq(-15, 30, by = 5)) +
  scale_y_continuous(limits = c(0.8, 3.5), breaks = seq(1, 3.5, by = 0.5)) +
  # Labels (no title - will be in LaTeX)
  labs(
    x = "Temperature (°C)",
    y = "Relative Risk (RR)"
  ) +
  theme_ie()

ggsave(file.path(img_dir, paste0(city_name, "_RR_curves.pdf")), p_combined, 
       width = 8, height = 5, device = cairo_pdf)
ggsave(file.path(img_dir, paste0(city_name, "_RR_curves.png")), p_combined, 
       width = 8, height = 5, dpi = 300, bg = "white")

cat(sprintf("Saved: %s/%s_RR_curves.pdf\n", img_dir, city_name))
cat(sprintf("Saved: %s/%s_RR_curves.png\n", img_dir, city_name))

#------------------------------------------------------------------------------
# Plot 2: Faceted by age group
#------------------------------------------------------------------------------

cat("Creating faceted RR curves plot...\n")

p_faceted <- ggplot(rr_data, aes(x = temperature, y = RR)) +
  # RR curve
  geom_line(color = ie_colors$OceanBlue, linewidth = 0.8) +
  # Reference line at RR = 1
  geom_hline(yintercept = 1, linetype = "dashed", color = ie_colors$OceanBlue, alpha = 0.5) +
  # MMT marker
  geom_point(
    data = rr_data[, .(MMT = unique(MMT)), by = agegroup],
    aes(x = MMT, y = 1),
    color = ie_colors$TechGreen, shape = 18, size = 3
  ) +
  # Facet by age
  facet_wrap(~agegroup, ncol = 5) +
  # Scales
  scale_x_continuous(breaks = seq(-10, 30, by = 10)) +
  scale_y_continuous(limits = c(0.8, 3.5), breaks = c(1, 2, 3)) +
  # Labels
  labs(
    x = "Temperature (°C)",
    y = "Relative Risk (RR)"
  ) +
  theme_ie(base_size = 32) +
  theme(
    strip.text = element_text(face = "bold", color = ie_colors$OceanBlue, size = 10),
    strip.background = element_rect(fill = "#F0F0F0", color = NA),
    panel.spacing = unit(0.8, "lines")
  )

ggsave(file.path(img_dir, paste0(city_name, "_RR_curves_faceted.pdf")), p_faceted, 
       width = 12, height = 3.5, device = cairo_pdf)
ggsave(file.path(img_dir, paste0(city_name, "_RR_curves_faceted.png")), p_faceted, 
       width = 12, height = 3.5, dpi = 300, bg = "white")

cat(sprintf("Saved: %s/%s_RR_curves_faceted.pdf\n", img_dir, city_name))
cat(sprintf("Saved: %s/%s_RR_curves_faceted.png\n", img_dir, city_name))

#------------------------------------------------------------------------------
# Summary table
#------------------------------------------------------------------------------

cat("\n=== Summary Statistics ===\n\n")

summary_stats <- rr_data[, .(
  MMT = round(unique(MMT), 1),
  RR_cold_max = round(max(RR[temperature == min(temperature)]), 2),
  RR_heat_max = round(max(RR[temperature == max(temperature)]), 2)
), by = agegroup]

print(summary_stats)

cat("\n=== Output Files ===\n")
cat("1. figures/rr_curves_combined.pdf  - All age groups overlaid\n")
cat("2. figures/rr_curves_combined.png  - All age groups overlaid (PNG)\n")
cat("3. figures/rr_curves_faceted.pdf   - Faceted by age group\n")
cat("4. figures/rr_curves_faceted.png   - Faceted by age group (PNG)\n")
