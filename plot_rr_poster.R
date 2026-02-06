################################################################################
# 
# Poster Layout: 3D Surface Plots and Heatmaps
# Comparing Discrete (non-interpolated) vs Smooth (interpolated) versions
#
# Layout:
#   Top row: 3D surfaces (static versions)
#   Bottom row: Heatmaps
#   Left column: Discrete/stepped (original data)
#   Right column: Smooth/interpolated
#
################################################################################

# Load required packages
library(data.table)
library(dplyr)
library(dlnm)
library(splines)
library(ggplot2)
library(viridis)
library(patchwork)  # For combining plots
library(png)        # For reading PNG files
library(grid)       # For rasterGrob
library(showtext)

# Register Montserrat font (consistent with IE LaTeX template)
font_add("Montserrat", 
         regular = "fonts/Montserrat-Regular.ttf",
         bold = "fonts/Montserrat-Bold.ttf",
         italic = "fonts/Montserrat-Italic.ttf",
         bolditalic = "fonts/Montserrat-BoldItalic.ttf")
showtext_auto()

#------------------------------------------------------------------------------
# Load city configuration
#------------------------------------------------------------------------------
source("config.R")

#------------------------------------------------------------------------------
# Load data and compute RR curves
#------------------------------------------------------------------------------

cat("Loading data...\n")

# Load coefficients
coefs_all <- fread("data/coefs.csv")
coefs_city <- coefs_all[URAU_CODE == city_code]

# Load temperature distribution
library(arrow)
tdist <- read_parquet("data/era5series.gz.parquet") |>
  subset(URAU_CODE == city_code & year(date) %between% histrange) |>
  reframe(perc = predper, tmean = quantile(era5landtmean, predper / 100, na.rm = TRUE))

varknots <- subset(tdist, perc %in% varper, tmean, drop = TRUE)
varbound <- range(tdist$tmean)
temp_range <- tdist$tmean

cat(sprintf("Temperature range: %.1f°C to %.1f°C\n", min(temp_range), max(temp_range)))

# Compute RR for each age group
argvar <- list(fun = varfun, degree = vardegree, knots = varknots, Bound = varbound)
n_temp <- 100
temp_seq <- seq(min(temp_range), max(temp_range), length.out = n_temp)
basis <- do.call(onebasis, c(list(x = temp_seq), argvar))

cat("Computing relative risk curves...\n")

rr_list <- list()
for (i in seq_along(agelabs)) {
  age <- agelabs[i]
  coef_row <- coefs_city[agegroup == age]
  coefs <- as.numeric(coef_row[, .(b1, b2, b3, b4, b5)])
  
  log_rr <- basis %*% coefs
  ind <- temp_seq >= quantile(temp_seq, 0.25) & temp_seq <= quantile(temp_seq, 0.99)
  mmt <- temp_seq[ind][which.min(log_rr[ind])]
  
  cenvec <- do.call(onebasis, c(list(x = mmt), argvar))
  log_rr_centered <- log_rr - drop(cenvec %*% coefs)
  rr <- pmax(exp(log_rr_centered), 1)
  
  rr_list[[age]] <- data.table(
    agegroup = age,
    age_mid = age_midpoints[i],
    temperature = temp_seq,
    RR = as.vector(rr),
    MMT = mmt
  )
  cat(sprintf("  %s: MMT = %.1f°C\n", age, mmt))
}

rr_data <- rbindlist(rr_list)

# Create matrix for surface plots
temp_grid <- unique(rr_data$temperature)
age_grid <- unique(rr_data$age_mid)

rr_matrix <- matrix(NA, nrow = length(temp_grid), ncol = length(age_grid))
for (j in seq_along(age_grid)) {
  age_data <- rr_data[age_mid == age_grid[j]]
  rr_matrix[, j] <- age_data$RR
}

#------------------------------------------------------------------------------
# Create DISCRETE (non-interpolated) heatmap
#------------------------------------------------------------------------------

cat("Creating discrete heatmap...\n")

# Define age band info with actual ranges
# For tiles to fill exactly from 20-100, we need centers at midpoint of each band
age_band_info <- data.table(
  agegroup = agelabs,
  age_start = c(20, 45, 65, 75, 85),
  age_end = c(45, 65, 75, 85, 100)  # Use exclusive end boundaries for proper tiling
)
age_band_info[, `:=`(
  age_center = (age_start + age_end) / 2,
  age_height = age_end - age_start
)]

# Merge into rr_data
rr_discrete <- merge(rr_data, age_band_info, by = "agegroup")

# Temperature tile width
temp_width <- diff(temp_seq)[1]

#------------------------------------------------------------------------------
# Create SMOOTH (interpolated) heatmap
#------------------------------------------------------------------------------

cat("Creating smooth heatmap...\n")

# Interpolate to fine regular grid (20 to 100 to match discrete range)
n_age_fine <- 200
age_fine <- seq(20, 100, length.out = n_age_fine)

rr_interp_list <- lapply(temp_grid, function(t) {
  rr_at_temp <- rr_matrix[which(temp_grid == t), ]
  rr_fine <- approx(x = age_midpoints, y = rr_at_temp, xout = age_fine, rule = 2)$y
  data.table(temperature = t, age = age_fine, RR = rr_fine)
})
rr_smooth_grid <- rbindlist(rr_interp_list)

# Shared color limits for all plots (compute before plotting)
rr_min_shared <- 1
rr_max_shared <- max(c(rr_discrete$RR, rr_smooth_grid$RR), na.rm = TRUE)

# Create labels at key points to show interpolation is correct
# Select a few representative temperatures (cold, MMT region, hot)
label_temps <- c(-10, 0, 15, 25, 30)
# Find closest temperatures in the grid
label_temps_actual <- sapply(label_temps, function(t) temp_grid[which.min(abs(temp_grid - t))])

# Get RR values at age midpoints for discrete data
rr_labels_discrete <- rr_data[temperature %in% label_temps_actual, 
                               .(temperature, age_mid, RR, agegroup)]
rr_labels_discrete[, RR_label := sprintf("%.2f", RR)]

# Get RR values at age midpoints for smooth data (should match discrete at midpoints)
# Find closest ages in the fine grid to the original midpoints
rr_labels_smooth <- rbindlist(lapply(age_midpoints, function(a) {
  closest_age <- age_fine[which.min(abs(age_fine - a))]
  rr_smooth_grid[temperature %in% label_temps_actual & age == closest_age,
                  .(temperature, age = a, RR)]  # Use original midpoint for plotting
}))
rr_labels_smooth[, RR_label := sprintf("%.2f", RR)]

#------------------------------------------------------------------------------
# IE LaTeX Template Colors and Theme
#------------------------------------------------------------------------------

ie_colors <- list(
  OceanBlue = "#000066",
  ElectricBlue = "#0000db",
  SeaBlue = "#47bfff",
  TechGreen = "#6DC201",
  TechBlack = "#000024"
)

# IE-styled color scale (blue to white to warm)
ie_heatmap_colors <- c("#000066", "#0000db", "#47bfff", "#F7F7F7", "#FDDBC7", "#EF8A62", "#B2182B")

theme_ie <- function(base_size = 40) {
  theme_minimal(base_size = base_size, base_family = "Montserrat") +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(face = "bold", color = ie_colors$OceanBlue, size = base_size + 6),
      axis.text = element_text(color = ie_colors$OceanBlue, size = base_size - 4),
      legend.title = element_text(face = "bold", color = ie_colors$OceanBlue, size = base_size),
      legend.text = element_text(color = ie_colors$OceanBlue, size = base_size - 6),
      text = element_text(family = "Montserrat", color = ie_colors$OceanBlue),
      legend.position = "top",
      legend.direction = "horizontal"
    )
}

heatmap_discrete <- ggplot(rr_discrete, aes(x = temperature, y = age_center, fill = RR)) +
  geom_tile(aes(height = age_height), width = temp_width) +
  scale_fill_gradientn(
    colors = ie_heatmap_colors,
    name = "RR",
    limits = c(rr_min_shared, rr_max_shared),
    oob = scales::squish
  ) +
  scale_y_continuous(
    breaks = age_band_info$age_center,
    labels = agelabs,
    limits = c(20, 100),
    expand = c(0, 0)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_point(
    data = rr_discrete[, .(MMT = unique(MMT), age_center = unique(age_center)), by = agegroup],
    aes(x = MMT, y = age_center),
    color = "white", shape = 4, size = 2.5, stroke = 1.5, inherit.aes = FALSE
  ) +
  geom_label(
    data = rr_labels_discrete,
    aes(x = temperature, y = age_mid, label = RR_label),
    color = ie_colors$OceanBlue, fill = alpha("white", 0.8), 
    size = 16, fontface = "bold", label.size = 0, label.padding = unit(0.1, "lines"),
    inherit.aes = FALSE
  ) +
  labs(
    x = "Temperature (°C)",
    y = "Age (years)"
  ) +
  guides(fill = guide_colorbar(title.position = "left", barwidth = 15, barheight = 1)) +
  theme_ie()

heatmap_smooth <- ggplot(rr_smooth_grid, aes(x = temperature, y = age, fill = RR)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_gradientn(
    colors = ie_heatmap_colors,
    name = "RR",
    limits = c(rr_min_shared, rr_max_shared),
    oob = scales::squish
  ) +
  scale_y_continuous(
    breaks = seq(20, 100, by = 10),
    expand = c(0, 0)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_point(
    data = rr_data[, .(MMT = unique(MMT)), by = .(agegroup, age_mid)],
    aes(x = MMT, y = age_mid),
    color = "white", shape = 4, size = 2.5, stroke = 1.5, inherit.aes = FALSE
  ) +
  geom_label(
    data = rr_labels_smooth,
    aes(x = temperature, y = age, label = RR_label),
    color = ie_colors$OceanBlue, fill = alpha("white", 0.8), 
    size = 16, fontface = "bold", label.size = 0, label.padding = unit(0.1, "lines"),
    inherit.aes = FALSE
  ) +
  labs(
    x = "Temperature (°C)",
    y = "Age (years)"
  ) +
  guides(fill = guide_colorbar(title.position = "left", barwidth = 15, barheight = 1)) +
  theme_ie()

#------------------------------------------------------------------------------
# Create static 3D surface plots
#------------------------------------------------------------------------------

cat("Creating 3D surface plots...\n")

# Use the same shared color limits as heatmaps (rr_min_shared, rr_max_shared defined above)

# Color function for surfaces using IE color gradient (same as heatmaps)
# persp() colors facets, so we need (nrow-1) x (ncol-1) colors
# Each facet color is based on the average of its 4 corner z-values
get_surface_colors <- function(z_matrix, zlim = c(rr_min_shared, rr_max_shared), ncolors = 100) {
  color_palette <- colorRampPalette(ie_heatmap_colors)(ncolors)
  
  nr <- nrow(z_matrix)
  nc <- ncol(z_matrix)
  
  # Compute facet centers as average of 4 corners
  facet_z <- (z_matrix[1:(nr-1), 1:(nc-1)] + 
              z_matrix[2:nr, 1:(nc-1)] + 
              z_matrix[1:(nr-1), 2:nc] + 
              z_matrix[2:nr, 2:nc]) / 4
  
  # Map to colors using shared limits
  z_scaled <- (facet_z - zlim[1]) / (zlim[2] - zlim[1])
  z_scaled <- pmax(0, pmin(1, z_scaled))  # Clamp to [0, 1]
  color_idx <- floor(z_scaled * (ncolors - 1)) + 1
  
  matrix(color_palette[color_idx], nrow = nr - 1, ncol = nc - 1)
}

# --- DISCRETE 3D surface ---
# Create stepped data for discrete version
age_bounds <- c(20, 45, 65, 75, 85, 100)
age_expanded <- c()
for (i in 1:(length(age_bounds) - 1)) {
  age_expanded <- c(age_expanded, age_bounds[i] + 0.01, age_bounds[i + 1] - 0.01)
}

rr_matrix_discrete <- matrix(NA, nrow = length(temp_grid), ncol = length(age_expanded))
for (j in seq_along(age_grid)) {
  age_data <- rr_data[age_mid == age_grid[j]]
  rr_matrix_discrete[, (j - 1) * 2 + 1] <- age_data$RR
  rr_matrix_discrete[, (j - 1) * 2 + 2] <- age_data$RR
}

# Save discrete 3D
png("figures/temp_3d_discrete.png", width = 800, height = 700, res = 100)
par(mar = c(0, 2, 0, 0), cex.lab = 2, cex.axis = 1.5)
persp(
  x = temp_grid, y = age_expanded, z = rr_matrix_discrete,
  theta = 40, phi = 20, expand = 0.55,
  col = get_surface_colors(rr_matrix_discrete),
  shade = 0.3, border = NA, ticktype = "detailed", nticks = 4,
  xlab = "Temperature (°C)", ylab = "Age", zlab = "RR",
  zlim = c(rr_min_shared, rr_max_shared),
  cex.lab = 2, cex.axis = 1.5
)
dev.off()

# --- SMOOTH 3D surface ---
# Rebuild smooth matrix properly: rows = temperature, cols = age
# rr_smooth_grid is ordered by temperature first, then age
rr_matrix_smooth <- matrix(NA, nrow = length(temp_grid), ncol = n_age_fine)
for (i in seq_along(temp_grid)) {
  rr_matrix_smooth[i, ] <- rr_smooth_grid[temperature == temp_grid[i], RR]
}

png("figures/temp_3d_smooth.png", width = 800, height = 700, res = 100)
par(mar = c(0, 2, 0, 0), cex.lab = 2, cex.axis = 1.5)
persp(
  x = temp_grid, y = age_fine, z = rr_matrix_smooth,
  theta = 40, phi = 20, expand = 0.55,
  col = get_surface_colors(rr_matrix_smooth),
  shade = 0.3, border = NA, ticktype = "detailed", nticks = 4,
  xlab = "Temperature (°C)", ylab = "Age", zlab = "RR",
  zlim = c(rr_min_shared, rr_max_shared),
  cex.lab = 2, cex.axis = 1.5
)
dev.off()

#------------------------------------------------------------------------------
# Combine into poster layout
#------------------------------------------------------------------------------

cat("Creating poster layout...\n")

# Read 3D images
img_discrete <- readPNG("figures/temp_3d_discrete.png")
img_smooth <- readPNG("figures/temp_3d_smooth.png")

# Convert to grobs
grob_discrete <- rasterGrob(img_discrete, interpolate = TRUE)
grob_smooth <- rasterGrob(img_smooth, interpolate = TRUE)

# Create ggplot wrappers for the images
plot_3d_discrete <- ggplot() +
  annotation_custom(grob_discrete, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_void()

plot_3d_smooth <- ggplot() +
  annotation_custom(grob_smooth, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_void()

# Combine using patchwork (no titles - will be added in LaTeX)
# Collect shared legend at top
poster <- (plot_3d_discrete | plot_3d_smooth) / 
          (heatmap_discrete | heatmap_smooth) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

# Save poster
ggsave(file.path(img_dir, "rr_poster_comparison_ie.png"), poster, 
       width = 14, height = 12, dpi = 300, bg = "white")

cat(sprintf("Saved: %s/rr_poster_comparison_ie.png\n", img_dir))

# Clean up temp files
unlink("figures/temp_3d_discrete.png")
unlink("figures/temp_3d_smooth.png")

cat("\n=== Poster Complete ===\n")
