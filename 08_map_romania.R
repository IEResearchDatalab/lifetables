# 08_map_romania.R

# Load libraries
library(dplyr)
library(tidyr)
library(data.table)
library(sf)
library(ggplot2)
library(arrow)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

# Ensure figures/maps directory exists
if (!dir.exists("figures/maps")) dir.create("figures/maps", recursive = TRUE)

# 1. Load City Results
# We need SSP3, Adapt 0%, Period 2095
message("Loading City Results...")

# Path to city_period parquet
pq_path <- "results_parquet/city_period.parquet"
if (!file.exists(pq_path)) stop("Parquet file not found.")

cities_pq <- open_dataset(pq_path) |> collect() |> setDT()

message("Filtering Data...")
# Filter for SSP3, Adapt 0%, Period 2095 (Target Scenario)
ssp_target <- 3
adapt_target <- "0%"
period_target <- 2095

city_data <- cities_pq[ssp == ssp_target & adapt == adapt_target & period == period_target]

# 2. Load Coordinates
# Using city_results.csv as it contains Lat/Lon
meta_path <- "data/city_results.csv"
if(!file.exists(meta_path)) stop("Metadata file not found.")

meta_dt <- fread(meta_path)
# Keep unique city lat/lon (meta might have multiple rows per city due to age groups)
coords_dt <- unique(meta_dt[, .(URAU_CODE, LABEL, lat, lon)])

# Filter only Romanian cities in our results
# Identify RO cities from codes starting with "RO"
ro_codes <- grep("^RO", city_data$city, value=TRUE) %>% unique()
city_data <- city_data[city %in% ro_codes]

# Merge coordinates
# Adjust column name if needed. Parquet likely has 'city' for code.
if("city" %in% names(city_data)) setnames(city_data, "city", "URAU_CODE")
city_data <- merge(city_data, coords_dt, by = "URAU_CODE", all.x = TRUE)

message("Data loaded for ", length(unique(city_data$URAU_CODE)), " cities.")


# 3. Retrieve Precise Population (2095, SSP3)
# We need this to calculate Rate per 100k accurately for the projected year
message("Reading population from generated lifetables...")
lt_files <- list.files("results_csv/individual_cities", pattern = "_lifetables.csv", full.names = TRUE)
pop_list <- list()

for(f in lt_files){
  d <- fread(f)
  # Filter 2095 SSP3 Adapt 0%
  d_sub <- d[period == 2095 & ssp == 3 & adapt == "0%"]
  if(nrow(d_sub) > 0){
    # Reconstruct pop from deaths / mx
    d_sub[, pop_recovered := ifelse(mx_no_cc > 0, deaths_no_cc / mx_no_cc, 0)]
    
    # Total pop
    total_pop <- sum(d_sub$pop_recovered, na.rm=TRUE)
    code <- unique(d_sub$URAU_CODE)
    pop_list[[code]] <- data.table(URAU_CODE = code, pop_2095 = total_pop)
  }
}
pop_dt <- rbindlist(pop_list)
message(sprintf("Population retrieved for %d cities.", nrow(pop_dt)))

# 4. Calculate Indicators per City
# Net Excess (Sum of Heat + Cold AN)
net_dt <- city_data[, .(an_net = sum(an_est), an_heat = sum(an_est[range=="heat"])), by = URAU_CODE]

# Merge with Pop and Coords
plot_dt <- merge(net_dt, pop_dt, by = "URAU_CODE")
plot_dt <- merge(plot_dt, coords_dt, by = "URAU_CODE")

# Calculate Rates
plot_dt[, ":="(
  net_rate = (an_net / pop_2095) * 100000,
  heat_rate = (an_heat / pop_2095) * 100000
)]


# 5. Prepare Map Data: Romania Borders
# Load Romania Map
message("Fetching map data...")
# Try rnaturalearth
romania_sf <- tryCatch({
  ne_countries(scale = "medium", country = "Romania", returnclass = "sf")
}, error = function(e){
    NULL
})


if (is.null(romania_sf) || nrow(romania_sf) == 0) {
  message("Using fallback map...")
  # Fallback to simple bounding box if NE fails (rare but possible offline)
  romania_sf <- st_as_sfc(st_bbox(c(xmin = 20, xmax = 30, ymin = 43, ymax = 48), crs = 4326))
}

# 6. Generate Maps

# For better visualization, let's encode:
# Color = The Primary Metric (Risk/Impact)
# Size  = The Underlying Population (Context/Scale)

message("Generating Map 1: Net Rate...")
# 1. Net Excess Mortality Rate Map
p1 <- ggplot() +
  geom_sf(data = romania_sf, fill = "grey95", color = "grey80") +
  geom_point(data = plot_dt, aes(x = lon, y = lat, size = pop_2095, color = net_rate), alpha = 0.8) +
  scale_size_continuous(range = c(2, 12), name = "Population (2095)", labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_gradient2(low = "#4575b4", mid = "white", high = "#d73027", midpoint = 0, name = "Net Rate\n(per 100k)") +
  labs(
    title = "Net Excess Mortality Risk (2095)",
    subtitle = "Color = Risk Intensity | Size = City Population",
    x = NULL, y = NULL
  ) +
  # Label top 3 cities (Bold, Black)
  geom_text(data = plot_dt[order(-abs(net_rate))][1:3], aes(x = lon, y = lat, label = LABEL), vjust = -1.8, size = 3, fontface="bold") +
  # Label other cities (Grey, Smaller)
  geom_text(data = plot_dt[order(-abs(net_rate))][4:.N], aes(x = lon, y = lat, label = LABEL), vjust = -1.8, size = 2.2, color = "grey40") +
  # Order legends: Color (1) then Size (2)
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2)) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

ggsave("figures/maps/map1_net_rate_2095.png", p1, width = 10, height = 7, bg = "white")

message("Generating Map 2: Heat Rate...")
# 2. Heat Excess Mortality Rate Map
p2 <- ggplot() +
  geom_sf(data = romania_sf, fill = "grey95", color = "grey80") +
  geom_point(data = plot_dt, aes(x = lon, y = lat, size = pop_2095, color = heat_rate), alpha = 0.8) +
  scale_size_continuous(range = c(2, 12), name = "Population (2095)", labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_viridis_c(option = "inferno", direction = -1, name = "Heat Excess\nRate (per 100k)") + 
  labs(
    title = "Projected Heat-Related Mortality Risk (2095)",
    subtitle = "Color = Risk Intensity | Size = City Population",
    x = NULL, y = NULL
  ) +
  # Label top 3 cities
  geom_text(data = plot_dt[order(-heat_rate)][1:3], aes(x = lon, y = lat, label = LABEL), vjust = -1.8, size = 3, fontface="bold") +
  # Label other cities
  geom_text(data = plot_dt[order(-heat_rate)][4:.N], aes(x = lon, y = lat, label = LABEL), vjust = -1.8, size = 2.2, color = "grey40") +
  # Order legends: Color (1) then Size (2)
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2)) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

ggsave("figures/maps/map2_heat_rate_2095.png", p2, width = 10, height = 7, bg = "white")

message("Generating Map 3: Total Deaths...")
# 3. Total Absolute Net Deaths
p3 <- ggplot() +
  geom_sf(data = romania_sf, fill = "grey95", color = "grey80") +
  geom_point(data = plot_dt, aes(x = lon, y = lat, size = pop_2095, color = an_net), alpha = 0.8) +
  scale_size_continuous(range = c(2, 12), name = "Population (2095)", labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_gradient(low = "#fee0d2", high = "#a50f15", name = "Count") +
  labs(
    title = "Total Absolute Excess Deaths (2095)",
    subtitle = "Color = Total Deaths | Size = City Population",
    x = NULL, y = NULL
  ) +
  # Label top 5 cities
  geom_text(data = plot_dt[order(-an_net)][1:5], aes(x = lon, y = lat, label = LABEL), vjust = -1.8, size = 3, fontface="bold") +
  # Label other cities
  geom_text(data = plot_dt[order(-an_net)][6:.N], aes(x = lon, y = lat, label = LABEL), vjust = -1.8, size = 2.2, color = "grey40") +
  # Order legends: Color (1) then Size (2)
  guides(color = guide_colorbar(order = 1), size = guide_legend(order = 2)) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

ggsave("figures/maps/map3_total_deaths_2095.png", p3, width = 10, height = 7, bg = "white")

message("Maps saved successfully.")
