################################################################################
# 
# Contrasting future heat and cold-related mortality under climate change, 
# demographic and adaptation scenarios in 854 European cities
#
# R Code Part 0: Download external data
#
# Pierre Masselot & Antonio Gasparrini
#
################################################################################

#----- Necessary libraries
library(wcde) # To Download data from Wittgensetin
library(countrycode) # To link various country codes
library(dplyr) # Data management
library(doParallel) # To load more quickly all projection data
library(data.table) # For efficient management of large datasets
library(arrow) # To write parquet files
library(zen4R) # To download from Zenodo
library(R.utils) # To download a file from internet
library(terra) # To link GDP grid to cities
library(tidyr) # For pivot_longer

# Whether to save the download data
savedata <- F

#-------------------------
# Results from LPH paper
#-------------------------

# List fo files from Zenodo
filelist <- list("coefs.csv", "coef_simu.csv", "results.zip",
  "additional_data.zip")

# Download in a temporary location
zpath <- tempdir()
options(timeout = 1000) # Increase timeout for coef_simu
download_zenodo("10.5281/zenodo.10288665", path = zpath, files = filelist,
  logger = "INFO")

# Copy files to be used as is
asisf <- c("coefs.csv")
file.copy(sprintf("%s/%s", zpath, asisf), ".", overwrite = savedata)

# Unzip some files
unzip(sprintf("%s/additional_data.zip", zpath), "meta-model.RData", exdir = ".")
unzip(sprintf("%s/results.zip", zpath), "cityage.csv", exdir = ".")
file.rename("cityage.csv", "city_results.csv")

# Load other files to be exported as parquet
coefsimu <- read.csv(sprintf("%s/coef_simu.csv", zpath))
era5series <- read.csv(unz(sprintf("%s/additional_data.zip", zpath), 
  "era5series.csv"), colClasses = c(date = "Date"))

# Export as parquet
if (savedata){
  write_parquet(coefsimu, "coef_simu.gz.parquet", compression = "gzip")
  write_parquet(era5series, "era5series.gz.parquet", compression = "gzip")
}

# Remove 
unlink(sprintf("%s/%s", zpath, filelist))

#-------------------------
# Warming levels
#-------------------------

# Download and load
url <- paste0("https://raw.githubusercontent.com/IPCC-WG1/Atlas/main", 
  "/warming-levels/CMIP6_Atlas_WarmingLevels.csv")
warming_years <- read.csv(url, check.names = F, na.strings = c("NA", "9999"))

# Export
if (savedata){
  write.csv(warming_years, "warming_years.csv", quote = F, row.names = F)
}

#-------------------------
# Wittgenstein projections
#-------------------------

# Get list of countries in the study
countries <- read.csv("city_results.csv") |> 
  subset(select = c(CNTR_CODE, cntr_name)) |>
  unique()

# Get country codes used in Wittgenstein
countries <- mutate(countries, 
  ISO3 = countrycode(CNTR_CODE, "eurostat", "iso3n"))

# Download Wittgenstein data
assr_data <- get_wcde(indicator = "assr", scenario = c(1:3),
  country_code = countries$ISO3, version = "wcde-v2")
pop_data <- get_wcde(indicator = "pop", scenario = c(1:3),
  country_code = countries$ISO3, pop_age = "all", pop_sex = "all", 
  version = "wcde-v2")

# Add info about URAU country code
assr_data <- merge(assr_data, countries, all.x = T,
    by.x = "country_code", by.y = "ISO3") |>
  subset(select = -c(country_code, name)) |>
  rename(ssp = "scenario")
pop_data <- merge(pop_data, countries, all.x = T,
    by.x = "country_code", by.y = "ISO3") |>
  subset(select = -c(country_code, name)) |>
  rename(ssp = "scenario")

# Export
if(savedata){
  write.csv(assr_data, "wittgenstein_assr.csv", quote = F, row.names = F)
  write.csv(pop_data, "wittgenstein_pop.csv", quote = F, row.names = F)
}
