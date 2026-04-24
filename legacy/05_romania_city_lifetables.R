################################################################################
# 
# Compute Single-Age Life Tables for Romanian Cities
# (With and Without Climate Change)
# Includes full Life Table columns (mx, qx, lx, dx, Lx, Tx, ex...)
# Includes ages < 20 (assuming 0 climate impact for 0-19 if missing)
#
################################################################################

source("01_pkg_params.R")
source("functions/lifetable.R")
library(data.table)
library(arrow)

# Function to expand broad age groups to single ages
expand_to_single_age <- function(dt_grouped){
  res_list <- list()
  
  for(i in 1:nrow(dt_grouped)){
    row <- dt_grouped[i]
    start <- row$age_start
    # Cap at 100 for single age table
    end <- if(is.infinite(row$age_end)) 100 else row$age_end
    
    n_years <- end - start + 1
    
    # Distribute counts evenly, assume constant rate
    new_rows <- data.table(
      age = start:end,
      pop = row$pop / n_years,
      death = row$death / n_years,
      an_clim = row$an_clim / n_years,
      an_full = row$an_full / n_years,
      an_demo = row$an_demo / n_years
    )
    res_list[[i]] <- new_rows
  }
  return(rbindlist(res_list))
}

message("Loading and Preparing Data...")

# 1. Load Baseline Country Data (Wittgenstein)
# ---------------------------------------------
wittpop <- fread("data/wittgenstein_pop.csv") 
wittassr <- fread("data/wittgenstein_assr.csv")

# Clean
wittpop[, year5 := year][, year := NULL]
wittassr[, year5 := as.numeric(substr(period, 1, 4))][, period := NULL]
wittassr <- wittassr[age != "Newborn"]
wittpop <- wittpop[age != "All" & sex != "Both"]

# Merge
proj <- merge(wittpop, wittassr, by = c("CNTR_CODE", "ssp", "age", "sex", "year5"))
# Units: pop is usually in thousands in wittgenstein_pop (check metadata, assuming yes per 02_prep_data)
proj[, ":="(pop = 1000 * pop, death = 1000 * pop * (1 - assr))]

# Aggregate Sexes
proj <- proj[, .(pop = sum(pop), death = sum(death)), 
             by = .(CNTR_CODE, age, ssp, year5)]

# Filter for Romania (CNTR_CODE == "RO")
proj <- proj[CNTR_CODE == "RO"]

# Map Ages to Broad Groups manually to handle < 20
# Wittgenstein Groups: "0--4", "5--9", "10--14", "15--19", "20--24", ...
proj[, age_start := as.numeric(sapply(strsplit(age, "[-+]"), "[", 1))]

# Define our bespoke grouping for matching results + baseline
# Study groups: 20-44, 45-64, 65-74, 75-84, 85+
# We need to map 0-4, 5-9, 10-14, 15-19 into a "0-19" or keep separate.
# Since we only have `an_clim` for 20+, we can aggregate 0-19 together or keep as is.
# Let's map them to: "0-19", "20-44", "45-64", "65-74", "75-84", "85+"
# to facilitate merging with results (which start at 20).
# Actually, keeping 5-year granularity for <20 is better for Lx precision later.
# So:
# If age < 20: Keep 5 year groups (0--4, 5--9, 10--14, 15--19)
# If age >= 20: Map to study groups.

# Function to assign group
assign_group <- function(x){
  if(x < 20) return(paste0(x, "-", x+4)) # 0-4, 5-9...
  if(x < 45) return("20-44")
  if(x < 65) return("45-64")
  if(x < 75) return("65-74")
  if(x < 85) return("75-84")
  return("85+")
}

proj[, agegroup := sapply(age_start, assign_group)]
# Note: "0-4" matches pattern? Yes.

# Aggregate to these new groups
proj_agg <- proj[, .(wittpop = sum(pop), wittdeath = sum(death)), 
                 by = .(CNTR_CODE, agegroup, ssp, year5)]
proj_agg[, wittdeath := wittdeath / 5] # Annualize

# 2. Calibration (Scale Country -> City)
# --------------------------------------
message("Calibrating to City Level...")

# Historical Witt
histo_witt <- proj_agg[year5 %between% range(histrange), 
                       .(histowpop = mean(wittpop), histowdeaths = mean(wittdeath)), 
                       by = .(CNTR_CODE, agegroup, ssp)]

# City Data (Urban Audit)
# Note: city_results.csv usually only has 20+.
# If we want 0-19, we might not have `citycal` data for them if not in file.
# Let's check `cityage` (city_results.csv)
citycal_full <- fread("data/city_results.csv")
citycal_full <- citycal_full[CNTR_CODE == "RO"]

# Check if 0-19 in citycal
# unique(citycal_full$agegroup) -> usually "20-44" etc.
# If missing, we cannot scale 0-19 using city-specific data easily.
# Assumption: Use the ratio of youngest available group (20-44) or Total?
# Better: Use "20-44" scaling factor for 0-19 as a proxy.
# We will create a mapping of `agegroup` -> `proxy_group_for_scaling`

# Calculate scaling factors for available groups
citycal_sum <- citycal_full[, .(uraupop = sum(agepop), uraudeath = sum(death)), 
                   by = .(URAU_CODE, CNTR_CODE, agegroup)]

# Merge to get factors for 20+
scales <- merge(histo_witt[agegroup %in% unique(citycal_sum$agegroup)], 
                citycal_sum, 
                by = c("CNTR_CODE", "agegroup"), allow.cartesian=TRUE)
scales[, ":="(popfac = uraupop / histowpop, dfac = uraudeath / histowdeaths)]

# Now we need factors for 0-4, 5-9...
# We will duplicate the "20-44" factors for them.
scale_proxy <- scales[agegroup == "20-44"]
young_groups <- c("0-4", "5-9", "10-14", "15-19")

# Create rows for young groups
extra_scales <- list()
for(yg in young_groups){
  tmp <- copy(scale_proxy)
  tmp[, agegroup := yg]
  # We use the factor, but we must link it to the HISTORICAL witt data for that young group
  # Actually, we just need `popfac` and `dfac`.
  extra_scales[[yg]] <- tmp[, .(URAU_CODE, CNTR_CODE, agegroup, ssp, popfac, dfac)]
}
extra_scales_dt <- rbindlist(extra_scales)

# Combine factors
final_scales <- rbind(
  scales[, .(URAU_CODE, CNTR_CODE, agegroup, ssp, popfac, dfac)],
  extra_scales_dt
)

# Merge back to PROJ
city_proj <- merge(proj_agg, final_scales, by = c("CNTR_CODE", "agegroup", "ssp"), allow.cartesian = TRUE)
city_proj[, ":="(pop = wittpop * popfac, death = wittdeath * dfac)]


# 3. Load Attribution Results (Climate)
# -------------------------------------
message("Loading City Attribution Results...")

res_path <- "results_parquet/city_period.parquet"
if(!file.exists(res_path)) stop("City results parquet not found!")

res_ds <- open_dataset(res_path) |> collect() |> setDT()
if("range" %in% names(res_ds)) res_ds <- res_ds[range == "tot"]

# Rename city -> URAU_CODE
if("city" %in% names(res_ds) && !"URAU_CODE" %in% names(res_ds)) setnames(res_ds, "city", "URAU_CODE")

# Filter for Romanian cities
res_ds <- res_ds[URAU_CODE %in% unique(citycal_full$URAU_CODE)]

# Reshape sc -> separate columns
if("sc" %in% names(res_ds)){
  # Pivot to wide format to get an_clim_est, an_full_est, an_demo_est
  res_ds <- dcast(res_ds, URAU_CODE + ssp + period + adapt + agegroup ~ sc, value.var = "an_est")
  
  # Rename columns
  for(scen in c("clim", "full", "demo")){
    if(scen %in% names(res_ds)) setnames(res_ds, scen, paste0("an_", scen, "_est"))
    else res_ds[[paste0("an_", scen, "_est")]] <- 0 
  }
}

res_ds[, ":="(
  period = as.numeric(period),
  ssp = as.integer(ssp),
  agegroup = as.character(agegroup)
)]

# Prepare Projection for Merge
city_proj[, ":="(
  period = as.numeric(year5),
  ssp = as.integer(ssp),
  agegroup = as.character(agegroup)
)]

# 4. Merge (Left Join to keep 0-19)
# ---------------------------------
full_dt <- merge(city_proj, res_ds, 
                 by = c("URAU_CODE", "ssp", "period", "agegroup"), 
                 all.x = TRUE) # Keep baseline ages even if no climate result

# Fill NA `an_clim_est` with 0 (Assumption: No climate impact for <20 or missing data)
full_dt[is.na(an_clim_est), an_clim_est := 0]
full_dt[is.na(an_full_est), an_full_est := 0]
full_dt[is.na(an_demo_est), an_demo_est := 0]


# 5. Calculation
# --------------
message("Calculating Full Life Tables...")

# Define age intervals for expansion
# We have groups: 0-4, 5-9, 10-14, 15-19, 20-44, 45-64, 65-74, 75-84, 85+
# We need to map them to start/end integers
age_map <- data.table(
  agegroup = c("0-4", "5-9", "10-14", "15-19", "20-44", "45-64", "65-74", "75-84", "85+"),
  start =    c(0,     5,     10,      15,      20,      45,      65,      75,      85),
  end   =    c(4,     9,     14,      19,      44,      64,      74,      84,      100)
)

full_dt <- merge(full_dt, age_map, by="agegroup")

# Loop through cities
results_list <- list()

# Only process scenarios present in results + baseline
# Note: full_dt expanded by adaptation scenarios from res_ds merge?
# For ages < 20, merge was all.x=T, so adapt became NA?
# We must Replicate 0-19 for each adapt scenario present in 20+
# Get list of adapt scenarios per city/ssp/period
adapt_scens <- unique(res_ds[, .(URAU_CODE, ssp, period, adapt)])
if(nrow(adapt_scens) > 0){
  # Identify rows with NA adapt (these are the <20s)
  young_rows <- full_dt[is.na(adapt)]
  adult_rows <- full_dt[!is.na(adapt)]
  
  # Cross join young_rows with adapt_scens
  # Join by city, ssp, period
  young_expanded <- merge(young_rows[, -c("adapt")], adapt_scens, 
                          by = c("URAU_CODE", "ssp", "period"), allow.cartesian=TRUE)
  
  full_dt <- rbind(adult_rows, young_expanded)
}

groups <- unique(full_dt[, .(URAU_CODE, ssp, period, adapt)])

pb <- txtProgressBar(min = 0, max = nrow(groups), style = 3)

for(i in 1:nrow(groups)){
  setTxtProgressBar(pb, i)
  g <- groups[i,]
  
  subdat <- full_dt[URAU_CODE == g$URAU_CODE & ssp == g$ssp & 
                    period == g$period & adapt == g$adapt]
  
  # Check we have full age range
  if(min(subdat$start) > 0) next # Skip if missing young ages
  
  dt_to_expand <- subdat[, .(age_start=start, age_end=end, 
                             pop=pop, death=death, 
                             an_clim=an_clim_est,
                             an_full=an_full_est,
                             an_demo=an_demo_est)]
  
  # Expand to single age
  single_age_dt <- expand_to_single_age(dt_to_expand)
  setorder(single_age_dt, age)
  
  # 1. No CC
  mx_no_cc <- single_age_dt$death / single_age_dt$pop
  lt_no_cc <- lifetable(mx_no_cc, single_age_dt$age)
  
  # 2. With CC
  mx_cc <- (single_age_dt$death + single_age_dt$an_clim) / single_age_dt$pop
  lt_cc <- lifetable(mx_cc, single_age_dt$age)
  
  # Format Output
  # Join columns
  # Suffixes
  cols <- names(lt_no_cc)
  
  res <- data.table(
    URAU_CODE = g$URAU_CODE,
    ssp = g$ssp,
    period = g$period,
    adapt = g$adapt,
    age = lt_no_cc$age
  )
  
  for(col in setdiff(cols, "age")){
    res[[paste0(col, "_no_cc")]] <- lt_no_cc[[col]]
    res[[paste0(col, "_cc")]] <- lt_cc[[col]]
  }
  
  # Add attributable numbers per single age
  # Rename columns to avoid overwriting 'n' (interval width) from the lifetable function
  res[, ":="(
    deaths_no_cc = single_age_dt$death,
    deaths_cc = single_age_dt$death + single_age_dt$an_clim,
    an_clim = single_age_dt$an_clim,
    an_full = single_age_dt$an_full,
    an_demo = single_age_dt$an_demo
  )]
  
  results_list[[i]] <- res
}
close(pb)

final <- rbindlist(results_list)

# Metadata
meta <- unique(citycal_full[, .(URAU_CODE, LABEL = gsub(",", "", LABEL))])
final <- merge(final, meta, by = "URAU_CODE", all.x = TRUE)

message("Done. Rows: ", nrow(final))
if(!dir.exists("results_csv")) dir.create("results_csv")

# Create output folder for cities
city_out_dir <- "results_csv/individual_cities"
if(!dir.exists(city_out_dir)) dir.create(city_out_dir)

# Split and save per city
cities <- unique(final$URAU_CODE)

# Avoid scientific notation in output
old_scipen <- getOption("scipen")
options(scipen = 999)

message("Saving separate city files into ", city_out_dir, "...")
pb_save <- txtProgressBar(min = 0, max = length(cities), style = 3)

for(k in seq_along(cities)){
  city_code <- cities[k]
  city_data <- final[URAU_CODE == city_code]
  
  # Construct filename
  fname <- file.path(city_out_dir, paste0(city_code, "_lifetables.csv"))
  write.csv(city_data, fname, row.names = FALSE)
  setTxtProgressBar(pb_save, k)
}
close(pb_save)
options(scipen = old_scipen)

message("Saved individual city files.")
