## This script brings in all the publicly available datasets of use to this paper
## from https://drive.google.com/drive/folders/1m6fbCoOynP3pxi0GObSCG0_77mvYsu74
##
## 2022-06-02 (updated 2023-05-03)
## Donnie Day, Peter Regier, Daniel Sandborn
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, ## keep things tidy
       googledrive) ## download files from google drive

## Set up common columns that are routinely used to in select() and join() calls
common_cols = c("kit_id", "transect_location")


# 2. Import datasets -----------------------------------------------------------

## Using data from the ESS-DIVE package
## Data provided 5/3/22
essdive_sediment_filepath = "data/ess_dive_package/ec1_sediment_v1/"
essdive_soil_filepath = "data/ess_dive_package/ec1_soil_v1/"

## GWC
gwc_sediment <- read_csv(paste0(essdive_sediment_filepath, "ec1_sediment_gwc_L2.csv")) 
gwc_soil <- read_csv(paste0(essdive_soil_filepath, "ec1_soil_gwc_L2.csv"))
gwc <- 

## We are finally ready to read data into R! Read in each file of interest: 
## BD = bulk density, GWC = gravimetric water content, LOI = loss on ignition,
## O2 = oxygen, TC = total carbon, TN = total nitrogen, WQ = water quality
bd <- read_csv(data_filenames[grepl("BulkDensity", data_filenames)])
cdom <- read_csv("data/EC1_CDOM_spectral_indices.csv")
gwc <- read_csv(data_filenames[grepl("GWC", data_filenames)])
ions <- read_csv(data_filenames[grepl("Ions", data_filenames)])
loi <- read_csv(data_filenames[grepl("LOI", data_filenames)])
npoc_tdn <- read_csv(data_filenames[grepl("NPOC_TDN", data_filenames)])
o2_drawdown <- read_csv(data_filenames[grepl("OxygenDrawdown", data_filenames)])
soil_ph <- read_csv(data_filenames[grepl("Soil_pH", data_filenames)]) %>% 
  mutate(transect_location = str_to_title(transect_location)) %>%
  dplyr::rename("soil_ph" = ph, "soil_cond" = specific_conductance_us_cm)
tctn <- read_csv(data_filenames[grepl("TCTN", data_filenames)])
tss <- read_csv(data_filenames[grepl("TSS", data_filenames)]) %>%
  group_by(kit_id) %>%
  summarize(tss_mg_perl = sum(tss_mg_perl))
wq <- read_csv(data_filenames[grepl("WaterQuality", data_filenames)])


# 3. Read in and clean up metadata ---------------------------------------------

## Read in Kit-level metadata. Columns of interest are: 
kit_level_raw <- read_csv(metadata_filenames[grepl("KitLevel.csv", metadata_filenames)]) %>% 
  ## There's no K041 in the metadata, so add manually
  add_row(kit_id = "K041", region = "Chesapeake Bay") %>% 
  ## Fix a single problem cell with air temp (given as a range)
  mutate(air_temp_c = as.double(ifelse(air_temperature_c == "21.1111-25", mean(c(21.1, 25)), air_temperature_c)), 
         ## Recode weather as numeric
         weather = case_when(weather_conditions == "Sunny" ~ 1,
                             weather_conditions == "Cloudy" ~ 2,
                             weather_conditions == "Overcast" ~ 3,
                             weather_conditions == "NA" ~ NaN), 
         ## Convert barometric pressure to double
         pressure_inhg = as.double(barometric_pressure_inhg)) %>% 
  select(kit_id, region, air_temp_c, weather, pressure_inhg)
        
## Read in Collection-level data, but don't format because we aren't going to
## use any of these data
collection_data_raw <- read_csv(metadata_filenames[grepl("Data_CollectionLevel.csv", metadata_filenames)])


## Read in Collection-level metadata. This is a big ole file with lots of interesting
## things, but needs lots of TLC
collection_metadata_raw <- read_csv(metadata_filenames[grepl("Metadata_CollectionLevel.csv", metadata_filenames)]) 

## Manual cleanup of misleading system types
collection_metadata_raw[11, 'water_systemtype'] <- "Estuary" #Chesapeake Bay National 
# Estuarine Research Reserve. Away from Pamunkey R; tidal descriptor misleading.

collection_metadata_raw[24, 'water_systemtype'] <- "Lacestuary" #Houghton, MI.  Despite seiche, 
# tidal river characterization is misleading.

collection_metadata_raw[43, 'water_systemtype'] <- "Tidal River" #Upper Patuxent.  Label "fresh-
# -water" is redundant given conductivity data.

collection_metadata <- collection_metadata_raw %>% 
  ## Rename lat and long, we're using just water to keep things simpler
  mutate(latitude = water_latitude, 
         longitude = water_longitude, 
         ## Recode macro + algae to Both
         water_macrophytes_algae = ifelse(water_macrophytes_algae == "Macrophytes, Algae", 
                              "Both", 
                              water_macrophytes_algae)) %>% 
  ## Now integerize it
  mutate(macro_algae_num = case_when(water_macrophytes_algae == "None" ~ 1, 
                                     water_macrophytes_algae == "Algae" ~ 2, 
                                     water_macrophytes_algae == "Macrophytes" ~ 3, 
                                     water_macrophytes_algae == "Both" ~ 4, 
                                     water_macrophytes_algae == "NA" ~ NaN),
         water_systemtype = ifelse(water_systemtype == "Tidal Stream" |
                                      water_systemtype == "Tidal stream", 
                                   "Tidal River", water_systemtype), 
         water_systemtype_num = case_when(water_systemtype == "Tidal River" ~ 1, 
                                          water_systemtype == "Estuary" ~ 2, 
                                          water_systemtype == "Lacestuary" ~ 3, 
                                          water_systemtype == "Lake" ~ 3, 
                                          water_systemtype == "NA" ~ NaN), 
         h2s_num = case_when(
           sediment_rotten_egg_smell == "No" ~ 0,
           sediment_rotten_egg_smell == 'Yes' ~ 1,
           sediment_rotten_egg_smell == 'NA' ~ NaN)) %>% 
  select(kit_id, latitude, longitude, 
         water_macrophytes_algae, macro_algae_num, 
         water_systemtype, water_systemtype_num, 
         sediment_rotten_egg_smell, h2s_num)


## We're going to set up a helper function for integerizing soil horizons since
## we need to do the same thing for wetland, transition, and upland soils
integerize_horizons <- function(col){
  case_when(col == "H" ~ 1, 
            col == "O" ~ 2, 
            col == "A" ~ 3, 
            col == "C" ~ 4, 
            col == "NA" ~ NaN, 
            col == "" ~ NaN)
}

## Read in manually assigned soil types (per Donnie)
collection_metadata_soiltypes_raw <- read_csv("data/220713_soil_types_from_donnie.csv")

## Awkward bind_rows: tried a function that didn't work easily so brute-forced it
collection_metadata_soiltypes <- bind_rows(collection_metadata_soiltypes_raw %>% 
  mutate(transect_location = "Wetland", 
         horizon = wetland_soil_horizon, 
         horizon_des = `wt_Simple Horizon designation`,
         horizon_num = integerize_horizons(horizon_des)) %>% 
    select(kit_id, transect_location, horizon, horizon_des, horizon_num), 
  collection_metadata_soiltypes_raw %>% 
    mutate(transect_location = "Transition", 
           horizon = transition_soil_horizon, 
           horizon_des = `tr_Simple Horizon designation`,
           horizon_num = integerize_horizons(horizon_des)) %>% 
    select(kit_id, transect_location, horizon, horizon_des, horizon_num), 
  collection_metadata_soiltypes_raw %>% 
    mutate(transect_location = "Upland", 
           horizon = upland_soil_horizon, 
           horizon_des = `up_Simple Horizon designation`,
           horizon_num = integerize_horizons(horizon_des)) %>% 
    select(kit_id, transect_location, horizon, horizon_des, horizon_num))


## I don't have a more elegant solution, so I'm going to pivot_longer 3 times
# collection_metadata_soiltypes_raw %>% 
#   pivot_longer(cols = c(wetland_soiltype_num, wetland_soil_horizon), 
#                names_to = )

## Read in hex/rgb values inferred from sediment color (per Daniel)
collection_metadata_sedcolors <- read_csv("data/220713_sed_colors_from_daniel.csv")

## Now combine all metadata sheets
metadata <- full_join(collection_metadata_soiltypes, kit_level_raw, by = "kit_id")%>% 
  full_join(collection_metadata, by = "kit_id") %>% 
  full_join(collection_metadata_sedcolors, by = "kit_id")

regions <- metadata %>% 
  group_by(kit_id) %>% 
  summarize(region = first(region))


# 4. Join datasets -------------------------------------------------------------

## Read in greenhouse gases and set up for joining with O2
ghg <- read_csv("data/230417_ghg_final.csv")
  #mutate(transect_location = site) %>% 
  #select(-date, -site) %>% 
  #filter(!is.na(pco2_ppm_c) & !is.na(pch4_ppm_c) & !is.na(pn2o_ppm_c))

d_ghg <- ghg %>% 
  pivot_wider(names_from = type, 
              values_from = c(pco2_uM, pch4_nM, pn2o_uM)) %>% 
  mutate(d_pco2 = pco2_uM_wet - pco2_uM_dry, 
         d_pch4 = pch4_nM_wet - pch4_nM_dry, 
         d_pn2o = pn2o_uM_wet - pn2o_uM_dry) 

## Gather the gases
gases <- full_join(d_ghg, 
                   o2_drawdown %>% select(common_cols, delta_do_hr), 
                   by = common_cols) #18 kits represented

## Gather the water samples
water_datasets <- #ions %>% select(kit_id, contains("ppm")) %>%  
  cdom %>% select(kit_id, BIX, FI, HIX, SUVA254) %>% 
  full_join(wq %>% select(kit_id, sal_psu, ph, orp_mv, alk_mgl_caco3), by = "kit_id") %>% 
  full_join(npoc_tdn %>% select(kit_id, npoc_mgl, tdn_mgl), by = "kit_id")  %>% 
  full_join(tss %>% select(kit_id, tss_mg_perl), by = "kit_id") %>% 
  filter(kit_id %in% gases$kit_id)

## Gather the soil and sediment samples
soil_datasets <- bd %>% select(common_cols, bulk_density_g_cm3) %>%
full_join(gwc %>% select(common_cols, gwc_perc), by = common_cols) %>% 
  full_join(loi %>% select(common_cols, loi_perc), by = common_cols)  %>% 
  full_join(soil_ph %>% select(common_cols, soil_ph, soil_cond), by = common_cols) %>% 
  full_join(tctn %>% select(common_cols, tn_perc, tc_perc), by = common_cols) %>% 
  filter(kit_id %in% gases$kit_id)

master <- left_join(gases, water_datasets, by = "kit_id") %>% 
  left_join(soil_datasets, by = common_cols) %>% 
  left_join(metadata %>% select(-region), by = common_cols) %>% 
  left_join(regions, by = "kit_id") %>% # there's an issue where metadata does 
  ## not have rows for sediments, so region was NA for all sediments, so this
  ## is a work-around
  filter(!is.na(delta_do_hr)) %>% # we only want rows where we have O2 data (fewer  
  ## gaps than the ghgs)
filter(region != "Great Lakes")

## If you want to delete the downloaded files from your local, run line below:
#file.remove(c(data_filenames, metadata_filenames))


# 4. Write merged uploaded data to file ----------------------------------------
write_csv(master, "data/master_data_all.csv")

