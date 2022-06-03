## This script brings in all the publicly available datasets of use to this paper
## from https://drive.google.com/drive/folders/1m6fbCoOynP3pxi0GObSCG0_77mvYsu74
##
## 2022-06-02
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, ## keep things tidy
       googledrive) ## download files from google drive


# 2. Import datasets -----------------------------------------------------------

## The first step is getting the files local so we can read them in. NOTE: the first
## time you use functions from either 'googledrive' or 'googlesheets4' packages, 
## you will need to authorize via the Tidyverse API using an email address with
## access the EXCHANGE Google Drive folder.

## Set two paths for the two folders with data we want
data_path <- "https://drive.google.com/drive/folders/1r3sP7tvhG2btZACvxZUdtrtSiLVGUUGp"
metadata_path <- "https://drive.google.com/drive/folders/1IQUq_sD-Jama7ajaZl1zW_9zlWfyCohn"

## Next, create two lists of file names we'll use to download files
data_filenames <- drive_ls(data_path) %>% pull(name)
metadata_filenames <- drive_ls(metadata_path) %>% pull(name)

## Now, download those files to your local
lapply(data_filenames, drive_download, overwrite = T)
lapply(metadata_filenames, drive_download, overwrite = T)

## We are finally ready to read data into R! Read in each file of interest: 
## BD = bulk density, GWC = gravimetric water content, LOI = loss on ignition,
## O2 = oxygen, TC = total carbon, TN = total nitrogen, WQ = water quality
bd <- read_csv(data_filenames[grepl("BulkDensity", data_filenames)])
cdom <- read_csv(data_filenames[grepl("CDOM.*csv", data_filenames)]) %>% 
  rename("a254" = `Abs 254nm`)
gwc <- read_csv(data_filenames[grepl("GWC", data_filenames)])
ions <- read_csv(data_filenames[grepl("Ions", data_filenames)])
metadata_raw <- read_csv(metadata_filenames[grepl("Metadata_Collection", metadata_filenames)])
regions <- read_csv(metadata_filenames[grepl("KitLevel", metadata_filenames)]) %>%
  mutate(kit_id = paste0("K", kit_id)) %>% 
  select(kit_id, region) %>% 
  add_row(kit_id = "K041", region = "Chesapeake Bay")
loi <- read_csv(data_filenames[grepl("LOI", data_filenames)])
npoc_tdn <- read_csv(data_filenames[grepl("NPOC_TDN", data_filenames)])
o2_drawdown <- read_csv(data_filenames[grepl("OxygenDrawdown", data_filenames)])
soil_ph <- read_csv(data_filenames[grepl("Soil_pH", data_filenames)]) %>% 
  mutate(transect_location = str_to_title(transect_location))
tctn <- read_csv(data_filenames[grepl("TCTN", data_filenames)])
tss <- read_csv(data_filenames[grepl("TSS", data_filenames)]) %>% 
  group_by(kit_id) %>% 
  summarize(tss_mg_perl = sum(tss_mg_perl))
wq <- read_csv(data_filenames[grepl("WaterQuality", data_filenames)])

## Most datasets are ready, but metadata needs a little cleanup love before joining
metadata <- metadata_raw %>% 
  mutate(latitude = water_latitude, longitude = water_longitude) %>% 
  select(kit_id, latitude, longitude, water_macrophytes_algae, water_systemtype, 
         sediment_rotten_egg_smell)

# 3. Join datasets -------------------------------------------------------------

## To make things simple, define common columns used to join datasets
common_cols = c("kit_id", "transect_location")

## Read in greenhouse gases and set up for joining with O2
ghg <- read_csv("data/ghg.csv") %>% 
  mutate(transect_location = site) %>% 
  select(-date, -site) %>% 
  filter(!is.na(pco2) & !is.na(pch4) & !is.na(pn2o))


## Gather the gases
gases <- full_join(ghg, o2_drawdown %>% select(common_cols, delta_do_hr), by = common_cols) %>% 
  left_join(regions %>% select(kit_id, region), by = "kit_id")

## Gather the water samples
water_datasets <- full_join(ions %>% select(kit_id, contains("ppm")), 
                            cdom %>% select(kit_id, a254, BIX, FI, HIX, SUVA254), by = "kit_id") %>% 
  full_join(wq %>% select(kit_id, sal_psu, ph, orp_mv, alk_mgl_caco3), by = "kit_id") %>% 
  full_join(npoc_tdn %>% select(kit_id, npoc_mgl, tdn_mgl), by = "kit_id")  %>% 
  full_join(tss %>% select(kit_id, tss_mg_perl), by = "kit_id")

## Gather the soil and sediment samples
soil_datasets <- full_join(bd %>% select(common_cols, bulk_density_g_cm3), 
                         gwc %>% select(common_cols, gwc_perc), by = common_cols) %>% 
  full_join(loi %>% select(common_cols, loi_perc), by = common_cols)  %>% 
  full_join(soil_ph %>% select(common_cols, ph, specific_conductance_us_cm), by = common_cols) %>% 
  full_join(tctn %>% select(common_cols, tn_perc, tc_perc), by = common_cols) %>% 
  full_join(metadata, by = "kit_id")

master <- left_join(gases, water_datasets, by = "kit_id") %>% 
  left_join(soil_datasets, by = common_cols)

## If you want to delete the downloaded files from your local, run lines below:
#file.remove(c(data_filenames, metadata_filenames))


# 4. Write merged uploaded data to file ----------------------------------------
write_csv(master, "data/master_data.csv")


