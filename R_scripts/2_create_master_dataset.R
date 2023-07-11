## This script brings in all the publicly available datasets of use to this paper
## from https://drive.google.com/drive/folders/1m6fbCoOynP3pxi0GObSCG0_77mvYsu74
##
## 2022-06-02 (updated 2023-06-22)
## Donnie Day, Peter Regier, Daniel Sandborn
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, ## keep things tidy
       ggallin,
       googledrive) ## download files from google drive

## Set up common columns that are routinely used to in select() and join() calls
common_cols = c("kit_id", "transect_location")


# 2. Import gas datasets -------------------------------------------------------

## Import GHG data
ghg_raw <- read_csv("data/230623_ghg_final_dataset.csv") %>% 
  select(common_cols, contains("uM_hr"))

## Read in DO consumption 
do_raw <- read_csv("data/230417_do_consumption_final.csv") %>% 
  filter(region == "CB")

## Gather the gases
gases <- inner_join(ghg_raw, 
                    do_raw, 
                   by = common_cols) %>%  #14 kits represented
  mutate(transect_location = str_to_lower(transect_location))


# 3. Import ESS-DIVE datasets -------------------------------------------------

## Using data from the ESS-DIVE package
## Data provided 5/3/22
essdive_metadata_filepath = "data/ess_dive_package/ec1_metadata_v1/"
essdive_sediment_filepath = "data/ess_dive_package/ec1_sediment_v1/"
essdive_soil_filepath = "data/ess_dive_package/ec1_soil_v1/"
essdive_water_filepath = "data/ess_dive_package/ec1_water_v1/"

## GWC ###########
gwc_sediment <- read_csv(paste0(essdive_sediment_filepath, "ec1_sediment_gwc_L2.csv")) 
gwc_soil <- read_csv(paste0(essdive_soil_filepath, "ec1_soil_gwc_L2.csv"))
gwc <- bind_rows(gwc_sediment, gwc_soil) %>% 
  rename("gwc_perc" = moisturecontent_perc_drywtbasis)

## TC/TN for soils (from ESS-DIVE) ###########
tc_soil <- read_csv(paste0(essdive_soil_filepath, "ec1_soil_tc_L2.csv")) %>% 
  rename("tc_perc" = carbon_weight_perc) %>% select(-campaign)
tn_soil <- read_csv(paste0(essdive_soil_filepath, "ec1_soil_tn_L2.csv")) %>% 
  rename("tn_perc" = nitrogen_weight_perc) %>% select(-campaign)
tc_tn_soil <- full_join(tc_soil, tn_soil, by = c("kit_id", "transect_location"))

## TC/TN for sediments: this one is a little tricky because we will need one
##  data-source from outside the ESS-DIVE package
read_tctn_file <- function(filename){
  read_delim(paste0("data/sediment_tctn/", filename), 
             skip = 12) %>% 
    slice(4:n()) %>% 
    mutate(kit_id = str_sub(sample, 1, 4)) %>% 
    select(kit_id, sample, sample_id, tn_perc, tc_perc) %>% 
    mutate(tn_perc = as.numeric(tn_perc), 
           tc_perc = as.numeric(tc_perc))
}

## Create raw Sediment TC/TN dataset
tc_tn_sediment_raw <- bind_rows(read_tctn_file("AD_SULI_EC1_Mixed_Ac_R4_4_11_2023.txt"), 
          read_tctn_file("EC1_Sediments_Mixed_UnAc_R1_4_10_2023.txt")) %>% 
  filter(grepl("SED", sample))

tc_tn_sediment <- tc_tn_sediment_raw %>% 
  group_by(kit_id) %>% 
  dplyr::summarize(tc_perc = mean(tc_perc), 
            tn_perc = mean(tn_perc)) %>% 
  mutate(transect_location = "sediment") %>% 
  select(colnames(tc_tn_soil))

## Make full TC/TN dataset
tc_tn <- bind_rows(tc_tn_sediment, tc_tn_soil)

## Water quality ###########
doc <- read_csv(paste0(essdive_water_filepath, "ec1_water_doc_L2.csv")) %>% 
  rename("doc_mgl" = doc_mgC_L) %>% select(-campaign)
ph <- read_csv(paste0(essdive_water_filepath, "ec1_water_ph_L2.csv")) %>% 
  select(-campaign)
salinity <- read_csv(paste0(essdive_water_filepath, "ec1_water_salinity_L2.csv")) %>% 
  select(-campaign)
alkalinity <- read_csv(paste0(essdive_water_filepath, "ec1_water_alkalinity_L2.csv")) %>% 
  select(-campaign)
tdn <- read_csv(paste0(essdive_water_filepath, "ec1_water_tdn_L2.csv")) %>% 
  rename("tdn_mgl" = tdn_mgN_L) %>% select(-campaign)
tss <- read_csv(paste0(essdive_water_filepath, "ec1_water_tss_L2.csv")) %>% 
  rename("tss_mgl" = tss_mg_L) %>% select(-campaign)
wq <- full_join(doc, tdn, by = c("kit_id", "transect_location")) %>% 
  full_join(ph, by = c("kit_id", "transect_location")) %>% 
  full_join(salinity, by = c("kit_id", "transect_location")) %>% 
  full_join(alkalinity, by = c("kit_id", "transect_location")) %>% 
  full_join(tss, by = c("kit_id", "transect_location")) %>% 
  select(-transect_location)


## Finally, bind all these datasets together -----------------------------------

ancillary <- full_join(gwc, tc_tn, by = common_cols) 


# 4. Bring in metadata ---------------------------------------------------------

collection_level <- read_csv(paste0(essdive_metadata_filepath, "ec1_metadata_collectionlevel.csv")) %>% 
  select(all_of(common_cols), latitude, longitude, elevation_m)


# 4. Merge and write out master dataset ----------------------------------------

## Create master dataset
master <- left_join(gases, ancillary, by = common_cols) %>% 
  left_join(wq, by = "kit_id") %>% 
  left_join(collection_level, by = common_cols)

## Write
write_csv(master, "data/230623_master_data.csv")

