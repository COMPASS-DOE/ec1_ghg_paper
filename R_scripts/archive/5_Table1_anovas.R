## This script makes a whole bunch of ANOVAs
## 
## 2023-03-08
## Peter Regier
##
# ############ #
# ############ #


# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, 
       cowplot, 
       corrplot, 
       broom,
       ggcorrplot, 
       ggfortify, 
       ggpubr)

theme_set(theme_bw())


# 2. Bring in data -------------------------------------------------------------

df_raw <- read_csv("data/master_data.csv")

climate_raw <- read_csv("data/230223_climate_data.csv")

soil_raw <- read_csv("data/230223_soil_data.csv")

lulc_raw <- read_csv("data/230315_lulc.csv") %>% 
  mutate(veg_factor = as.factor(case_when(vegetation_class == "Open tree canopy" ~ "open_canopy", 
                                vegetation_class == "Closed tree canopy" ~ "closed_canopy",
                                vegetation_class == "Herbaceous - grassland" ~ "grass",
                                TRUE ~ "barren")),
         veg_2factor = as.factor(case_when(vegetation_class == "Open tree canopy" ~ "trees", 
                                          vegetation_class == "Closed tree canopy" ~ "trees",
                                          vegetation_class == "Herbaceous - grassland" ~ "no_trees",
                                          TRUE ~ "no_trees")),
         lifeform_cat = as.factor(case_when(vegetation_lifeform == "Shrub" ~ "Tree", 
                                            vegetation_lifeform == "Agriculture" ~ "Developed", 
                                            TRUE ~ vegetation_lifeform)))

lulc_raw %>% group_by(lifeform_cat) %>% count()

## Set the number of bins for factorizing variables (4 matches number of transect locations)
no_of_cuts = 4

df1 <- inner_join(df_raw, climate_raw, by = "kit_id") %>% 
  inner_join(soil_raw, by = "kit_id") %>% 
  inner_join(lulc_raw, by = c("kit_id", "transect_location")) %>% 
  mutate(transect_cat = as.factor(case_when(transect_location == "Sediment" ~ "Wetter", 
                                            transect_location == "Wetland" ~ "Wetter",
                                            transect_location == "Transition" ~ "Drier",
                                            transect_location == "Upland" ~ "Drier"))) %>% 
  mutate(lat_lon = lat * lon) %>% 
  mutate(sal_cat = as.factor(ntile(sal_psu, no_of_cuts)), 
         gwc_cat = as.factor(ntile(gwc_perc, no_of_cuts)), 
         loi_cat = as.factor(ntile(loi_perc, no_of_cuts)), 
         lat_cat  = as.factor(ntile(lat, no_of_cuts)), 
         lon_cat  = as.factor(ntile(lon, no_of_cuts)), 
         lat_lon_cat  = as.factor(ntile(lat_lon, no_of_cuts)), 
         temp_cat  = as.factor(ntile(mat_c, no_of_cuts)), 
         precip_cat  = as.factor(ntile(monthly_precip, no_of_cuts)), 
         sand_cat  = as.factor(ntile(sand, no_of_cuts)), 
         silt_cat  = as.factor(ntile(silt, no_of_cuts)), 
         clay_cat  = as.factor(ntile(clay, no_of_cuts)))

# x. Set up functions ----------------------------------------------------------

## Set up a matrix to feed the function
response_var <- c("delta_do_hr", 
                  "pco2_uM_wet", "pco2_uM_dry",
                  "pch4_nM_wet", "pch4_nM_dry",
                  "pn2o_uM_wet", "pn2o_uM_dry")


factorial_anova <- function(response_var){
  
  ## Drivers for 2-way 
  # driver_vars <- c("transect_cat", "sal_cat", "gwc_cat", "loi_cat",
  #                  "lat_cat", "lon_cat", 
  #                  "temp_cat", "precip_cat", 
  #                  "sand_cat", "silt_cat", "clay_cat", "veg_2factor")
  
  ## Drivers for 4-way
  driver_vars <- c("transect_location", "sal_cat", "gwc_cat", "loi_cat", "veg_factor",
                   "lat_cat", "lon_cat", "lat_lon_cat",
                   "temp_cat", "precip_cat",
                   "sand_cat", "silt_cat", "clay_cat")
  
  x <- df1 %>% 
    select(response_var, all_of(driver_vars)) %>% 
    drop_na()
  
  f <- as.formula(paste(response_var, paste(driver_vars, collapse = " + "), sep = "~"))
  
  tidy(aov(f, data = x)) %>% 
    filter(term != "Residuals") %>% 
    select(term, p.value) %>% 
    #rename(!!sym(response_var) := p.value) %>% 
    mutate(response = response_var)
    
}

anovas <- response_var %>% 
  map(factorial_anova) %>% 
  bind_rows()

aov_table_all <- anovas %>% 
  mutate(sig = case_when(p.value < 0.001 ~ "***",
                         p.value < 0.01 ~ "**",
                         p.value < 0.1 ~ "*",
                         TRUE ~ "ns")) %>% 
  mutate(p_value = paste(round(p.value, 3), sig)) %>% 
  mutate(p_value = ifelse(p_value == "0 ***", "<0.001 ***", p_value)) %>% 
  rename("driver" = term) %>% 
  select(driver, response, p_value) %>% 
  pivot_wider(names_from = "response", values_from = "p_value")


## Select the drivers to include
keepers <- c("transect_location", "sal_cat", "gwc_cat", "loi_cat", "veg_factor", 
             "lat_cat", "temp_cat", "sand_cat", "clay_cat")

aov_table <- aov_table_all %>% 
  filter(driver %in% keepers)


