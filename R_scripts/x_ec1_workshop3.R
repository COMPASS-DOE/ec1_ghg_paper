## Generate master data used for EC1 Workshop #3 and pre-bake some potential
## graphs
## 
## 2022-06-09
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, # keep it tidy
       cowplot, 
       corrplot, 
       PNWColors) #corr

## Set theme
theme_set(theme_bw())

## cheatcode to quick-reference 
common_cols = c("kit_id", "transect_location")

## Set a color theme
color_theme <- c("#1B264F", "#CE8147", "#FAF33E", "#3BB273")

# 2. Import and assemble dataset -----------------------------------------------

## Read in existing master dataset
masterdata_raw <- read_csv("data/master_data.csv")

## First remove ghg outliers
ghg_no_outliers <- masterdata_raw %>% 
  #select(common_cols, type, pco2_c, pch4_c, pn2o_c) %>% 
  ## IMPORTANT: first, I'm going to remove 4 outliers identified by plotting the data
  filter(pco2_c < 150000) %>% #removes K015 Upland-dry (CO2 = 208,251 ppm)
  filter(pch4_c < 15) %>% #removes K050 Wetland-wet (CH4 = 20.7 ppm)
  filter(pn2o_c < 400) %>% #removes K018 Upland-wet and K034 Upland-wet 
  mutate(transect_location = fct_relevel(transect_location, c("Sediment", "Wetland", "Transition", "Upland")))

## Calculate change in ghgs
d_ghg <- ghg_no_outliers %>%
  select(common_cols, type, pco2_c, pch4_c, pn2o_c) %>% 
  distinct() %>% 
  drop_na() %>% 
  pivot_wider(names_from = type, 
              values_from = c(pco2_c, pch4_c, pn2o_c)) %>% 
  mutate(d_pco2 = pco2_c_wet - pco2_c_dry, 
         d_pch4 = pch4_c_wet - pch4_c_dry, 
         d_pn2o = pn2o_c_wet - pn2o_c_dry)

## Now merge EVERYTHING into one thing
masterdata_all <- masterdata_raw %>% 
  filter(type == "dry") %>% #remove duplicates (everything except ghg is dups)
  inner_join(d_ghg, by = common_cols) %>% #join datasets
  mutate(macrophytes = case_when(water_macrophytes_algae == "None" ~ 1, 
                                 water_macrophytes_algae == "Macrophytes, Algae" ~ 2, 
                                 water_macrophytes_algae == "Macrophytes" ~ 3),
         system = case_when(water_systemtype == "Estuary" ~ 1, 
                            water_systemtype == "Estuary, Tidal River" ~ 1, 
                            water_systemtype == "Lacestuary" ~ 2, 
                            water_systemtype == "Tidal Stream" ~ 3), 
         rotten_eggs = case_when(sediment_rotten_egg_smell == "No" ~ 1, 
                                 sediment_rotten_egg_smell == "Yes" ~ 2))

count_what_we_have <- masterdata_all %>% 
  select(-pco2, -pch4, -pn2o, -pco2_c, -pch4_c, -pn2o_c) %>% 
  select(where(is.numeric)) %>%
  summarise_all(funs(sum(is.na(.)))) %>% 
  pivot_longer(cols = everything(), names_to = "par", values_to = "no_NAs") %>% 
  mutate(perc = 1 - (no_NAs / nrow(masterdata_all)))


## Create variable vectors to conveniently subset
gas_vars = c("delta_do_hr", "d_pco2", "d_pch4", "d_pn2o")
water_vars = c("sal_psu", "ph", "orp_mv", "alk_mgl_caco3", #bulk water quality
                "sulfate_ppm", "nitrate_ppm", "npoc_mgl", "tdn_mgl", "SUVA254", "FI", "HIX")
soil_vars = c("gwc_perc", "loi_perc", "bulk_density_g_cm3", "tc_perc", "tn_perc")
env_vars = c("latitude", "longitude", "macrophytes", "system", "rotten_eggs")
  
## Throw everything in 
df <- masterdata_all %>% select(common_cols, gas_vars, water_vars, soil_vars, env_vars)
#write_csv(df, "220609_masterdata_for_jmp.csv", na = "")


## Make specific datasets
df_water <- df %>% select(common_cols, gas_vars, water_vars)
df_soil <- df %>% select(common_cols, gas_vars, soil_vars)
df_env <- df %>% select(common_cols, gas_vars, env_vars)


# 3. Some potentially useful plots ---------------------------------------------
  
## What percent of data are available across the different data types? 
ggplot(count_what_we_have, aes(par, perc)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90))

## What do correlation plots look like for each subset? 
df_water %>% 
  select(-common_cols) %>% 
  drop_na() %>% 
  cor() %>% 
  corrplot()

## Correlation colored by site
ggplot(df, aes(sal_psu, delta_do_hr)) + 
  geom_point(aes(color = transect_location)) + 
  geom_smooth(method = "lm", se = F) + 
  scale_color_manual(values = color_theme)
  
## Correlations faceted by site
ggplot(df, aes(sal_psu, delta_do_hr, color = transect_location)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  facet_wrap(~transect_location) + 
  scale_color_manual(values = color_theme)






