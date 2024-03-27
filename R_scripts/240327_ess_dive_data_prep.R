## Reformat data for ESS-DIVE

require(pacman)
p_load(tidyverse, 
       janitor)

common_cols = c("kit_id", "transect_location")

## Read raw data
ghg_raw <- read_csv("data/230623_ghg_final_dataset.csv")
do_raw <- read_csv("data/230417_do_consumption_final.csv")


## Bring in the raw data so we can compare means and standard deviations (in ppm)
## to ballpark the accuracy of each gas to determine the number of decimal points
ghg_sd <- read_csv("data/230623_ghg_raw_dataset.csv", na = c("#VALUE!", "#DIV/0!")) %>% 
  clean_names() %>% 
  select(contains("corrected")) %>% 
  drop_na() %>% 
  summarize(across(where(is.numeric), mean))

## Manually estimate accuracy based on average means and standard deviations in 
## raw data
co2_sig <- 54.3/14614
ch4_sig <- 0.0382/2.34
n2o_sig <- 0.166/28.1

## Calculate order-of-magnitude accuracy
# mean(ghg1$co2_uM_hr) * co2_sig ## CO2: 0.01 (2 decimals)
# mean(ghg1$ch4_uM_hr) * ch4_sig ## CH4: 0.001 (3 decimals)
# mean(ghg1$n2o_uM_hr) * n2o_sig ## CH4: 0.0001 (4 decimals)

## make transect_location values lowercase
ghg1 <- ghg_raw %>% 
  mutate(campaign = "EC1") %>% 
  select(campaign, all_of(common_cols), contains("uM_hr")) %>% 
  mutate(transect_location = str_to_lower(transect_location)) %>% 
  mutate(co2_uM_hr = round(co2_uM_hr, digits = 2), 
         ch4_uM_hr = round(ch4_uM_hr, digits = 3), 
         n2o_uM_hr = round(n2o_uM_hr, digits = 4)) 

## DO resolution is 0.01 mg/L, so convert to uM (31 uM = 1 mg/L), so 0.3 uM
mean(do_raw$do_uM_hr / do_raw$do_mgL_hr)

do1 <- do_raw %>% 
  mutate(campaign = "EC1", 
         transect_location = str_to_lower(transect_location), 
         do_uM_hr = round(do_uM_hr, digits = 1)) %>% 
  select(campaign, kit_id, transect_location, do_uM_hr) 

## Split by soil and sediment
do_soil <- do1 %>% filter(transect_location != "sediment")
do_sediment <- do1 %>% filter(transect_location == "sediment")

ghg_soil <- ghg1 %>% filter(transect_location != "sediment")
ghg_sediment <- ghg1 %>% filter(transect_location == "sediment")

write_csv(do_soil, "data/ec1_ghg_ess_data_package/ec1_soil_oxygen_drawdown_L2.csv")
write_csv(do_sediment, "data/ec1_ghg_ess_data_package/ec1_sediment_oxygen_drawdown_L2.csv")
write_csv(ghg_soil, "data/ec1_ghg_ess_data_package/ec1_soil_greenhouse_gases_L2.csv")
write_csv(ghg_sediment, "data/ec1_ghg_ess_data_package/ec1_sediment_greenhouse_gases_L2.csv")

length(unique(do1$kit_id))


