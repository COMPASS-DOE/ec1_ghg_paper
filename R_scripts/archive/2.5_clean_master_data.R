## This script does the cleaning of outliers etc so that all scripts after this
## use the same dataset
##
## 2023-03-20 (Updated 2023-05-03)
## Peter Regier
##
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(tidyverse, 
       outliers)

theme_set(theme_bw())


# 2. Read in raw data ----------------------------------------------------------

master_raw <- read_csv("data/230503_master_data_untrimmed.csv")


# 3. Check columns for outliers ------------------------------------------------

## Removing outliers that 1) fail grubbs.test() and 2) are more than 2x the nearest value
## Note: for pn2o_uM_wet, there are values at 14.1 and 13.8 (nearest neighbor: 2.4)
## Not removing for the time being cause it might be real, but that's sketch?

## Code for checking outliers
### Run Grubb's test
# grubbs.test(master_raw$pn2o_uM_wet)
# ### Plot to visualize
# ggplot(master_raw, aes(pch4_nM_wet)) + geom_boxplot()
### Pull offending rows
# master_raw %>% filter(pco2_uM_wet > 3)


master_final <- master_raw %>% 
 # filter(pco2_uM_wet < 2000) %>% # remove 2016 uM (K030 UP) - next is 1404
 # filter(pco2_uM_dry < 4000) %>% # remove 8641 uM (K015 UP) - next is 3627
  #filter(pch4_nM_wet < 20) %>% # remove 25.7 (K050 W) - next is 11.8 
  #filter(pn2o_uM_wet < 10) %>% # remove 13.8 (K034 U) - next is 3.67
  filter(region == "CB") ## Final step: remove LE since we're not using here.
 
write_csv(master_final, "data/230503_master_data.csv")

