## Play around with SEM and see if something falls out - not initially
## https://lavaan.ugent.be/tutorial/ - official lavaan tutorial
## https://stats.oarc.ucla.edu/r/seminars/rsem/ - useful lavaan tutorial
## https://rdrr.io/cran/tidySEM/f/README.md - graph_sem info
## https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2435.12928 - Fig 2 is inspiration

## 2023-03-08
## Peter Regier
## 
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, 
       ranger, 
       PNWColors)


# 2. Load data -----------------------------------------------------------------

## Load in all data
df_raw <- read_csv("data/master_data.csv") %>% 
  mutate(transect_cat = as.factor(case_when(transect_location == "Sediment" ~ "Wetter", 
                                            transect_location == "Wetland" ~ "Wetter",
                                            transect_location == "Transition" ~ "Drier",
                                            transect_location == "Upland" ~ "Drier")), 
         transect_num = as.factor(case_when(transect_location == "Sediment" ~ "1", 
                                            transect_location == "Wetland" ~ "2",
                                            transect_location == "Transition" ~ "3",
                                            transect_location == "Upland" ~ "4")),) %>% 
  mutate(sal_cat = as.factor(ntile(sal_psu, 2))) %>% 
  filter(pch4_nM_wet < 20) %>% 
  filter(pn2o_uM_wet < 10)

climate_raw <- read_csv("data/230223_climate_data.csv")

soil_raw <- read_csv("data/230223_soil_data.csv")

df1 <- inner_join(df_raw, climate_raw, by = "kit_id") %>% 
  inner_join(soil_raw, by = "kit_id")


###########################################################################
# Experiment to see if SEM is an alternative here
# Modeling after https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2435.12928 Fig 2
# and a bit of https://www.sciencedirect.com/science/article/pii/S0013935120304692?via%3Dihub#fig4 Fig 4

p_load(lavaan,
       tidySEM)


## Set up a dataset with all needed variables
dat <- df1 %>% 
  dplyr::select(delta_do_hr, pco2_uM_wet, #responses
                mat_c, monthly_precip, lat, lon, #drivers
                sand, silt, clay, #drivers
                sal_psu, ph, gwc_perc, loi_perc) %>% #latents
  drop_na()

normalize_ <- function(var){
  ({{var}} - min({{var}})) / 
    (max({{var}}) - min({{var}}))
}


dat1 <- dat %>% 
  select(delta_do_hr, lat, sand, sal_psu, gwc_perc) %>% 
  mutate(across(everything(), normalize_))

cov(dat1)

summary(lm(delta_do_hr ~ sal_psu, data = dat1))

HS.model <- 'delta_do_hr  ~ sal_psu + gwc_perc
             sal_psu ~ sand + lat
             gwc_perc ~ sand + lat'
fit <- cfa(HS.model, data=dat1)

graph_sem(model = fit)


m1b <-   '
  # regressions
    delta_do_hr ~ gwc_perc
  # variance (optional)
    gwc_perc ~~ loi_perc
'
fit1b <- sem(m1b, data = dat1)

summary(fit1b)








