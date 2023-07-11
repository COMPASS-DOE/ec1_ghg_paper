## This script constructs random forests using Boruta for variable selection
##
## 2023-01-12
## Peter Regier
##
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, bestNormalize, cowplot, 
       corrplot, ggcorrplot, ggfortify, ggpubr,
       janitor,
       infotheo, ranger, Boruta)

## Character variable vector
chars <- c("kit_id", "transect_location")
common_cols <- c("delta_do_hr", "d_pco2", "d_pch4", "d_pn2o")

## Set theme
theme_set(theme_bw())


# 2. Bring in data -------------------------------------------------------------

df_raw <- read_csv("data/master_data.csv") %>% 
  mutate(transect_num = case_when(transect_location == "Sediment" ~ 1, 
                                  transect_location == "Wetland" ~ 2,
                                  transect_location == "Transition" ~ 3,
                                  transect_location == "Upland" ~ 4)) %>% 
  fill(macro_algae_num, .direction = "up") %>% 
  filter(region == "Chesapeake Bay") %>% 
  filter(d_pn2o < 500) %>% 
  filter(d_pco2 > -100000) %>% 
  filter(d_pch4 < 10)

df_models <- df_raw %>% select(all_of(common_cols), transect_num, 
                               npoc_mgl, tdn_mgl, nitrate_ppm, sulfate_ppm, sal_psu,
                               gwc_perc, loi_perc) %>% 
  drop_na()


#3. Determine variables to use -------------------------------------------------

## Function to reformat Boruta output
pick_vars <- function(data){
  as.data.frame(data) %>% 
    tibble::rownames_to_column() %>%
    rename("decision" = 2) %>% 
    as_tibble() %>% 
    filter(decision == "Confirmed") %>% 
    pull(rowname)
}

## Set up vars
do_vars <- pick_vars(Boruta(delta_do_hr ~ ., data = df_models, doTrace = 2)$finalDecision)
co2_vars <- pick_vars(Boruta(d_pco2 ~ ., data = df_models, doTrace = 2)$finalDecision)
no2_vars <- pick_vars(Boruta(d_pch4 ~ ., data = df_models, doTrace = 2)$finalDecision)
ch4_vars <- pick_vars(Boruta(d_pn2o ~ ., data = df_models, doTrace = 2)$finalDecision)


# 4. Make some models ----------------------------------------------------------

do_model <- ranger(delta_do_hr ~., data = df_models %>% select(delta_do_hr, do_vars), importance = "impurity")
co2_model <- ranger(d_pco2 ~., data = df_models %>% select(d_pco2, co2_vars), importance = "impurity")
n2o_model <- ranger(d_pch4 ~., data = df_models %>% select(d_pch4, no2_vars), importance = "impurity")
ch4_model <- ranger(d_pn2o ~., data = df_models %>% select(d_pn2o, ch4_vars), importance = "impurity")



as.data.frame(do_model$variable.importance) %>% 
  tibble::rownames_to_column() %>%
  rename("vi" = `do_model$variable.importance`) %>% 
  as_tibble() %>% 
  mutate(n_vi = vi / sum(vi)) %>% 
  ggplot(aes(n_vi, rowname)) + geom_col()


do_model_all <- ranger(delta_do_hr ~., data = df_models, importance = "impurity")







