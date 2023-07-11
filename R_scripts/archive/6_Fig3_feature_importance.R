## Make RF models
## 
## 2023-05-03
## Peter Regier
## 
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, 
       tidymodels, 
       ranger, 
       automl,
       Boruta,
       PNWColors)


# 2. Load data -----------------------------------------------------------------

## Load in all data
df_raw <- read_csv("data/230503_master_data.csv") %>% 
  mutate(transect_location = str_to_sentence(transect_location)) %>% 
  mutate(transect_cat = as.factor(case_when(transect_location == "Sediment" ~ "Wetter", 
                                            transect_location == "Wetland" ~ "Wetter",
                                            transect_location == "Transition" ~ "Drier",
                                            transect_location == "Upland" ~ "Drier")), 
         transect_num = case_when(transect_location == "Sediment" ~ 1, 
                                            transect_location == "Wetland" ~ 2,
                                            transect_location == "Transition" ~ 3,
                                            transect_location == "Upland" ~ 4)) %>% 
  mutate(sal_cat = as.factor(ntile(sal_psu, 2))) 

climate_raw <- read_csv("data/230223_climate_data.csv")

soil_raw <- read_csv("data/230223_soil_data.csv")

df1 <- inner_join(df_raw, climate_raw, by = "kit_id") %>% 
  inner_join(soil_raw, by = "kit_id")





x <- df1 %>% 
  select("delta_do_hr", 
         contains("_wet"), 
         #contains("d_"),
                    "transect_num",
                    "gwc_perc", "tc_perc", "tn_perc", 
                    lat, lon, sal_psu, ph, doc_mgl, tdn_mgl,
                    sand, silt, clay, monthly_precip, mat_c) %>% 
  drop_na()

Boruta(delta_do_hr ~ ., data = x)
ranger(delta_do_hr ~ ., data = x, importance = "impurity")$r.squared

y <- x %>% recipe(pco2_uM_wet ~ .) %>% 
  #step_corr(all_predictors()) %>%
  step_normalize(all_predictors(), all_outcomes()) %>% 
  prep() %>% 
  bake(x) 

Boruta(pco2_uM_wet ~ ., data = y)

#pn2o_uM_wet, sand, silt

rf_co2 <- rand_forest(trees = 1000, mtry = tune(), mode = "regression") %>%
  set_engine("ranger", importance = "impurity") %>%
  fit(pco2_uM_wet ~ sal_psu + tc_perc + transect_num + silt, data = y)

as.data.frame(rf_co2$variable.importance) %>% 
  tibble::rownames_to_column() %>%
  rename("vi" = `rf_co2$variable.importance`) %>% 
  as_tibble() %>% 
  mutate(n_vi = vi / sum(vi), 
         dependent = "pCO2")



## Pick up here: next step is to normalize all these data and see if that helps
do_recipe <- rf_potential %>% 
  recipe(delta_do_hr ~ .) %>% 
  step_corr(all_predictors()) %>%
  step_normalize(all_predictors(), all_outcomes()) %>% 
  prep()

bake_do <- do_recipe %>% 
  bake(rf_potential)

potential_vars <- Boruta(delta_do_hr ~ ., data = rf_potential)
Boruta(delta_do_hr ~ ., data = bake_do)

ranger(delta_do_hr ~ ., data = rf_potential, importance = "impurity")$r.squared
ranger(delta_do_hr ~ ., data = bake_do, importance = "impurity")$r.squared



## Set seed for before making model
set.seed(42)

## Make the actual model
rf_model <- rand_forest(trees = 1000, mtry = tune(), mode = "regression") %>%
  set_engine("ranger", importance = "impurity") %>%
  fit(delta_do_hr ~ ., data = bake_do)


Boruta(delta_do_hr ~ ., data = rf_potential)

rf <- df1 %>% 
  dplyr::select(transect_num, sal_psu,
                delta_do_hr, pco2_uM_wet, 
                "gwc_perc", "tc_perc", "tn_perc", 
                silt, sand, clay, bd) %>% 
  drop_na()



rf_do <- ranger(delta_do_hr ~ ., data = bake_do, importance = "impurity")
rf_co2 <- ranger(pco2_uM_wet ~ ., data = rf %>% select(-delta_do_hr), importance = "impurity")

rf_do$r.squared
rf_co2$r.squared



predictors <- bind_rows(as.data.frame(rf_do$variable.importance) %>% 
                          tibble::rownames_to_column() %>%
                          rename("vi" = `rf_do$variable.importance`) %>% 
                          as_tibble() %>% 
                          mutate(n_vi = vi / sum(vi), 
                                 dependent = "DO"), 
                        as.data.frame(rf_co2$variable.importance) %>% 
                          tibble::rownames_to_column() %>%
                          rename("vi" = `rf_co2$variable.importance`) %>% 
                          as_tibble() %>% 
                          mutate(n_vi = vi / sum(vi), 
                                 dependent = "pCO2")) 
 
                        # as.data.frame(rf_ch4$variable.importance) %>% 
                        #   tibble::rownames_to_column() %>%
                        #   rename("vi" = `rf_ch4$variable.importance`) %>% 
                        #   as_tibble() %>% 
                        #   mutate(n_vi = vi / sum(vi), 
                        #          dependent = "pCH4"), 
                        # as.data.frame(rf_n2o$variable.importance) %>% 
                        #   tibble::rownames_to_column() %>%
                        #   rename("vi" = `rf_n2o$variable.importance`) %>% 
                        #   as_tibble() %>% 
                        #   mutate(n_vi = vi / sum(vi), 
                        #          dependent = "pN2O"))

## set up a color scheme for water quality
pred_vars <- unique(predictors$rowname)
pred_colors <- PNWColors::pnw_palette("Bay", n = length(pred_vars))

fi_do <- ggplot(predictors %>% filter(dependent == "DO"), 
                aes(n_vi*100, fct_reorder(rowname, n_vi), fill = rowname)) + 
  geom_col(alpha = 0.5, color = "black", show.legend = F) + 
  labs(x = "Importance (%)", y = "Predictor", title = "DO")

fi_co2 <- ggplot(predictors %>% filter(dependent == "pCO2"), 
                 aes(n_vi*100, fct_reorder(rowname, n_vi), fill = rowname)) + 
  geom_col(alpha = 0.5, color = "black", show.legend = F) + 
  labs(x = "Importance (%)", y = "Predictor", title = "pCO2")

# fi_ch4 <- ggplot(predictors %>% filter(dependent == "pCH4"), 
#                  aes(n_vi*100, fct_reorder(rowname, n_vi), fill = rowname)) + 
#   geom_col(alpha = 0.5, color = "black", show.legend = F) + 
#   labs(x = "Importance (%)", y = "Predictor", title = "pCH4")
# 
# fi_n2o <- ggplot(predictors %>% filter(dependent == "pN2O"), 
#                  aes(n_vi*100, fct_reorder(rowname, n_vi), fill = rowname)) + 
#   geom_col(alpha = 0.5, color = "black", show.legend = F) + 
#   labs(x = "Importance (%)", y = "Predictor", title = "pN2O")

plot_grid(fi_do, fi_co2, ncol = 2)
ggsave("figures/5_Fig5_feature_importance.png", width = 6, height = 3)



