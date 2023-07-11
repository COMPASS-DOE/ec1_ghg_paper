## Random forests v2
##
## Peter Regier
## 2023-03-15
##
# ########## #
# ########## #

# 1. Setup --------------------------------

require(pacman)
p_load(tidyverse, 
       Boruta,
       cowplot,
       PNWColors,
       ranger)

theme_set(theme_bw())


# 2. Set up data --------------------------

df_raw <- read_csv("data/master_data.csv") %>% 
  filter(pch4_nM_wet < 20) %>% 
  filter(pn2o_uM_wet < 10)

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


# 3. Construct models -----------------------------------

## Set up a matrix to feed the function
response_var <- c("delta_do_hr", 
                  "pco2_uM_wet", "pco2_uM_dry",
                  "pch4_nM_wet", "pch4_nM_dry",
                  "pn2o_uM_wet", "pn2o_uM_dry")


make_rf_model <- function(response){
  
  ## Drivers for 4-way
  driver_vars <- c("transect_location", "sal_psu",
                   "gwc_perc", "loi_perc",
                   "vegetation_class",
                   "mat_c", 
                   "sand", "silt")
  
  ## Drivers for 4-way
  # driver_vars <- c("transect_location", "sal_cat", "gwc_cat", "loi_cat", "veg_factor",
  #                  "temp_cat", "sand_cat", "silt_cat")
  
  ## Third, create a new dataframe that only includes the important variables
  x <- df1 %>% 
    select(response, all_of(driver_vars)) %>% 
    drop_na()
  
  ## Make the formula
  f <- as.formula(paste(response, paste(driver_vars, collapse = " + "), sep = "~"))
  
  ## Make the model
  rf <- ranger(f, data = x, importance = "impurity")
  
  r2 <- rf$r.squared
  
  var_importance <- as.data.frame(rf$variable.importance) %>%
      tibble::rownames_to_column() %>%
      rename("vi" = `rf$variable.importance`) %>%
      as_tibble() %>%
      mutate(n_vi = vi / sum(vi)) 
  
  tibble(parameter = response,
         vars = driver_vars,
         fi = var_importance,
         fit = r2)
  
  ## Make the plot
  # as.data.frame(rf$variable.importance) %>%
  #   tibble::rownames_to_column() %>%
  #   rename("vi" = `rf$variable.importance`) %>%
  #   as_tibble() %>%
  #   mutate(n_vi = vi / sum(vi)) %>%
  #   ggplot(aes(n_vi, rowname)) + geom_col()
}

output <- response_var[1:2] %>% 
  map(make_rf_model) %>% 
  bind_rows()

r2 <- output %>% 
  group_by(parameter) %>% 
  summarize(r2 = round(mean(fit), 2)) %>% 
  pull(r2)

fi <- output$fi %>% 
  mutate(par = output$parameter)

## set up a color scheme for water quality
pred_vars <- unique(fi$rowname)
pred_colors <- PNWColors::pnw_palette("Bay", n = length(pred_vars))

fi_do <- ggplot(fi %>% filter(par == "delta_do_hr"), 
                aes(n_vi*100, fct_reorder(rowname, n_vi), fill = rowname)) + 
  geom_col(alpha = 0.5, color = "black", show.legend = F) + 
  labs(x = "Importance (%)", y = "Predictor", 
       title = paste0("DO, R2 = ", r2[[1]]))


fi_co2 <- ggplot(fi %>% filter(par == "pco2_uM_wet"), 
                aes(n_vi*100, fct_reorder(rowname, n_vi), fill = rowname)) + 
  geom_col(alpha = 0.5, color = "black", show.legend = F) + 
  labs(x = "Importance (%)", y = "Predictor", 
       title = paste0("CO2, R2 = ", r2[[2]]))

plot_grid(fi_do, fi_co2, ncol = 2)

ggsave("figures/5_Fig5_feature_importance.png", width = 6, height = 3)




