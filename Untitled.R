## Make PCA plots
## 
## 2023-05-31
## Peter Regier
## 
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)

p_load(tidyverse, 
       tidymodels, 
       cowplot,
       ranger, 
       ggpubr,
       vegan,
       Boruta,
       PNWColors,
       ggfortify,
       ggConvexHull)

theme_set(theme_bw())


# 2. Load data -----------------------------------------------------------------

## Load in all data
df_raw <- read_csv("data/230503_master_data.csv") %>% 
  mutate(transect_location = str_to_sentence(transect_location)) %>% 
  mutate(transect_location = fct_relevel(as.factor(transect_location), 
                                         c("Sediment", "Wetland", "Transition", "Upland"))) %>% 
  mutate(transect_cat = as.factor(case_when(transect_location == "Sediment" ~ "Wetter", 
                                            transect_location == "Wetland" ~ "Wetter",
                                            transect_location == "Transition" ~ "Drier",
                                            transect_location == "Upland" ~ "Drier")), 
         transect_num = case_when(transect_location == "Sediment" ~ 1, 
                                  transect_location == "Wetland" ~ 2,
                                  transect_location == "Transition" ~ 3,
                                  transect_location == "Upland" ~ 4))

climate_raw <- read_csv("data/230223_climate_data.csv")

soil_raw <- read_csv("data/230223_soil_data.csv")

unique(lulc_raw$vegetation_lifeform)

lulc_raw <- read_csv("data/230315_lulc.csv") %>% 
  filter(transect_location != "Water") %>% 
  mutate(veg_lifeform = case_when(vegetation_lifeform == "Shrub" ~ "Tree", 
                                  vegetation_lifeform == "Agriculture" ~ "Developed", 
                                  TRUE ~ vegetation_lifeform)) %>% 
  mutate(veg_type = case_when(veg_lifeform == "Tree" ~ 4, 
                              veg_lifeform == "Herb" ~ 3, 
                              veg_lifeform == "Developed" ~ 2,
                              veg_lifeform == "Water" ~ 1)) %>%
  select(kit_id, transect_location, veg_lifeform, veg_type)


df1 <- inner_join(df_raw, climate_raw, by = "kit_id") %>% 
  inner_join(soil_raw, by = "kit_id") %>% 
  inner_join(lulc_raw, by = c("kit_id", "transect_location"))


df_pca <- df1 %>% 
  select(transect_location, transect_num, delta_do_hr, contains("M_wet"),  
         sal_psu, doc_mgl, tdn_mgl, gwc_perc, tc_perc, tn_perc, 
         veg_type, sand, clay, monthly_precip, mat_c) %>% 
  drop_na()

pca1 <- prcomp(df_pca %>% select(-transect_location), scale. = TRUE, retx = TRUE) 

autoplot(pca1, data = df_pca, colour = 'transect_location',
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) +
  geom_convexhull(aes(group = transect_location, fill = transect_location), alpha = 0.2) +
  scale_color_viridis_d() + 
  scale_fill_viridis_d() + 
  labs(colour = "", fill = "") +
  theme_bw()
ggsave("figures/230601_pca.png", width = 7, height = 6)


ggplot(df1, aes(sal_psu, ph)) + geom_point()

