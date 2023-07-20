## Make PCA plots
## 
## 2023-05-31 (updated 6/26/23)
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
       ggallin,
       ggfortify,
       ggConvexHull)

theme_set(theme_bw())


# 2. Load data -----------------------------------------------------------------

## Load in all data
df_raw <- read_csv("data/230623_master_data.csv") %>% 
  mutate(transect_location = str_to_sentence(transect_location)) %>% 
  mutate(transect_location = fct_relevel(as.factor(transect_location), 
                                         c("Sediment", "Wetland", "Transition", "Upland")), 
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
  select(contains("uM_hr"), #transect_location, 
         #sal_psu, sand
         #gwc_perc, doc_mgl, tc_perc
         #) %>% 
    transect_location, 
    lat, lon, 
    #ph, tss_mgl, 
    silt,
         sal_psu, doc_mgl, 
    tdn_mgl, 
    gwc_perc, tc_perc, tn_perc, 
         veg_type, 
    sand, clay, monthly_precip, mat_c) %>% 
  drop_na()

df_pca <- df1 %>% 
  select(contains("uM_hr"), 
         transect_location, 
         sal_psu, 
         doc_mgl, #tdn_mgl,
         tc_perc, #tn_perc,
         sand, silt, clay, 
         #veg_type,
         gwc_perc,
         #lat,
         mat_c) %>% 
  #select(-contains("n2o"))  %>% 
  mutate(transect_location = fct_relevel(transect_location, c("Sediment", "Wetland", "Transition", "Upland"))) %>% 
  drop_na()

pca1 <- prcomp(df_pca %>% select(-transect_location), scale. = TRUE, retx = TRUE)

autoplot(pca1, data = df_pca, colour = 'transect_location',
           loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, size = 3) +
  geom_convexhull(aes(group = transect_location, fill = transect_location), alpha = 0.2) +
  scale_color_viridis_d() + 
  scale_fill_viridis_d() + 
  labs(colour = "", fill = "") +
  theme_bw()
ggsave("figures/5_Fig5_pca.png", width = 7, height = 6)

as.data.frame(pca1$rotation[,1:2])


## Calculate loading stats
loadings <- as_tibble(pca1$x) %>% 
  select(PC1, PC2) %>% 
  bind_cols(transect_location = df_pca$transect_location) %>% 
  mutate(transect_location = fct_relevel(transect_location, c("Sediment", "Wetland", "Transition", "Upland")))

pc1_plot <- ggplot(loadings, aes(transect_location, PC1)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 0) +
  stat_compare_means() + 
  labs(x = "")

pc2_plot <- ggplot(loadings, aes(transect_location, PC2)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 0) +
  stat_compare_means() + 
  labs(x = "")

plot_grid(pc1_plot, pc2_plot, nrow = 1)
ggsave("figures/SA_pca_score_boxplots.png", width = 8, height = 4)


