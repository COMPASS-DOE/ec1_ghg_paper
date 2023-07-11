## Make PCA plots
## 
## 2022-11-16
## Peter Regier
## 
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, 
       ggfortify,
       ggConvexHull)


# 2. Load data -----------------------------------------------------------------

## Load in all data
df_raw <- read_csv("data/master_data.csv") %>% 
  mutate(transect_num = case_when(transect_location == "Sediment" ~ 1, 
                                  transect_location == "Wetland" ~ 2,
                                  transect_location == "Transition" ~ 3,
                                  transect_location == "Upland" ~ 4)) %>% 
  fill(macro_algae_num, .direction = "up") 


common_drivers <- c("sal_psu", "ph")

all <- df_raw %>% 
  dplyr::select(transect_num, delta_do_hr, contains("M_wet"), common_drivers, transect_location) %>% 
  drop_na() %>% 
  mutate(transect_cat = ifelse(transect_num > 2, "Drier", "Wetter")) %>% 
  filter(pch4_nM_wet < 20) 

nrow(all)

pca1 <- prcomp(all %>% dplyr::select(-transect_location, -transect_cat), scale. = TRUE)

p1 <- autoplot(pca1, data = all, colour = 'transect_location', 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) + 
  geom_convexhull(aes(group = transect_location, fill = transect_location), alpha = 0.2) +
  scale_color_viridis_d() + 
  scale_fill_viridis_d() + 
  theme_bw()



no_sed <- df_raw %>% 
  dplyr::select(transect_num, delta_do_hr, contains("M_wet"), 
         #tn_perc, tc_perc, 
         gwc_perc, loi_perc, transect_location, bulk_density_g_cm3) %>% 
  drop_na() %>% 
  mutate(transect_cat = ifelse(transect_num > 2, "Drier", "Wetter")) %>% 
  filter(pch4_nM_wet < 20)

pca2 <- prcomp(no_sed %>% dplyr::select(-transect_cat, -transect_location), scale. = TRUE)

p2 <- autoplot(pca2, data = no_sed, colour = 'transect_location', 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) + 
  geom_convexhull(aes(group = transect_location, fill = transect_location), alpha = 0.2) +
  scale_color_viridis_d() + 
  scale_fill_viridis_d() + 
  theme_bw()

p2

plot_grid(p1, p2, nrow = 1)
ggsave("figures/4_Fig4_PCAs.png", width = 12, height = 5)


#####################


