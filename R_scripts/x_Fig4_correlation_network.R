## This script constructs correlation networks
##
## 2023-06-26
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
       corrr, 
       igraph, 
       ggraph, 
       psych)


# 2. Load data -----------------------------------------------------------------

## Load in all data
df_raw <- read_csv("data/230623_master_data.csv") %>% 
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


df_cor <- df1 %>% 
  mutate(soil_cn = tc_perc / tn_perc, 
         water_cn = doc_mgl / tdn_mgl) %>% 
  mutate(soil_cn = ifelse(is.infinite(soil_cn), NA, soil_cn)) %>% 
  select(transect_num, 
         contains("_uM_hr"), 
         sal_psu, ph, 
         gwc_perc, 
         doc_mgl, tdn_mgl, 
         tc_perc, tn_perc,
         sand, silt, clay, 
         monthly_precip,
         mat_c, 
         #veg_type,
         soil_cn, water_cn) %>% 
  drop_na()


# 3. Make Correlation Networks -------------------------------------------------

## Pulling p-values using Hmisc package following 
## https://bioinformatics.stackexchange.com/questions/14803/extracting-p-values-using-corrr-package-in-r
make_corr_network <- function(data, title){
  tidy_r <- data %>% 
    correlate(method = "pearson") %>% 
    stretch()
  
  tidy_p <- psych::corr.test(data)$p %>% corrr::as_cordf() %>% 
    stretch() %>% 
    rename("p" = r)
  
  tidy_cors <- inner_join(tidy_r, tidy_p, by = c("x", "y"))
  
  graph_cors <- tidy_cors %>% 
    filter(p < 0.05) %>% 
    graph_from_data_frame(directed = FALSE)
  
  ggraph(graph_cors) +
    geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
    guides(edge_alpha = "none", edge_width = "none") +
    scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
    geom_node_point(color = "black", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_graph() + 
    labs(title = title)
}


cormap_all <- make_corr_network(df_cor, "All")
cormap_by_location <-plot_grid(make_corr_network(df_cor %>% filter(transect_num > 2), "Wetter"), 
                               make_corr_network(df_cor %>% filter(transect_num > 3), "Drier"), 
                               nrow = 1)

plot_grid(cormap_all, cormap_by_location, ncol = 1)
ggsave("figures/4_Fig4_corr_networks_wetter_v_drier.png", width = 14, height = 12)


