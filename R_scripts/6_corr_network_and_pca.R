## This script is used to create the final two figures for the EC1-GHG paper, which
## will include a correlation matrix, and a PCA. The PCA will use variables that 
## are selected using a random forests (presented as a supplemental figure)
##
## 2023-08-04
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
       ggConvexHull,
       corrr, 
       igraph, 
       ggraph, 
       psych)

## Set color theme
color_theme <- c("#1B264F", "#CE8147", "#FAF33E", "#3BB273")


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

df <- inner_join(df_raw, climate_raw, by = "kit_id") %>% 
  inner_join(soil_raw, by = "kit_id") 


# 3. Correlation networks ------------------------------------------------------

## Create a dataset for correlations
df_trim <- df %>% 
  mutate(soil_cn = tc_perc / tn_perc, 
         water_cn = doc_mgl / tdn_mgl) %>% 
  mutate(soil_cn = ifelse(is.infinite(soil_cn), NA, soil_cn)) %>% 
  dplyr::select(transect_location, 
                transect_num, 
         contains("_uM_hr"), 
         sal_psu, ph, 
         gwc_perc, 
         doc_mgl, tdn_mgl, 
         tc_perc, tn_perc,
         sand, silt, clay, 
         latitude, longitude, elevation_m,
         monthly_precip, mat_c, 
         soil_cn, water_cn) %>% 
  drop_na()

df_cor <- df_trim %>% 
  dplyr::select(-transect_location) %>% 
  rename("DO (uM/hr)" = do_uM_hr, 
         "CO2 (uM/hr)" = co2_uM_hr, 
         "CH4 (uM/hr)" = ch4_uM_hr, 
         "N2O (uM/hr)" = n2o_uM_hr, 
         "Salinity (PSU)" = sal_psu, 
         "pH" = ph,
         "GWC (%)" = gwc_perc,
         "DOC (mg/L)" = doc_mgl, 
         "TDN (mg/L)" = tdn_mgl, 
         "TC (%)" = tc_perc, 
         "TN (%)" = tn_perc, 
         "% Sand" = sand, 
         "% Silt" = silt, 
         "% Clay" = clay,
         "Precip." = monthly_precip, 
         "Air temp." = mat_c, 
         "Soil C:N" = soil_cn, 
         "Water C:N" = water_cn, 
         "Latitude" = latitude,
         "Longitude" = longitude,
         "Elev. (m)" = elevation_m)

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


cor_sediment <- make_corr_network(df_cor %>% filter(transect_num == 1), "Sediment")
cor_wetland <- make_corr_network(df_cor %>% filter(transect_num == 2), "Wetland")
cor_transition <- make_corr_network(df_cor %>% filter(transect_num == 3), "Transition")
cor_upland <- make_corr_network(df_cor %>% filter(transect_num == 4), "Upland")

plot_grid(cor_sediment, cor_wetland, cor_transition, cor_upland, 
          nrow = 2, labels = c("A", "B", "C", "D"))
ggsave("figures/4_Fig4_correlation_networks.png", width = 12, height = 8)

# 4. Random Forest model -------------------------------------------------------

plot_fi <- function(data){
  
  rf_model <- ranger(transect_num ~ ., data = data, importance = "impurity")
  
  print(rf_model)
  
  var_names <- rf_model$variable.importance
  col_names <- c("predictor", "raw_fi")
  
  ## Convert feature importance to a tibble with variables as a column
  fi0 <- as.data.frame(var_names) %>% 
    tibble::rownames_to_column() %>% 
    as_tibble()
  
  ## Rename columns
  colnames(fi0) = col_names
  
  fi0 %>% 
    arrange(raw_fi) %>% 
    ggplot(aes(raw_fi, fct_reorder(predictor, raw_fi))) + 
    geom_col() + 
    labs(x = "Feature Importance", y = "Variable (Feature)")
  
}

fi_plot_all <- plot_fi(df_trim %>% dplyr::select(-transect_location)) + 
  geom_vline(aes(xintercept = 0.88*2), linetype = "dashed") + 
  ggtitle("RF model with all potential variables")

vars_to_keep <- fi0 %>% 
  filter(raw_fi > max(fi0$raw_fi) / 5)

fi_plot_vars <- plot_fi(df_trim %>% 
                          dplyr::select(transect_num, 
                                                  all_of(vars_to_keep %>% 
                                                           pull(predictor)))) + 
  ggtitle("RF model with all selected variables") 
  

plot_grid(fi_plot_all, fi_plot_vars, nrow = 1) 
ggsave("figures/SB_rf_feature_importance.png", width = 10, height = 5)

# 5. PCA -----------------------------------------------------------------------

pca_data <- df_trim %>% 
  dplyr::select(contains("transect"), all_of(vars_to_keep %>% pull(predictor)))

pca_data_scaled <- scale(pca_data %>% dplyr::select(-transect_location)) %>% 
  as_tibble() %>% 
  rename("DO (uM/hr)" = do_uM_hr, 
         "CO2 (uM/hr)" = co2_uM_hr, 
         "CH4 (uM/hr)" = ch4_uM_hr, 
         "N2O (uM/hr)" = n2o_uM_hr, 
         #"Salinity (PSU)" = sal_psu, 
         #"pH" = ph,
         "GWC (%)" = gwc_perc,
         #"DOC (mg/L)" = doc_mgl, 
         #"TDN (mg/L)" = tdn_mgl, 
         "TC (%)" = tc_perc, 
         "TN (%)" = tn_perc, 
         #"% Sand" = sand, 
         #"% Silt" = silt, 
         #"% Clay" = clay,
         #"Precip." = monthly_precip, 
         #"Air temp." = mat_c, 
         #"Soil C:N" = soil_cn, 
         #"Water C:N" = water_cn, 
         #"Latitude" = latitude,
         #"Longitude" = longitude,
         "Elev. (m)" = elevation_m) %>% 
  as.data.frame()

pca0 <- prcomp(pca_data_scaled %>% dplyr::select(-contains("transect")), 
               scale. = TRUE, retx = T)

loadings <- as_tibble(pca0$x) %>% 
  dplyr::select(PC1, PC2) %>% 
  mutate(PC1 = abs(PC1), 
         PC2 = abs(PC2)) 

pca_w_transect <- pca_data %>% mutate(transect_location = pca_data$transect_location)
  

autoplot(pca0, data = pca_w_transect, 
         colour = 'transect_location',
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, size = 3) +
  geom_convexhull(aes(group = transect_location, fill = transect_location, color = transect_location), alpha = 0.2) + 
  scale_color_manual(values = color_theme) + 
  scale_fill_manual(values = color_theme) + 
  labs(color = "Transect \n location", fill = "Transect \n location")
ggsave("figures/5_Fig5_pca.png", width = 6, height = 5)


# 6. LDA -----------------------------------------------------------------------

lda_model <- lda(transect_num ~ ., pca_data_scaled)

lda_loadings <- lda_model$scaling %>% 
  as.data.frame() %>%
  rownames_to_column() %>% 
  as_tibble()

lda_data <- as.data.frame(predict(lda_model, pca_data_scaled)) %>% 
  as_tibble() %>% 
  dplyr::select(x.LD1, x.LD2) %>% 
  rename("LD1" = x.LD1, 
         "LD2" = x.LD2) %>% 
  mutate(transect_location = pca_data$transect_location)

ggplot() + 
  geom_point(data = lda_data, aes(LD1, LD2, color = transect_location), size = 3, alpha = 0.8) + 
  geom_convexhull(data = lda_data, aes(LD1, LD2, group = transect_location, fill = transect_location), alpha = 0.2) + 
  geom_segment(data = lda_loadings, aes(x = LD1, y = LD2, xend = 0, yend = 0)) + 
  geom_label(data = lda_loadings, aes(x = LD1, y = LD2, label = rowname), alpha = 0.2)




