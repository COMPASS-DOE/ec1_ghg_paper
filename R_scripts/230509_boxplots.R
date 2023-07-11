



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
       PNWColors)


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

## Create categories
df_cat <- df_raw %>% 
  mutate(sal_cat = ifelse(sal_psu > median(df_raw$sal_psu, na.rm = T), "high", "low")) %>% 
  mutate(doc_cat = ifelse(doc_mgl > median(df_raw$doc_mgl, na.rm = T), "high", "low")) %>% 
  mutate(tdn_cat = ifelse(tdn_mgl > median(df_raw$tdn_mgl, na.rm = T), "high", "low")) %>% 
  mutate(tc_cat = ifelse(tc_perc > median(df_raw$tc_perc, na.rm = T), "high", "low")) %>% 
  mutate(gwc_cat = ifelse(gwc_perc > median(df_raw$gwc_perc, na.rm = T), "high", "low"))


# 3. Make table ----------------------------------------------------------------

by_salinity <- df_cat %>% 
  select(sal_cat, delta_do_hr, contains("M_wet")) %>% 
  group_by(sal_cat) 


# 4. Make plots ----------------------------------------------------------------



## Cross-effect boxplot

cross_effect_boxplot <- function(var1, var2){
  
  x <- df_cat %>% 
    filter(!is.na({{var2}}))
  
  do_plot <- ggplot(x, aes({{var1}}, delta_do_hr, fill = {{var2}})) + 
    geom_boxplot(show.legend = F) + 
    stat_compare_means(label = "p.signif", vjust = 1) + 
    labs(y = "", title = "DO") 
  
  co2_plot <- ggplot(x, aes({{var1}}, pco2_uM_wet, fill = {{var2}})) + 
    geom_boxplot(show.legend = F) + 
    stat_compare_means(label = "p.signif", vjust = 1) + 
    labs(y = "", title = "CO2") 
  
  ch4_plot <- ggplot(x, aes({{var1}}, pch4_nM_wet, fill = {{var2}})) + 
    geom_boxplot(show.legend = F) + 
    stat_compare_means(label = "p.signif", vjust = 1) + 
    labs(y = "", title = "CH4")
  
  n2o_plot <- ggplot(x, aes({{var1}}, pn2o_uM_wet, fill = {{var2}})) + 
    geom_boxplot() + 
    stat_compare_means(label = "p.signif", vjust = 1) + 
    labs(y = "", title = "N2O") 
  
  
  plot_grid(do_plot, co2_plot, ch4_plot, n2o_plot, 
            nrow = 1)
}

plot_grid(cross_effect_boxplot(transect_cat, sal_cat), 
          cross_effect_boxplot(transect_cat, doc_cat), 
          cross_effect_boxplot(transect_cat, gwc_cat), 
          #cross_effect_boxplot(sal_cat, gwc_cat), 
          #cross_effect_boxplot(sal_cat, doc_cat), 
          #cross_effect_boxplot(gwc_cat, doc_cat), 
          ncol = 1)
ggsave("figures/230512_cross_effect_boxplots.png", 
       width = 9, height = 5)


## Single effect boxplots
single_effect_boxplot <- function(var1){
  
  x <- df_raw %>% 
    filter(!is.na({{var1}}))
  
  do_plot <- ggplot(x, aes({{var1}}, delta_do_hr)) + 
    geom_boxplot(show.legend = F) + 
    stat_compare_means(label = "p.signif", vjust = 1) + 
    labs(y = "", title = "DO") 
  
  co2_plot <- ggplot(x, aes({{var1}}, pco2_uM_wet)) + 
    geom_boxplot(show.legend = F) + 
    stat_compare_means(label = "p.signif", vjust = 1) + 
    labs(y = "", title = "CO2") 
  
  ch4_plot <- ggplot(x, aes({{var1}}, pch4_nM_wet)) + 
    geom_boxplot(show.legend = F) + 
    stat_compare_means(label = "p.signif", vjust = 1) + 
    labs(y = "", title = "CH4")
  
  n2o_plot <- ggplot(x, aes({{var1}}, pn2o_uM_wet)) + 
    geom_boxplot() + 
    stat_compare_means(label = "p.signif", vjust = 1) + 
    labs(y = "", title = "N2O") 
  
  
  plot_grid(do_plot, co2_plot, ch4_plot, n2o_plot, 
            nrow = 1)
}

plot_grid(single_effect_boxplot(transect_cat), 
          single_effect_boxplot(sal_cat), 
          single_effect_boxplot(doc_cat), 
          ncol = 1)
ggsave("figures/230517_single_effect_boxplots.png", 
       width = 9, height = 7)


# 5. Other stuff ---------------------------------------------------------------

ggplot(df_raw, aes(transect_cat, pn2o_uM_wet, fill = sal_cat)) + 
  geom_boxplot() + 
  stat_compare_means(label = "p.signif")
  

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


ggplot(df_raw, aes(transect_location, pn2o_uM_wet)) + 
  geom_boxplot() + 
  scale_y_sqrt()

#### Try a correlation network map
p_load(corrr, igraph, ggraph, cowplot, psych)

df_cor <- df1 %>% 
  mutate(soil_cn = tc_perc / tn_perc, 
         water_cn = doc_mgl / tdn_mgl) %>% 
  mutate(soil_cn = ifelse(is.infinite(soil_cn), NA, soil_cn)) %>% 
  select(transect_num, 
         delta_do_hr, contains("_wet"), 
         sal_psu, gwc_perc, 
         doc_mgl, tdn_mgl, 
         tc_perc, tn_perc,
         sand, clay, 
         monthly_precip,
         mat_c, 
         veg_type,
         soil_cn, water_cn) %>% 
  drop_na()



  

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
    #filter(abs(r) > 0.3) %>%
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
ggsave("figures/230511_corr_networks_wetter_v_drier.png", width = 14, height = 12)


ggplot(df1, aes(delta_do_hr, pch4_nM_wet, color = transect_cat)) + 
  geom_point() + geom_smooth(method = "lm")

summary(lm(delta_do_hr ~ pch4_nM_wet, data = df1))


ggplot(df1, aes(pch4_nM_wet, delta_do_hr, color = transect_cat)) + geom_point()


## test case
data(varespec)
data(varechem)

cca(varespec)
plot(cca(varespec))

df2 <- df1 %>% 
  select(delta_do_hr, contains("d_"), contains("_wet"), 
         transect_num, transect_cat, 
         tn_perc, tc_perc, bd, silt, sand, clay, 
         sal_psu, ph, doc_mgl, tdn_mgl, gwc_perc, tss_mgl, 
         mat_c, monthly_precip) %>% 
  mutate(c_n = tc_perc / tn_perc) %>% 
  mutate(sal_cat = ifelse(sal_psu > median(df1$sal_psu, na.rm = T), "low", "high")) %>% 
  mutate(sand_cat = ifelse(sand > median(df1$sand, na.rm = T), "low", "high")) %>% 
  mutate(silt_cat = ifelse(silt > median(df1$silt, na.rm = T), "low", "high")) %>% 
  mutate(clay_cat = ifelse(clay > median(df1$clay, na.rm = T), "low", "high")) %>% 
  mutate(gwc_cat = ifelse(gwc_perc > median(df1$gwc_perc, na.rm = T), "low", "high")) %>% 
  mutate(doc_cat = ifelse(doc_mgl > median(df1$doc_mgl, na.rm = T), "low", "high")) %>% 
  mutate(tc_cat = ifelse(tc_perc > median(df1$tc_perc, na.rm = T), "low", "high")) %>% 
  mutate(tn_cat = ifelse(tn_perc > median(df1$tn_perc, na.rm = T), "low", "high")) %>% 
  mutate(cn_cat = ifelse(c_n > median(df1$c_n, na.rm = T), "low", "high")) %>% 
  mutate(tss_cat = ifelse(tss_mgl > median(df1$tss_mgl, na.rm = T), "low", "high")) %>%
  mutate(rain_cat = ifelse(monthly_precip > median(df1$monthly_precip, na.rm = T), "low", "high")) %>% 
  mutate(temp_cat = ifelse(mat_c > median(df1$mat_c, na.rm = T), "low", "high")) %>%
  drop_na() 


make_density_plot <- function(var){
  
  p_do <- ggplot(df2, aes(x = {{var}}, y = delta_do_hr)) + 
    geom_boxplot(alpha = 0.5, show.legend = F) +
    scale_y_log10() + 
    stat_compare_means()
  p_co2 <- ggplot(df2, aes(x = {{var}}, y = pco2_uM_wet)) + 
    geom_boxplot(alpha = 0.5, show.legend = F) +
    scale_y_log10() + 
    stat_compare_means()
  p_ch4 <- ggplot(df2, aes(x = {{var}}, y = pch4_nM_wet)) + 
    geom_boxplot(alpha = 0.5, show.legend = F) +
    scale_y_log10() + 
    stat_compare_means()
  p_n2o <- ggplot(df2, aes(x = {{var}}, y = pn2o_uM_wet)) + 
    geom_boxplot(alpha = 0.5, show.legend = F) +
    scale_y_log10() + 
    stat_compare_means()

  plot_grid(p_do, p_co2, p_ch4, p_n2o, nrow = 1)
}

make_density_plot(transect_cat) # DO, co2, n2o < 0.05
make_density_plot(sal_cat) #co2 < 0.05
make_density_plot(ph_cat) #co2 < 0.05
make_density_plot(sand_cat) # co2 < 0.1
make_density_plot(silt_cat) # none
make_density_plot(clay_cat) # ch4 < 0.1
make_density_plot(gwc_cat) # DO, n2o < 0.05
make_density_plot(doc_cat) # co2 < 0.5, ch4 < 0.1
make_density_plot(tc_cat) # n2o < 0.1
make_density_plot(tn_cat) # none
make_density_plot(tss_cat) # co2
make_density_plot(rain_cat) # none
make_density_plot(temp_cat) # none

plot_grid(make_density_plot(sal_cat), 
          make_density_plot(doc_cat), 
          make_density_plot(gwc_cat), 
          ncol = 1)




ggplot(df2, aes(delta_do_hr, fill = sal_cat)) + 
  geom_density(alpha = 0.5) + 
  scale_x_log10()


## Function to reformat Boruta output
pick_vars <- function(data){
  as.data.frame(data) %>% 
    tibble::rownames_to_column() %>%
    rename("decision" = 2) %>% 
    as_tibble() %>% 
    filter(decision == "Confirmed") %>% 
    pull(rowname)
}

boruta_vars <- pick_vars(Boruta(delta_do_hr ~ ., data = df2, doTrace = 2)$finalDecision)

ranger(delta_do_hr ~ boruta_vars, data = df2)

x <- df2 %>% 
  select(delta_do_hr, contains("_wet")) 

y <- df2 %>% 
  select(transect_num, tn_perc, tc_perc, silt, sand, clay, 
         sal_psu, ph, doc_mgl, tdn_mgl, gwc_perc)

plot(cca(x, y, scale = T))






# tidy_cors <- df_cor %>% 
#   correlate(method = "spearman") %>% 
#   stretch()
#   
# graph_cors <- tidy_cors %>% 
#   filter(abs(r) > 0.3) %>% 
#   graph_from_data_frame(directed = FALSE)
# 
# ggraph(graph_cors) +
#   geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
#   guides(edge_alpha = "none", edge_width = "none") +
#   scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
#   geom_node_point(color = "black", size = 5) +
#   geom_node_text(aes(label = name), repel = TRUE) +
#   theme_graph()







