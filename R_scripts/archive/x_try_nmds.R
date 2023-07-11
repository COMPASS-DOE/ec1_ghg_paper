## Make an NMDS plot for the EC1 GHG data
 
# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(tidyverse, 
       ggfortify,
       ggConvexHull,
       vegan)

# 2. Load data -----------------------------------------------------------------

df_raw <- read_csv("data/master_data.csv")

df <- df_raw %>% 
  select(transect_location, delta_do_hr, contains("d_p"),
         sal_psu,
         npoc_mgl, tdn_mgl, gwc_perc, loi_perc,
         nitrate_ppm) %>% 
  drop_na()

df_soil <- df_raw %>% 
  select(transect_location, delta_do_hr, contains("d_p"), 
         gwc_perc, loi_perc, bulk_density_g_cm3, 
         tn_perc, tc_perc) %>% 
  drop_na()


# 3. Transform data ------------------------------------------------------------

my_list <- df %>% select_if(is.numeric) %>% colnames()

yj_normalize <- function(var){
  x <- df_raw %>% dplyr::pull({{var}})
  bc <- bestNormalize::yeojohnson(x) #Boxcox can't handle negative values
  p <- predict(bc)
  return(p)
}

bc_list <- list()
for(i in 1:length(my_list)){
  bc_list[[i]] <- df_raw %>% 
    mutate(x = yj_normalize(my_list[[i]])) %>% 
    select(x)
}

yj_df <- do.call(cbind.data.frame, bc_list)
colnames(yj_df) <- my_list

df_n <- df_raw %>% select(transect_location) %>% 
  add_column(yj_df)


# 3.5 Transform soils data -----------------------------------------------------

my_list_soil <- df_soil %>% select_if(is.numeric) %>% colnames()

yj_normalize <- function(var){
  x <- df_raw %>% dplyr::pull({{var}})
  bc <- bestNormalize::yeojohnson(x) #Boxcox can't handle negative values
  p <- predict(bc)
  return(p)
}

bc_list_soil <- list()
for(i in 1:length(my_list_soil)){
  bc_list_soil[[i]] <- df_raw %>% 
    mutate(x = yj_normalize(my_list_soil[[i]])) %>% 
    select(x)
}

yj_df_soil <- do.call(cbind.data.frame, bc_list_soil)
colnames(yj_df_soil) <- my_list_soil

df_soil_n <- df_raw %>% select(transect_location) %>% 
  add_column(yj_df_soil)


# 4. Try an NMDS (lackluster results) ------------------------------------------

m_com <- as.matrix(df_n %>% select(-transect_location) %>% drop_na()) 

set.seed(123)
nmds = metaMDS(m_com, distance = "bray", autotransform = FALSE)
plot(nmds)


data.scores = as_tibble(scores(nmds))

df_nmds <- bind_cols(df, data.scores)

ggplot(df_nmds, aes(NMDS1, NMDS2, color = transect_location)) + 
  geom_point(aes(size = sal_psu)) + 
  theme_bw()

ordiplot(nmds, type = "n", main = "ellipses")


# 5. Try a PCA 

df_pca <- df %>% 
  mutate(transect_c = case_when(transect_location == "Sediment" ~ 1, 
                                transect_location == "Wetland" ~ 2, 
                                transect_location == "Transition" ~ 3, 
                                transect_location == "Upland" ~ 4)) %>% 
  filter(d_pch4 < 80) %>% 
  drop_na()

pca <- prcomp(df_pca %>% select(-transect_location), scale. = TRUE)
autoplot(pca, data = df_pca, colour = 'transect_location', 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) + 
  geom_convexhull(aes(group = transect_location, fill = transect_location), alpha = 0.2) +
  scale_color_viridis_d() + 
  scale_fill_viridis_d() + 
  theme_bw()
ggsave("figures/221116_PCA_all_samples.png", width = 5, height = 4)

# 6 Soil PCA

df_pca_soil <- df_soil %>% 
  mutate(transect_c = case_when(transect_location == "Sediment" ~ 1, 
                                transect_location == "Wetland" ~ 2, 
                                transect_location == "Transition" ~ 3, 
                                transect_location == "Upland" ~ 4)) %>% 
  drop_na()

pca_soil <- prcomp(df_pca_soil %>% select(-transect_location), scale. = TRUE)
autoplot(pca_soil, data = df_pca_soil, colour = 'transect_location', 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) + 
  geom_convexhull(aes(group = transect_location, fill = transect_location), alpha = 0.2) +
  scale_color_viridis_d() + 
  scale_fill_viridis_d() + 
  theme_bw()
ggsave("figures/221116_PCA_soil_samples.png", width = 5, height = 4)







