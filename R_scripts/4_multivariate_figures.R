## This script creates the second round of figures showing how gases (GHGs and O2)
## relate to soil physics, soil chemistry, water quality, and site characteristics
## 
## 2022-04-27 (updated 6/3/22)
## Peter Regier
# ############ #
# ############ #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, #keep things tidy
       cowplot, #plot_grid
       ggpubr, #stat_compare_means()
       ggpattern, #geom_boxplot_pattern()
       psych, ggbiplot, ggfortify, vegan, rpart, rpart.plot, #NMDS
       corrplot, ggbiplot,
       tidymodels, ranger, hydroGOF,
       ggallin) #pseudolog10_trans()

## Set ggplot theme
theme_set(theme_bw())

## Set color theme
color_theme <- c("#1B264F", "#CE8147", "#FAF33E", "#3BB273")

## Set up convenience strings of different column groupings
gas_cols <- c("delta_do_hr", "d_pco2", "d_pch4", "d_pn2o")
wq_cols <- c("sulfate_ppm", "nitrate_ppm", "a254", "BIX", "FI", "SUVA254", 
             "sal_psu", "ph", "orp_mv", "alk_mgl_caco3", "npoc_mgl", "tdn_mgl", "tss_mg_perl")
soil_cols <- c("bulk_density_g_cm3", "gwc_perc", "loi_perc", "soil_ph", 
               "soil_cond", "tn_perc", "tc_perc")
env_cols <- c("latitude", "longitude", "macro", "system", "rotten_eggs", "site")
common_cols <- c("kit_id", "transect_location")

# 2. Import data ---------------------------------------------------------------

df_raw <- read_csv("data/master_data.csv") %>% 
  filter(region != "Great Lakes") 

## Prep GHG data
d_ghg <- df_raw %>% 
  select(kit_id, transect_location, type, pco2_c, pch4_c, pn2o_c) %>% 
  group_by(kit_id, transect_location, type) %>% 
  filter(pco2_c < 150000) %>% #removes K015 Upland-dry (CO2 = 208,251 ppm)
  filter(pch4_c < 15) %>% #removes K050 Wetland-wet (CH4 = 20.7 ppm)
  filter(pn2o_c < 400) %>% #removes K018 Upland-wet and K034 Upland-wet 
  dplyr::summarize(across(everything(), mean, na.rm = T)) %>% 
  drop_na() %>% 
  pivot_wider(names_from = type, 
              values_from = c(pco2_c, pch4_c, pn2o_c)) %>% 
  mutate(d_pco2 = pco2_c_wet - pco2_c_dry, 
         d_pch4 = pch4_c_wet - pch4_c_dry, 
         d_pn2o = pn2o_c_wet - pn2o_c_dry) %>% 
  select(common_cols, contains("d_p"))
  

df_numerics <- left_join(d_ghg, df_raw %>% filter(type == "dry"), by = common_cols) %>% 
  mutate(macro = case_when(water_macrophytes_algae == "None" ~ 1, 
                           water_macrophytes_algae == "Macrophytes" ~ 2,
                           water_macrophytes_algae == "Macrophytes, Algae" ~ 3)) %>% 
  mutate(system = case_when(water_systemtype == "Estuary" ~ 1, 
                            water_systemtype == "Tidal Stream" ~ 2,
                            water_systemtype == "Estuary, Tidal River" ~ 2)) %>% #only K017
  mutate(rotten_eggs = case_when(sediment_rotten_egg_smell == "No" ~ 1, 
                            sediment_rotten_egg_smell == "Yes" ~ 2)) %>% 
  mutate(site = case_when(transect_location == "Sediment" ~ 1, 
                          transect_location == "Wetland" ~ 2,
                          transect_location == "Transition" ~ 3,
                          transect_location == "Upland" ~ 4)) %>% 
  mutate(pco2_sink = ifelse(d_pco2 > 0, "source", "sink"), 
         pch4_sink = ifelse(d_pch4 > 0, "source", "sink"), 
         pn2o_sink = ifelse(d_pn2o > 0, "source", "sink")) %>% 
  ungroup() # important so common_cols are dropped prior to cor()
  

df_numerics %>% 
  select(gas_cols, env_cols) %>% 
  drop_na() %>% 
  cor() %>% 
  corrplot()

df_numerics %>% 
  filter(transect_location == "Sediment") %>% 
  select(gas_cols, sal_psu, ph, nitrate_ppm) %>% 
  drop_na() %>% 
  cor() %>% 
  corrplot()

df_numerics %>% 
  #filter(transect_location == "Sediment") %>% 
  ggplot(aes(ph, d_pn2o, color = transect_location)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  facet_wrap(~transect_location) + 
  #scale_color_manual(values = color_theme) + 
  labs(title = "Water pH vs change in N2O", 
       x = "Water pH", y = " N2O (Wet - dry, ppm)")

summary(lm(d_pn2o ~ ph, data = df_numerics %>% 
             filter(transect_location == "Sediment")))

summary(lm(d_pn2o ~ ph, data = df_numerics %>% 
             filter(transect_location == "Upland")))

prep_data <- function(vars){
  df_numerics %>%
    select(gas_cols, vars) %>% 
    drop_na()
}

## Just gas_cols: 110
## rotten egg: 98
## site: 110 (include)
## gwc_perc: 108 (likely keep)
## soil cond: 53
## tc_perc: 80
## SUVA254: 69
## alk_mgl_caco3: 110
## sulfate_ppm: 110
## nitrate_ppm: 110

df_numerics %>% 
  select(gas_cols, site, sulfate_ppm, nitrate_ppm, alk_mgl_caco3) %>% 
  drop_na() %>% 
  nrow()

custom_cols <- c("site", "sulfate_ppm")


x_pca <- prcomp(prep_data(soil_cols), center = T, scale. = T)
ggbiplot::ggbiplot(x_pca)
nrow(prep_data(soil_cols))



o2_rf <- function(data){
  
  rf_data <- data %>%
    drop_na()
  
  set.seed(42)
  split_data <- initial_split(rf_data, prop = 0.7)
  
  model_recipe <- training(split_data) %>% 
    recipe(delta_do_hr ~ .) %>% 
    step_corr(all_predictors()) %>%
    step_normalize(all_predictors(), -all_outcomes()) %>% 
    prep()
  
  testing_data <- model_recipe %>%
    bake(testing(split_data)) 
  
  n_test = nrow(testing_data)
  
  training_data <- juice(model_recipe)
  
  ## Set seed for before making model
  set.seed(42)
  
  ## Make the actual model
  rf_model <- rand_forest(trees = 500, mode = "regression") %>%
    set_engine("ranger") %>%
    fit(delta_do_hr ~ ., data = training_data)
  
  ## Calculate predicted variables
  model_fit <- testing_data %>% 
    mutate(predict(rf_model, testing_data)) 
  
  #hydroGOF::NSE(model_fit$.pred, model_fit$delta_do_hr)
  rf_model
}

o2_rf(df_numerics %>% select(gas_cols))






df0 <- df %>% 
  filter(type == "dry") %>% #remove extra rows that are now spurious
  select(kit_id, site, sal_psu, gwc_perc, loi_perc, ph, npoc_mgl, alk_mgl_caco3, delta_do_hr) %>% 
  inner_join(delta_ghg, by = c("kit_id", "site")) %>% 
  select(-site) %>% 
  group_by(kit_id) %>% 
  dplyr::summarize(across(everything(), mean, na.rm = T)) %>% 
  drop_na() %>% 
  select(-kit_id) 

ggplot(df0, aes(sal_psu, d_pch4)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Salinity (PSU)", y = "dCH4 (ppm, wet - dry)")
ggsave("/Users/regi350/OneDrive - PNNL/Documents/presentations/2022-05 JASM presentation/sal_v_ch4.pdf", width = 4, height = 3)


dplyr::summarize(sal = mean(sal_psu), 
            delta_do_hr = mean(delta_do_hr), 
            pco2_c = mean(pco2_c), 
            pch4_c = mean(pch4_c), 
            pn2o_c = mean(pn2o_c))




df_pca <- df %>% 
  mutate(type_dbl = ifelse(type == "wet", 1, 0),
         site = as.factor(site)) %>% 
  mutate(site_cd2 = as.numeric(fct_recode(as.factor(site), 
                                          "1" = "Sediment", 
                                          "2" = "Wetland", 
                                          "3" = "Transition", 
                                          "4" = "Upland"))) %>% 
  filter(type == "wet") %>% 
  select(site, delta_do_hr, pco2_c, pch4_c, pn2o_c, 
         npoc_mgl, sal_psu, ph, alk_mgl_caco3, 
         gwc_perc, loi_perc, bulk_density_g_cm3) %>% 
  filter(pco2_c < 200000 & pn2o_c < 200) %>% 
  drop_na()

df.pca <- prcomp(df_pca %>% select(-site), center = TRUE, scale = TRUE)



summary(df.pca)

ggbiplot::ggbiplot(prcomp(df0, center = TRUE, scale = TRUE))





## Since not getting much from PCAs, try both NMDS and CART

## For CART, the goal is to understand which variables are most important for
## O2 and each of the gases. Need two things: R2, and the actual tree

x <- df %>% 
  filter(type == "wet") %>% 
  select(delta_do_hr, site, 
                   gwc_perc, loi_perc, npoc_mgl,
                   sal_psu, ph, orp_mv, alk_mgl_caco3) %>% 
  drop_na()

y = rpart(delta_do_hr ~ ., data = x)

prp(y)
  
z <- x %>% mutate(pred = predict(y, newdata = x))

summary(lm(pred ~ delta_do_hr, data = z))[9]



ggplot(z, aes(pch4_c, pred)) + 
  geom_point(aes(color = site)) + 
  geom_smooth(method = "lm", se = F)




df %>% 
  mutate(site_cd2 = as.numeric(fct_recode(as.factor(site), 
                               "1" = "Sediment", 
                               "2" = "Wetland", 
                               "3" = "Transition", 
                               "4" = "Upland"))) %>% 
  select(site_cd2)


x <- df %>% 
  mutate(site_cd = as.numeric(fct_recode(as.factor(site), 
                                          "1" = "Sediment", 
                                          "2" = "Wetland", 
                                          "3" = "Transition", 
                                          "4" = "Upland"))) %>%
  select(delta_do_hr, site, site_cd, pco2_c, pch4_c, pn2o_c, gwc_perc) %>% 
  filter(across(where(is.numeric), ~ .x > 0)) %>% 
  drop_na() 

## Make a separate step here so I can use x to play with ggplot
mds.x <- x %>% select(-site) %>% 
  as.matrix()

mds <- metaMDS(mds.x, distance = "bray", autotransform = T)

plot(mds)

tb_mds <- bind_cols(as_tibble(mds$points), 
          as_tibble(mds.x)) %>% 
  mutate(site = x$site)

site.fit <- as.data.frame(scores(envfit(mds, x, permutations = 999), display = "vectors"))
site.fit <- cbind(site.fit, vars = rownames(site.fit))

sediment <- tb_mds[tb_mds$site == "Sediment", ][chull(tb_mds[tb_mds$site == 
                                                                   "Sediment", c("MDS1", "MDS2")]), ]  
wetland <- tb_mds[tb_mds$site == "Wetland", ][chull(tb_mds[tb_mds$site == 
                                                                   "Wetland", c("MDS1", "MDS2")]), ] 
transition <- tb_mds[tb_mds$site == "Transition", ][chull(tb_mds[tb_mds$site == 
                                                             "Transition", c("MDS1", "MDS2")]), ] 
upland <- tb_mds[tb_mds$site == "Upland", ][chull(tb_mds[tb_mds$site == 
                                                             "Upland", c("MDS1", "MDS2")]), ] 


hull_data <- bind_rows(sediment, wetland, transition, upland)


ggplot() + 
  geom_point(data = tb_mds, aes(MDS1, MDS2, color = site)) + 
  geom_polygon(data = hull_data, aes(MDS1, MDS2, group = site, fill = site, alpha = 0.2))
  
  
  geom_segment(data = site.fit, 
               aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), 
               color = "grey10", lwd=0.3) 

+ 
  ggrepel::geom_text_repel(data = site.fit, aes(x=NMDS1, y=NMDS2, label = vars), 
                           cex = 3, direction = "both", segment.size = 0.25)








make_cart <- function(var){
  x <- df %>% select(eval(parse(text = var)), site, type, gwc_perc, npoc_mgl, sal_psu) %>% 
    mutate(dep = var) %>% 
    drop_na()
  
  head(x)
  #y = rpart(dep ~ ., data = x)
  
  #prp(y)
}

make_cart("delta_do_hr")





z <- x %>% mutate(pred = predict(y, newdata = x))
  
ggplot(z, aes(pco2_c, pred)) + 
  geom_point(aes(color = site)) + 
  geom_smooth(se = F)

summary(lm(pred ~ pco2_c, data = z))[9]

make_cart <- function(var){
  
  x <- df %>% select({{var}}, gwc_perc, npoc_mgl, sal_psu) %>% 
    drop_na()
  
  rpart(var~., data = x)
  
  #prp(cart)
}


x <- df %>% select({{var}}, site, gwc_perc, npoc_mgl, sal_psu) %>% 
  drop_na()


y = rpart(delta_do_hr ~ ., data = x)

rsq.rpart(y)


make_cart(pco2_c)


cartdf <- 

cart <- rpart(delta_do_hr ~ ., data = cartdf)

cart_o2 <- prp(cart)

rf <- ranger(delta_do_hr ~ ., data = rfdf, importance = 'impurity')

run_rf <- function(data){
}








as.data.frame(df %>% filter(is.na(bulk_density_g_cm3)) %>% 
                filter(site != "Sediment") %>% 
                select(kit_id, delta_do_hr, site, bulk_density_g_cm3))

