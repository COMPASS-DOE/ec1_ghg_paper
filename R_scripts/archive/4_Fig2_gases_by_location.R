## New and improved script to make a new gas figure that includes everything
##
## 2022-04-22
## Peter Regier
##
# ############ #
# ############ #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, #keep things tidy
       cowplot, #plot_grid
       ggpubr, #stat_compare_means()
       ggpattern, #geom_boxplot_pattern()
       rstatix, #levene_test()
       ggallin) #pseudolog10_trans()

## Set ggplot theme
theme_set(theme_bw())

## Set color theme
color_theme <- c("#1B264F", "#CE8147", "#FAF33E", "#3BB273")

## Set comparisons for stats
compare_transect <- list( c("Sediment", "Wetland"), c("Sediment", "Transition"), 
                          c("Sediment", "Upland"), c("Wetland", "Transition"), 
                          c("Wetland", "Upland"), c("Transition", "Upland"))

## Make it easier to reference ID columns
common_cols <- c("kit_id", "transect_location")


# 2. Import and merge datasets -------------------------------------------------

## Read in full dataset
df <- read_csv("data/master_data.csv")

## Subset O2 and GHGs down to useful columns only
o2_raw <- df %>% 
  select(common_cols, delta_do_hr, region) %>% 
  mutate(transect_location = fct_relevel(transect_location, c("Sediment", "Wetland", "Transition", "Upland"))) %>% 
  drop_na()

o2_cb <- o2_raw %>% 
  filter(region != "Great Lakes") %>% 
  group_by(kit_id, transect_location) %>% 
  summarize(delta_do_hr = mean(delta_do_hr))

ghg_raw <- read_csv("data/221116_ghg_cor.csv")

## Identify outliers


ghg_all <- ghg_raw %>% 
  mutate(transect_location = fct_relevel(transect_location, c("Sediment", "Wetland", "Transition", "Upland"))) 
#%>% 
 # filter(pco2_uM < 7500) %>% #filters out 1 sample
 # filter(pch4_nM < 100) %>% #filters out 1 sample
 # filter(pn2o_uM < 10) #filters out 1 sample
  
ghg_wet <- ghg_all %>% filter(type == "wet")

gases <- full_join(o2_cb, ghg_wet, by = c("kit_id", "transect_location")) %>% 
  mutate(pch4_nM = ifelse(pch4_nM == 0, 0.01, pch4_nM))

# 3. Make figures and export ---------------------------------------------------

make_boxplot <- function(var, y_label){
  ggplot(gases, aes(transect_location, {{var}}, fill = transect_location)) + 
    geom_boxplot(show.legend = F, outlier.alpha = 0, width = 0.5) +
    geom_jitter(color = "black", show.legend = F, width = 0.05) + 
   #geom_jitter(data = gases %>% filter(kit_id == "K034"), color = "red", size = 4) +
    scale_fill_manual(values = color_theme) + 
    #scale_y_continuous(trans = pseudolog10_trans, expand = c(0,0)) +
    #scale_y_continuous(trans = "sqrt") + 
    labs(x = "Location", y = y_label) + 
    stat_compare_means(comparisons = compare_transect, label = "p.signif")
}

p_do <- make_boxplot(delta_do_hr, "DO consumption (mg/L/hr)")
p_co2 <- make_boxplot(pco2_uM, "pCO2 (uM)")
p_ch4 <- make_boxplot(pch4_nM, "pCH4 (nM)")
p_n2o <- make_boxplot(pn2o_uM, "pN2O (uM)")

plot_grid(p_do, p_co2, p_ch4, p_n2o, ncol = 2, labels = c("A", "B", "C", "D"))
ggsave("figures/2_Fig2_gases_by_location.png", width = 8, height = 7)


# 4. Calculate variance statistics (Table S1) ----------------------------------

## First, identify which gases pass the fligner test
## Fligner v Levene: https://biostats.w.uib.no/test-for-homogeneity-of-variances-levenes-test/
fligner.test(delta_do_hr ~ transect_location, data = gases) # Fail: p<0.001
fligner.test(pco2_uM ~ transect_location, data = gases) # Fail: p<0.01
fligner.test(pch4_nM ~ transect_location, data = gases) # Fail: p<0.001
fligner.test(pn2o_uM ~ transect_location, data = gases) # Fail: p<0.001



## All fail, so let's do pairwise levene's tests


all_combos <- expand_grid(vars = c("delta_do_hr", "pco2_uM", "pch4_nM", "pn2o_uM"), 
                          site1 = c("Sediment", "Wetland", "Transition", "Upland"), 
                          site2 = c("Sediment", "Wetland", "Transition", "Upland")) %>% 
  filter(site1 != site2)

calculate_pairwise_levene_tests <- function(i){
  sites = all_combos %>% slice(i)
  
  f <- as.formula(paste(sites$vars, "~ transect_location"))
  
  x <- gases %>% filter(transect_location == sites$site1 | 
                          transect_location == sites$site2) %>% 
    ungroup() %>% 
    levene_test(f)
  
  tibble(variable = sites$vars, 
         group1 = sites$site1, 
         group2 = sites$site2, 
         p_value = round(x$p, 3))
}

pairwise_levene_list = list()
for(i in 1:nrow(all_combos)){
  pairwise_levene_list[[i]] <- calculate_pairwise_levene_tests(i)
}

bind_rows(pairwise_levene_list) %>% 
  mutate(locations = paste0(group1, "-", group2)) %>% 
  filter(group2 != "Sediment") %>% 
  filter(locations != "Transition-Wetland") %>% 
  filter(group1 != "Upland") %>% 
  select(variable, locations, p_value) %>% 
  pivot_wider(names_from = "variable", values_from = "p_value")







### GRAVEYARD ###


gases %>% 
  ungroup() %>% 
  mutate(f_transect = as.factor(transect_location)) %>% 
  #group_by(f_transect) %>% 
  #group_by(transect_location) %>% 
  levene_test(delta_do_hr ~ f_transect)

levene_test(pco2_uM ~ transect_location, data = gases %>% ungroup() %>% group_by(transect_location)) %>% 
  filter(group)


summary(aov(pn2o_uM ~ transect_location, data = gases))

calculate_variance <- function(var){
  gases %>% 
    select(transect_location, {{var}}) %>%
    drop_na() %>% 
    group_by(transect_location) %>%
    summarize(variance = (max({{var}}) - min({{var}})) / 
                median({{var}}))
}

calculate_variance_gaussian <- function(var){
  gases %>% 
    select(transect_location, {{var}}) %>%
    drop_na() %>% 
    group_by(transect_location) %>%
    summarize(variance = sd({{var}}) / 
                mean({{var}}))
}

variance <- inner_join(calculate_variance(delta_do_hr) %>% rename("DO" = variance), 
           calculate_variance(pco2_uM) %>% rename("pCO2" = variance)) %>%
  inner_join(calculate_variance(pch4_nM) %>% rename("pCH4" = variance)) %>% 
  inner_join(calculate_variance(pn2o_uM) %>% rename("pN2O" = variance)) %>% 
  pivot_longer(cols = -c(transect_location))

variance_gaussian <- inner_join(calculate_variance_gaussian(delta_do_hr) %>% rename("DO" = variance), 
                                calculate_variance_gaussian(pco2_uM) %>% rename("pCO2" = variance)) %>%
  inner_join(calculate_variance_gaussian(pch4_nM) %>% rename("pCH4" = variance)) %>% 
  inner_join(calculate_variance_gaussian(pn2o_uM) %>% rename("pN2O" = variance)) %>% 
  pivot_longer(cols = -c(transect_location))

bind_rows(variance_gaussian %>% mutate(type = "normal"), 
          variance %>% mutate(type = "non-parameteric")) %>% 
  ggplot(aes(name, value, fill = transect_location)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~type, ncol = 1, scales = "free")

ggplot(variance_gaussian, aes(name, value, fill = transect_location)) + 
  geom_col(position = "dodge")




gases %>% 
  select(transect_location, delta_do_hr) %>%
  drop_na() %>% 
  group_by(transect_location) %>%
  summarize(variance = (max(delta_do_hr) - min(delta_do_hr)) / 
              median(delta_do_hr)) 



