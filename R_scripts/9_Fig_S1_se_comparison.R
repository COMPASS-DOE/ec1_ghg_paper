## This baby scirpt calculates standard errors for each gas rate based on grouping
## either by transect location or sampling site to see how inter and intra-site 
## variability compare (Figure S1)
##
## 2023-07-07
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
       janitor,
       ggrepel,
       ggConvexHull,
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


# 2. Import dataset ------------------------------------------------------------

## Read in full dataset
df <- read_csv("data/230623_master_data.csv") %>% 
  select(all_of(common_cols), contains("uM_hr")) %>% 
  mutate(transect_location = str_to_sentence(transect_location)) %>% 
  mutate(transect_location = fct_relevel(transect_location, c("Sediment", "Wetland", "Transition", "Upland")))

## Function for se
se <- function(var){sd({{var}}) / sqrt(length({{var}}))}

## Function to find outliers (so we can label em)
findoutlier <- function(x) {
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}

by_location <- df %>% 
  rename("group_label" = transect_location) %>% 
  group_by(group_label) %>% 
  summarize(across(where(is.numeric), se)) %>% 
  mutate(type = "Transect Location")

by_kit <- df %>% 
  rename("group_label" = kit_id) %>% 
  group_by(group_label) %>% 
  summarize(across(where(is.numeric), se)) %>% 
  mutate(type = "Sampling Site")

se_df <- bind_rows(by_location, by_kit)



se_boxplot <- function(var, y_label){
  
  x <- se_df %>% 
    group_by(type) %>% 
    select(group_label, {{var}}) %>% 
    mutate(outlier = ifelse(findoutlier({{var}}), {{var}}, NA))
  
  ggplot(x, aes(type, {{var}})) + 
    geom_boxplot() + 
    geom_text_repel(data = x %>% filter(!is.na(outlier)), aes(label=group_label), na.rm=TRUE, hjust=-.5) + 
    labs(x = "", y = y_label)
}

plot_grid(se_boxplot(do_uM_hr, "DO (uM/hr)"), 
          se_boxplot(co2_uM_hr, "CO2 (uM/hr)"), 
          se_boxplot(ch4_uM_hr, "CH4 (uM/hr)"), 
          se_boxplot(n2o_uM_hr, "N2O (uM/hr)"), 
          nrow = 2)
ggsave("figures/S1_intra_vs_inter_site_se.png", width = 6, height = 5)



