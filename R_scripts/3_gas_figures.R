## This script creates the first several figures showing oxygen consumption and
## greenhouse gas production across the coastal TAI
## 
## 2022-04-22
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


# 3. Create oxygen consumption figure ------------------------------------------

ggplot(o2_cb, aes(transect_location, delta_do_hr, fill = transect_location)) + 
  geom_boxplot(width = 0.5, outlier.alpha = 0, alpha = 0.8, show.legend = F, color = "black") +
  geom_jitter(alpha = 0.6, width = 0.2, show.legend = F) + 
  scale_fill_manual(values = color_theme) + 
  stat_compare_means(aes(label = ..p.signif..), comparisons = compare_transect) + 
  scale_y_sqrt() + 
  labs(x = "", y = "Oxygen consumed (mg/L/hr)", fill = "")
ggsave("figures/B_oxygen_consumption.pdf", width = 4, height = 4)
  

# 4. Create GHG wet v dry figure -----------------------------------------------

ghg_raw <- df %>% 
  filter(region != "Great Lakes") %>% 
  select(kit_id, transect_location, type, pco2_c, pch4_c, pn2o_c) %>% 
  group_by(kit_id, transect_location, type) %>% 
  summarize(across(everything(), mean, na.rm = T)) %>% 
  mutate(transect_location = fct_relevel(transect_location, c("Sediment", "Wetland", "Transition", "Upland"))) 

## IMPORTANT: first, I'm going to remove 4 outliers identified by plotting the data
ghg_no_outliers <- ghg_raw %>% 
  filter(pco2_c < 150000) %>% #removes K015 Upland-dry (CO2 = 208,251 ppm)
  filter(pch4_c < 15) %>% #removes K050 Wetland-wet (CH4 = 20.7 ppm)
  filter(pn2o_c < 400) %>% #removes K018 Upland-wet and K034 Upland-wet 
mutate(transect_location = fct_relevel(transect_location, c("Sediment", "Wetland", "Transition", "Upland")))

## First function: wet v dry
plot_wet_dry <- function(var, y_lab){
  ghg_no_outliers %>% 
    select(transect_location, {{var}}, type) %>% 
    drop_na() %>% 
    ggplot(aes(transect_location, {{var}}, fill = transect_location, pattern = type)) + 
    geom_boxplot_pattern() + 
    scale_fill_manual(guide = "none", values = color_theme) + 
    scale_pattern_manual(values = c("dry" = "none", "wet" = "stripe")) +
    labs(x = "", y = y_lab, pattern = "Treatment") + 
    theme(legend.position = c(0.2, 0.8), 
          legend.background = element_blank())
}

plot_grid(plot_wet_dry(pco2_c, "CO2"), 
          plot_wet_dry(pch4_c, "CH4"), 
          plot_wet_dry(pn2o_c, "N2O"), 
          nrow = 1)
ggsave("figures/C_ghg_wet_dry.pdf", width = 8, height = 5)


# 5. Create GHG source/sink figure ---------------------------------------------

d_ghg <- ghg_no_outliers %>%
  select(kit_id, transect_location, type, pco2_c, pch4_c, pn2o_c) %>% 
  distinct() %>% 
  drop_na() %>% 
  pivot_wider(names_from = type, 
              values_from = c(pco2_c, pch4_c, pn2o_c)) %>% 
  mutate(d_pco2 = pco2_c_wet - pco2_c_dry, 
         d_pch4 = pch4_c_wet - pch4_c_dry, 
         d_pn2o = pn2o_c_wet - pn2o_c_dry)

plot_delta <- function(var, y_lab){
  d_ghg %>% 
    select(transect_location, {{var}}) %>% 
    drop_na() %>% 
    ggplot(aes(transect_location, {{var}}, fill = transect_location)) + 
    geom_boxplot() + 
    scale_fill_manual(guide = "none", values = color_theme) + 
    stat_compare_means(aes(label = ..p.signif..), comparisons = compare_transect) + 
    labs(x = "", y = y_lab)
}

plot_grid(plot_delta(d_pco2, "CO2"), 
          plot_delta(d_pch4, "CH4"), 
          plot_delta(d_pn2o, "N2O"), 
          nrow = 1)
ggsave("figures/X_ghg_delta.pdf", width = 8, height = 5)


plot_source_sink <- function(var, y_lab){
  
  x <- d_ghg %>% ungroup() %>%
    dplyr::select({{var}}) %>%
    drop_na() %>%
    summarize(y_min = min(.)*1.1,
              y_max = max(.)*1.1)
  
  ggplot(d_ghg, aes(x = 0, y = {{var}})) + 
    annotate(geom = "rect", xmin = -0.5, xmax = 0.5,
             ymin = 0, ymax = x$y_max,
             fill = "palegreen", alpha = 0.2) +
    annotate(geom = "rect", xmin = -0.5, xmax = 0.5,
             ymin = x$y_min, ymax = 0,
             fill = "red", alpha = 0.2) +
    geom_boxplot(width = 0.5, outlier.color = NA) + 
    geom_jitter(alpha = 0.5, width = 0.05) +
    geom_hline(yintercept = 0) + 
    labs(x = "", y = y_lab) + 
    theme(axis.text.x=element_blank(), #remove x axis labels
          axis.ticks.x=element_blank()) + #remove x axis ticks 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0))
}

plot_grid(plot_source_sink(d_pco2, "Change in CO2 (ppm)"), 
          plot_source_sink(d_pch4, "Change in CH4 (ppm)"), 
          plot_source_sink(d_pn2o, "Change in N2O (ppm)") + 
            scale_y_continuous(trans = pseudolog10_trans, expand = c(0,0)), 
          nrow = 1)
ggsave("figures/D_ghg_source_sink.pdf", width = 6, height = 4)
