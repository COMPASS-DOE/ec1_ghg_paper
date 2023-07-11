## This script calculates source-sink behavior for our data, and ingests
## literature values to compare to our results
##
## 2023-02-06
## Peter Regier
## 
# ########### #
# ########### #

## Make RF models
## 
## 2023-
## Peter Regier
## 
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, 
       cowplot,
       ggallin, 
       ggpubr, 
       ggbreak, #scale_y_break()
       ggforece) #facet_zoom

## Set ggplot theme
theme_set(theme_bw())

## Set color theme
color_theme <- c("#1B264F", "#CE8147", "#FAF33E", "#3BB273")

# 2. Load data -----------------------------------------------------------------

## Load in all data
df_raw <- read_csv("data/master_data.csv") %>% 
  mutate(transect_num = case_when(transect_location == "Sediment" ~ 1, 
                                  transect_location == "Wetland" ~ 2,
                                  transect_location == "Transition" ~ 3,
                                  transect_location == "Upland" ~ 4)) %>% 
  mutate(transect_location = as.factor(transect_location)) %>% 
  filter(pch4_nM_wet < 20)


## Calculate medians
median_ch4 <- median(df_raw$d_pch4, na.rm = T)
median_co2 <- median(df_raw$d_pco2, na.rm = T)
median_n2o <- median(df_raw$d_pn2o, na.rm = T)

## Calculate means
mean_ch4 <- mean(df_raw$d_pch4, na.rm = T)
mean_co2 <- mean(df_raw$d_pco2, na.rm = T)
mean_n2o <- mean(df_raw$d_pn2o, na.rm = T)


make_plot <- function(var, y_label){
  
  ## Set comparisons for stats
  compare_transect <- list( c("Sediment", "Wetland"), c("Sediment", "Transition"), 
                            c("Sediment", "Upland"), c("Wetland", "Transition"), 
                            c("Wetland", "Upland"), c("Transition", "Upland"))
  
  ggplot(df_raw, aes(fct_reorder(transect_location, transect_num), {{var}}, fill = transect_location)) + 
    geom_boxplot(outlier.alpha = 0, show.legend = F) + 
    geom_jitter(width = 0.1, alpha = 0.5, show.legend = F) + 
    geom_hline(yintercept = 0) + 
    labs(x = "", y = y_label) + 
    scale_fill_manual(values = color_theme) #+ 
    #stat_compare_means(comparisons = compare_transect, label = "p.signif")
    #scale_y_break(c(y_start, y_stop))
}

## Make plots
plot_grid(make_plot(d_pco2, "Change in CO2 (uM)"), 
          make_plot(d_pch4, "Change in CH4 (uM)"), 
          make_plot(d_pn2o, "Change in N2O (uM)"), 
          nrow = 1, labels = c("A", "B", "C"))
ggsave("figures/6_Fig6_source_sink.png", width = 8, height = 4)
#ggsave("figures/S_FigSC_source_sink_w_stats.png", width = 8, height = 4)

mean(df_raw$d_pco2, na.rm = T)
mean(df_raw$d_pch4, na.rm = T)
mean(df_raw$d_pn2o, na.rm = T)


df_raw %>% 
  group_by(transect_location) %>% 
  summarize(mean(d_pn2o, na.rm = T))



boxplots <- function(var){
  x <- df_raw %>% 
    mutate(divide_d_pco2 = ifelse(d_pco2 > 0, "source", "sink"))
  
  ggplot(x, aes(divide_d_pco2, {{var}})) + 
    geom_boxplot() + 
    stat_compare_means()
}

boxplots(latitude)




df_raw %>% filter(d_pn2o > 10)






## Calculate mins
min_ch4 <- min(df_raw$d_pch4, na.rm = T)
min_co2 <- min(df_raw$d_pco2, na.rm = T)
min_n2o <- min(df_raw$d_pn2o, na.rm = T)

## Back-of-the-envelop calculations to convert to other units

# Vial diameter is 24, subtracting 4mm for the walls then /2
vial_radius_m <- 10/1000

calculate_uM_m2_d <- function(gas_value){
  
  m2 <- (3.14159 * vial_radius_m^2)
  
  gas_value / m2
}

calculate_t_ha_yr <- function(gas_value){
  m2 <- (3.14159 * vial_radius_m^2)
  ha <- m2 * 0.0001
  
  (gas_value / ha) * 365
}

molar_masses = tibble(gas = c("co2", "ch4", "n2o"), 
                      mass = c(44.01, 16.04, 44.013))

calculate_mg_m2_hr <- function(gas_value, molar_mass){
  
  # (umol / L) * (mmol / umol) * (mg/mmol) * (L/mL) * 40 mL = mg CO2
   mg_gas <- gas_value * (1/1000) * molar_mass * (1/1000) * 40
   
   mg_gas / (3.14159 * vial_radius_m^2) / 24
}

calculate_mg_m2_hr(max_co2, molar_masses %>% filter(gas == "co2") %>% pull(mass))

