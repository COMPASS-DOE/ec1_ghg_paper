## This script creates median plots
##
## 2023-05-19
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
df <- read_csv("data/230503_master_data.csv") %>% 
  select(all_of(common_cols), delta_do_hr, contains("d_")) %>% 
  mutate(transect_location = str_to_sentence(transect_location)) %>% 
  mutate(transect_location = fct_relevel(transect_location, c("Sediment", "Wetland", "Transition", "Upland"))) %>% 
  drop_na()


# 3. Make figures and export ---------------------------------------------------

make_barchart <- function(var, y_label){
  
  x <- df %>% 
    select(transect_location, {{var}}) %>% 
    group_by(transect_location) %>% 
    summarize(median_value = mean({{var}}, na.rm = T))
  
  ggplot(x, aes(transect_location, median_value, fill = transect_location)) + 
    geom_col(show.legend = F, width = 0.5, color = "black") +
    scale_fill_manual(values = color_theme) +
    labs(x = "", y = y_label) + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1))
}

bar_do <- make_barchart(delta_do_hr, "DO consumption (mg/L/hr)")
bar_co2 <- make_barchart(d_pco2, "∂-CO2 (uM)")
bar_ch4 <- make_barchart(d_pch4, "∂-CH4 (nM)")
bar_n2o <- make_barchart(d_pn2o, "∂-N2O (uM)")

plot_grid(bar_do, bar_co2, bar_ch4, bar_n2o, nrow = 1, labels = c("A", "B", "C", "D"), align = "hv")
ggsave("figures/3_Fig3_gas_medians_by_location.png", width = 10, height = 3)


## make an alternative figure, inspired by James, where we plot densities

make_density_plots <- function(var, y_label){
  
  ggplot(df, aes({{var}}, fill = transect_location)) + 
    geom_density(show.legend = F, alpha = 0.8, color = "black") +
    scale_fill_manual(values = color_theme) + 
    pseudolog10_trans()
}





