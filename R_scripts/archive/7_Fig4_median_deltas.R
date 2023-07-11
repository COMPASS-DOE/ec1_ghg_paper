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
df <- read_csv("data/230503_master_data.csv") %>% 
  mutate(transect_location = fct_relevel(str_to_sentence(transect_location), 
                                         c("Sediment", "Wetland", "Transition", "Upland")))

# df %>% 
#   ungroup() %>% 
#   mutate(transect_cat = as.factor(case_when(transect_location == "Sediment" ~ "Wetter", 
#                                             transect_location == "Wetland" ~ "Wetter",
#                                             transect_location == "Transition" ~ "Drier",
#                                             transect_location == "Upland" ~ "Drier"))) %>% 
#   select(transect_cat, delta_do_hr, contains("d_p")) %>% 
#   group_by(transect_cat) %>% 
#   summarize(across(everything(), median, na.rm = T))

medians <- df %>% 
  ungroup() %>% 
  select(transect_location, do_uM_hr, d_pco2_uM_hr, d_pch4_uM_hr, d_pn2o_uM_hr) %>% 
  group_by(transect_location) %>% 
  summarize(across(everything(), median, na.rm = T))

make_median_plots <- function(var, y_lab){
  ggplot(medians, aes(transect_location, {{var}}, fill = transect_location)) + 
    geom_col(color = "black", show.legend = F, width = 0.5) +
    geom_hline(yintercept = 0) +
    scale_fill_manual(values = color_theme) + 
    labs(x = "", y = y_lab)
}

# plot_grid(make_median_plots(delta_do_hr, "∂-DO (mg/L/hr)"), 
#           make_median_plots(d_pco2, "∂-CO2 (µM)"), 
#           ncol = 2, align = "hv", labels = c("A", "B"))
#ggsave("figures/230508_median_deltas_DO_CO2.png", width = 6, height = 2.5)

medians_plot <- plot_grid(make_median_plots(d_pco2_uM_hr, "∆-CO2 (µM/hr)"), 
          make_median_plots(d_pch4_uM_hr, "∆-CH4 (µM/hr)"), 
          make_median_plots(d_pn2o_uM_hr, "∆-N2O (µM/hr)"), 
          nrow = 1, align = "hv", labels = c("A", "B", "C"))
ggsave("figures/4_Fig4_median_deltas.png", width = 6, height = 3)








