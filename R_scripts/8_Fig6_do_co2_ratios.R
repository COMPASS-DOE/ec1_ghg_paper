## This script creates a figure to show CO2:DO ratios to understand potential
## biotic v abiotic v other drivers
##

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

## Make it easier to reference ID columns
common_cols <- c("kit_id", "transect_location")


# 2. Import data ---------------------------------------------------------------

## Read in ghg dataset
df <- read_csv("data/230623_master_data.csv") %>% 
  mutate(transect_location = fct_relevel(str_to_sentence(transect_location), 
                                         c("Sediment", "Wetland", "Transition", "Upland")))

## Read in seawater for reference
sw <- read_csv("data/230623_ghg_saltwater.csv") %>% 
  select(contains("_uM_hr")) %>% 
  summarize(across(everything(), mean, na.rm = T))

df_cor <- df %>% 
  mutate(co2_uM_hr_cor = co2_uM_hr - sw$co2_uM_hr) %>% 
  mutate(co2_do = co2_uM_hr_cor / do_uM_hr)


# 3. Make figure ---------------------------------------------------------------

ggplot(df_cor, aes(co2_do, transect_location, fill = transect_location)) + 
  geom_boxplot(alpha = 0.5, show.legend = F) + 
  geom_vline(xintercept = 1, linetype = "dashed") + 
  scale_fill_manual(values = color_theme) +
  scale_x_log10() + 
  labs(x = "CO2:DO (unitless)", y = "")
ggsave("figures/6_Fig6_co2_do_ratios.png", width = 6, height = 3.5)





