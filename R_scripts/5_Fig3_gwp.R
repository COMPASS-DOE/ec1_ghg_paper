## This script calculates greenhouse gas warming potentials and compares them
## 1) by location across the transect, and 2) as a map?
##
## 2023-06-26
## Peter Regier
##
# ########### #
# ########### #


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

## Read in ghg dataset
df <- read_csv("data/230623_master_data.csv") %>% 
  mutate(transect_location = fct_relevel(str_to_sentence(transect_location), 
                                         c("Sediment", "Wetland", "Transition", "Upland")))

## Read in seawater for reference
sw <- read_csv("data/230623_ghg_saltwater.csv") %>% 
  dplyr::select(contains("_uM_hr")) %>% 
  summarize(across(everything(), mean, na.rm = T))


# X. Calculate greenhouse gas equivalents --------------------------------------

## 100-year global warming potentials relative to CO2 are based on 
## https://www.ipcc.ch/site/assets/uploads/2018/02/WG1AR5_Chapter08_FINAL.pdf, 
## Appendix 8, using the GWP 100 value
## 1 CH4 unit = 28 CO2 units, 1 N2O unit = 265 CO2 units
ch4_gwp_conversion = 28
n2o_gwp_conversion = 265

df_gwp <- df %>% 
  mutate(co2_gwp = co2_uM_hr, 
         ch4_gwp = ch4_uM_hr * ch4_gwp_conversion, 
         n2o_gwp = n2o_uM_hr * n2o_gwp_conversion) %>% 
  dplyr::select(kit_id, transect_location, contains("_gwp")) %>% 
  pivot_longer(cols = contains("_gwp")) %>% 
  mutate(name = str_to_upper(str_sub(name, 1, 3)))

df_gwp_c <- df_gwp %>% 
  mutate(value_c = case_when(name == "CO2" ~ value - sw$co2_uM_hr, 
                             name == "CH4" ~ value - (sw$ch4_uM_hr * ch4_gwp_conversion), 
                             name == "N2O" ~ value - (sw$n2o_uM_hr * n2o_gwp_conversion)))

gwp_c_means <- df_gwp_c %>% 
  group_by(transect_location, name) %>%
  summarize(mean = mean(value_c))

## Set up a helper function to paste mean and se together
n_sigfigs = 3
se <- function(var){round(sd({{var}}) / sqrt(length({{var}})), n_sigfigs)}

gwc_c_se <- df_gwp_c %>% 
  group_by(transect_location, name) %>%
  summarize(se = se(value_c))

gwp_c <- inner_join(gwp_c_means, gwc_c_se, by = c("transect_location", "name"))

gwp_c %>% 
  ggplot(aes(transect_location, fill = name)) + 
  geom_col(aes(y = mean), alpha = 0.5, color = "black", width = 0.7, position = "dodge") + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width= 0.2, position = position_dodge(0.7)) + 
  labs(x = "", y = "100-yr Global warming potential (CO2 eq / hr)", fill = "")
ggsave("figures/3_Fig3_GWP_percent.png", width = 5, height = 4)

gwp_c_means %>% 
  summarize(value = sum(value))

gwp_c %>% 
  ungroup() %>% 
  group_by(transect_location) %>% 
  mutate(mean_p  = mean / sum(mean), 
         se_p = se / sum(mean))



