# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, 
       cowplot, 
       corrplot, 
       ggcorrplot, 
       ggfortify, 
       ggpubr)

theme_set(theme_bw())


# 2. Bring in data -------------------------------------------------------------

df_raw <- read_csv("data/master_data.csv") %>% 
  mutate(transect_cat = as.factor(case_when(transect_location == "Sediment" ~ "Wetter", 
                                  transect_location == "Wetland" ~ "Wetter",
                                  transect_location == "Transition" ~ "Drier",
                                  transect_location == "Upland" ~ "Drier"))) %>% 
  mutate(sal_cat = as.factor(ntile(sal_psu, 2))) %>% 
  filter(pch4_nM_wet < 20) %>% 
  filter(pn2o_uM_wet < 10)


aov_two_way <- function(var){
  x <- df_raw %>% 
    select(transect_cat, sal_cat, {{var}})
  
  ## Check that you have two factors
  str(x)
  
  ## Check if it's balanced
  table(x$transect_cat, x$sal_cat)
  
  ggplot(x, aes(transect_cat, {{var}}, color = sal_cat)) + 
    geom_boxplot(show.legend = F) + 
    labs(x = "")
  
  # res.aov2 <- aov(pn2o_uM_wet ~ transect_cat + sal_cat, data = x)
  # summary(res.aov2)
}

#delta_do_hr
#pco2_uM_wet
#pch4_nM_wet
#pn2o_uM_wet

plot_grid(aov_two_way(delta_do_hr), 
          aov_two_way(pco2_uM_wet), 
          aov_two_way(pch4_nM_wet), 
          aov_two_way(pn2o_uM_wet), 
          ncol = 2)

df_raw %>% 
  filter(transect_cat == "Wetter") %>% 
  select(delta_do_hr, contains("M_wet")) %>% 
  drop_na() %>% 
  cor() %>% 
  corrplot(method = "number")

df_raw %>% 
  filter(transect_cat == "Drier") %>% 
  select(delta_do_hr, contains("M_wet")) %>% 
  drop_na() %>% 
  cor() %>% 
  corrplot(method = "number")


df_raw %>% 
  filter(sal_cat == 1) %>% 
  select(delta_do_hr, contains("M_wet")) %>% 
  drop_na() %>% 
  cor() %>% 
  corrplot(method = "number")

df_raw %>% 
  #filter(sal_cat == 2) %>% 
  select(delta_do_hr, contains("M_wet")) %>% 
  drop_na() %>% 
  cor() %>% 
  corrplot(method = "number")


