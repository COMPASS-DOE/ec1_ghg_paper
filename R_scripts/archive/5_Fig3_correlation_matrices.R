## Correlation matrices
##
## 2022-10-07 (Updated 2023-02-09)
## Peter Regier
##
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, 
       cowplot, 
       corrplot, 
       ggcorrplot, 
       ggfortify, 
       ggpubr)

## Character variable vector
chars <- c("kit_id", "transect_location")
common_cols <- c("delta_do_hr", "d_pco2", "d_pch4", "d_pn2o")

## Set theme
theme_set(theme_bw())


# 2. Bring in data -------------------------------------------------------------

df_raw <- read_csv("data/230503_master_data.csv") %>% 
  mutate(transect_cat = as.factor(case_when(transect_location == "Sediment" ~ "Wetter", 
                                            transect_location == "Wetland" ~ "Wetter",
                                            transect_location == "Transition" ~ "Drier",
                                            transect_location == "Upland" ~ "Drier"))) %>% 
  mutate(transect_num = )
  mutate(sal_cat = as.factor(ntile(sal_psu, 2))) 

climate_raw <- read_csv("data/230223_climate_data.csv")

soil_raw <- read_csv("data/230223_soil_data.csv")

df1 <- inner_join(df_raw, climate_raw, by = "kit_id") %>% 
  inner_join(soil_raw, by = "kit_id")


# 3. Make Figure 3 - correlation plots -----------------------------------------

common_drivers <- c("gwc_perc", "tc_perc", "tn_perc", "sal_psu", "ph", "mat_c",
                    "monthly_precip", "bd", "clay", "sand", "silt")

all  <- df_raw %>% 
  select(transect_num, delta_do_hr, contains("M_wet"), common_drivers) %>% 
  drop_na()

sed_wet <- df_raw %>% 
  filter(transect_num < 3) %>% 
  select(transect_num, delta_do_hr, contains("M_wet"), common_drivers) %>% 
  drop_na()

tr_up <- df_raw %>% 
  filter(transect_num > 2) %>% 
  select(transect_num, delta_do_hr, contains("M_wet"), common_drivers, tn_perc) %>% 
  select(-gwc_perc, -loi_perc) %>% 
  drop_na()

nrow(sed_wet) # should be 29
nrow(tr_up) # should be 24
nrow(all) # should be 53


make_corrplot <- function(data){
  p.mat <- ggcorrplot::cor_pmat(cor(data))
  
  ggcorrplot(cor(data), method = "square",
             type = "upper", 
             colors = c("#E46726", "white", "#6D9EC1"), 
             lab = TRUE, 
             lab_size = 3)
}

corrplot_all <- make_corrplot(all) + 
  ggtitle("All samples (n=53)") + 
  theme(legend.position = c(0.8, 0.3))

corrplot_sed_wet <- make_corrplot(sed_wet) + 
  ggtitle("SED and WET samples (n=29)") + 
  theme(legend.position = c(0.8, 0.3))

corrplot_tr_up <- make_corrplot(tr_up) + 
  ggtitle("TR and UP samples (n=24)") + 
  theme(legend.position = c(0.8, 0.3))

plot_grid(corrplot_all, corrplot_sed_wet, corrplot_tr_up, nrow = 1)
ggsave("figures/3_Fig3_corrplots.png", height = 4, width = 12)


# 4. Make Supplemental correlation plots ---------------------------------------

all_wq <- df_raw %>% 
  select(transect_num, delta_do_hr, contains("M_wet"), sal_psu, ph, orp_mv, alk_mgl_caco3, 
         npoc_mgl, tdn_mgl, tss_mg_perl) %>% 
  drop_na() 

make_corrplot(all_wq)
ggsave("figures/s_corrplot_all_gases_and_wa.png", width = 6, height = 6)




## Graveyard

# # 2.5. Sort out if salt plays a role on CH4/N2O production
# make_salt_plot <- function(var){
#   ggplot(df_raw, aes(gwc_perc, {{var}})) + 
#     geom_point(aes(color = transect_location), show.legend = F) + 
#     geom_smooth(span = 0.8) + 
#     facet_wrap(~transect_num, scales = "free")
# }
# 
# plot_co2 <- plot_grid(make_salt_plot(d_pco2) + ggtitle("CO2"), 
#                       make_salt_plot(pco2_uM_dry), 
#                       make_salt_plot(pco2_uM_wet), ncol = 1)
# 
# plot_ch4 <- plot_grid(make_salt_plot(d_pch4) + ggtitle("CH4"), 
#                       make_salt_plot(pch4_nM_dry), 
#                       make_salt_plot(pch4_nM_wet), ncol = 1)
# 
# plot_n2o <- plot_grid(make_salt_plot(d_pn2o) + ggtitle("N2O"), 
#                       make_salt_plot(pn2o_uM_dry), 
#                       make_salt_plot(pn2o_uM_wet), ncol = 1)
# 
# plot_grid(plot_co2, plot_ch4, plot_n2o, nrow = 1)
# ggsave("figures/x_ghgs_v_salt.png", width = 12, height = 12)


# ggplot(df_raw, aes(as.factor(transect_num), delta_do_hr)) + geom_boxplot()
# ggplot(df_raw, aes(as.factor(transect_num), delta_do_hr)) + geom_boxplot()
# 
# x <- df_raw %>% 
#   select(transect_num, delta_do_hr, contains("M_wet"), sal_psu, gwc_perc, loi_perc) %>% 
#   drop_na() %>% 
#   filter(transect_num < 3)

# salt_df <- df_raw %>% select(contains("transect"), sal_psu, contains("M_wet"), contains("delta")) %>% 
#   pivot_longer(cols = c(contains("wet"), contains("delta")), names_to = "gas", values_to = "value")
# 
# sed_wet <- ggplot(salt_df %>% filter(transect_num < 3), aes(sal_psu, value)) + 
#   geom_point(aes(color = transect_location)) +
#   geom_smooth(span = 0.8) + 
#   facet_wrap(~gas, nrow = 1, scales = "free_y")
# 
# tr_up <- ggplot(salt_df %>% filter(transect_num > 2), aes(sal_psu, value)) + 
#   geom_point(aes(color = transect_location)) +
#   geom_smooth(span = 0.8) + 
#   facet_wrap(~gas, nrow = 1, scales = "free_y")
# 
# plot_grid(sed_wet + ggtitle("Usually inundated"), 
#           tr_up + ggtitle("Rarely inundated"), ncol = 1)
# ggsave("figures/x_ghgs_v_salt_simplified.png", width = 10, height = 7)


df2 <- df1 %>% 
  mutate(transect_cat = ifelse(transect_num > 2, "Rare", "Frequent"))

make_scatterplots <- function(var){
  ggplot(df2, aes(silt, {{var}}, color = transect_cat)) + 
    geom_point(alpha = 0.5, show.legend = F) + 
    geom_smooth(method = "lm", show.legend = F) #+ 
    #annotate(geom = "text", x = 20, y = max_y, label = r2)
}

plot_grid(make_scatterplots(delta_do_hr), 
          make_scatterplots(pco2_uM_wet), 
          make_scatterplots(pch4_nM_wet), 
          make_scatterplots(pn2o_uM_wet), 
          nrow = 1)


summary(lm(pco2_uM_wet ~ silt, data = df2 %>% filter(transect_cat == "Frequent")))
summary(lm(pco2_uM_wet ~ silt, data = df2 %>% filter(transect_cat == "Rare")))
summary(lm(pco2_uM_wet ~ silt, data = df2))


###### Retry some stuff

make_tile <- function(data){
  
  x <- data %>% select(delta_do_hr, contains("M_wet"), 
                      transect_num, gwc_perc, loi_perc, 
                      sal_psu, mat_c, monthly_precip, 
                      bd, clay, sand, silt) %>% 
    drop_na()
  
  x_cor <- cor(x) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble() %>% 
    slice(1:4) %>% 
    pivot_longer(cols = -c(rowname))
  
  x_p <- cor_pmat(cor(x)) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble() %>% 
    slice(1:4) %>% 
    pivot_longer(cols = -c(rowname))
  
  y <- inner_join(x_cor %>% rename("cor" = value), 
                  x_p %>% rename("sig" = value), 
                  by = c("rowname", "name")) %>% 
    filter(name != "delta_do_hr") %>% 
    filter(!grepl("_wet", name)) %>%
    mutate(cor = round(cor, 2))
    #filter(abs(sig) < 0.1 & abs(cor) < 1)
  
    ggplot(y, aes(name, rowname, fill = cor)) + 
      geom_tile() + 
      geom_text(aes(label = cor)) +
      scale_fill_gradient2(low = "red",
                           mid = "white",
                           high = "blue") + 
      labs(x = "", y = "")
}

plot_grid(make_tile(df2), 
          make_tile(df2 %>% filter(transect_num <= 2)), 
          make_tile(df2 %>% filter(transect_num > 2)), 
          ncol = 1)
ggsave("230301_correlations.png", width = 9, height = 7)

