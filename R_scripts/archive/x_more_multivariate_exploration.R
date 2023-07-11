## Mutual information
##
## 2022-11-03
## Peter Regier
##
# ########### #


# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, bestNormalize, cowplot, 
       corrplot, ggcorrplot, ggfortify, ggpubr,
       janitor,
       infotheo, ranger, Boruta)

## Character variable vector
chars <- c("kit_id", "transect_location")
common_cols <- c("delta_do_hr", "d_pco2", "d_pch4", "d_pn2o")

## Set theme
theme_set(theme_bw())


# 2. Bring in data -------------------------------------------------------------

df_raw <- read_csv("data/master_data.csv") %>% 
  mutate(transect_num = case_when(transect_location == "Sediment" ~ 1, 
                                  transect_location == "Wetland" ~ 2,
                                  transect_location == "Transition" ~ 3,
                                  transect_location == "Upland" ~ 4)) %>% 
  fill(macro_algae_num, .direction = "up") %>% 
  filter(region == "Chesapeake Bay") %>% 
  filter(d_pn2o < 500) %>% 
  filter(d_pco2 > -100000) %>% 
  filter(d_pch4 < 10)



# 2. Bring in data -------------------------------------------------------------

df_raw <- read_csv("data/master_data.csv") %>% 
  mutate(transect_num = case_when(transect_location == "Sediment" ~ 1, 
                                  transect_location == "Wetland" ~ 2,
                                  transect_location == "Transition" ~ 3,
                                  transect_location == "Upland" ~ 4)) %>% 
  fill(macro_algae_num, .direction = "up") %>% 
  filter(region == "Chesapeake Bay") %>% 
  filter(d_pn2o < 500) %>% 
  filter(d_pco2 > -100000) %>% 
  filter(d_pch4 < 10)

# 3. Transform data ------------------------------------------------------------

my_list <- df_raw %>% select_if(is.numeric) %>% colnames()

yj_normalize <- function(var){
  x <- df_raw %>% dplyr::pull({{var}})
  bc <- bestNormalize::yeojohnson(x) #Boxcox can't handle negative values
  p <- predict(bc)
  return(p)
}

bc_list <- list()
for(i in 1:length(my_list)){
  bc_list[[i]] <- df_raw %>% 
    mutate(x = yj_normalize(my_list[[i]])) %>% 
    select(x)
}

yj_df <- do.call(cbind.data.frame, bc_list)
colnames(yj_df) <- my_list

df_n <- df_raw %>% select(chars) %>% 
  add_column(yj_df)

df <- df_raw


df_subset <- df %>% 
  select(delta_do_hr, d_pco2, d_pch4, d_pn2o, 
         transect_num, npoc_mgl, sal_psu, gwc_perc, loi_perc, ph, nitrate_ppm) %>% 
  drop_na() %>% 
  discretize()

x <- data.frame(mutinformation(df_subset)) %>% 
  rownames_to_column() %>% 
  as_tibble()

plot_mi <- function(var, title){
  x %>% 
    select(rowname, {{var}}) %>% 
    mutate(mi = {{var}} / max({{var}})) %>% 
    filter(mi != 1) %>% 
    ggplot(aes(fct_reorder(rowname, mi), mi)) + 
    geom_col(color = "blue", fill = "blue", alpha = 0.2) + 
    labs(x = "", y = "Mutual information", title = title) + 
    scale_y_continuous(limits = c(0, 0.25)) +
    coord_flip()
}

plot_grid(plot_mi(delta_do_hr, "DO consumption"), 
          plot_mi(d_pco2, "pCO2"),
          plot_mi(d_pch4, "pCH4"),
          plot_mi(d_pn2o, "pN2O"), 
          nrow = 1)

plot_mi(delta_do_hr, "DO consumption")
ggsave("figures/mutual_info.png", width = 3, height = 3)

x %>% 
  select(rowname, delta_do_hr) %>% 
  mutate(mi = delta_do_hr / max(delta_do_hr)) %>% 
  ggplot(aes(fct_reorder(rowname, mi), mi)) + 
  geom_col() + 
  labs(x = "", y = "Mutual information") + 
  coord_flip()



y <- df %>% 
  select(delta_do_hr, d_pco2, d_pch4, d_pn2o, 
         transect_num, npoc_mgl, sal_psu, gwc_perc, loi_perc, ph, nitrate_ppm) %>% 
  drop_na()

make_rf_model <- function(df, var){
  as.data.frame(Boruta({{var}} ~ ., data = df, doTrace = 2)$finalDecision) %>% 
    tibble::rownames_to_column() %>%
    rename("decision" = 2) %>% 
    as_tibble() %>% 
    filter(decision == "Confirmed") %>% 
    pull(rowname)
}

make_rf_model(y, delta_do_hr)

z <- as.data.frame(Boruta(delta_do_hr ~ ., data = y, doTrace = 2)$finalDecision) %>% 
  tibble::rownames_to_column() %>%
  rename("decision" = 2) %>% 
  as_tibble() %>% 
  filter(decision == "Confirmed") %>% 
  pull(rowname)


nrow(y)

rf <- ranger(delta_do_hr ~ ., data = y, 
            importance = "impurity")
  
as.data.frame(rf$variable.importance) %>% 
  tibble::rownames_to_column() %>%
  rename("vi" = rf$variable.importance) %>% 
  as_tibble()


