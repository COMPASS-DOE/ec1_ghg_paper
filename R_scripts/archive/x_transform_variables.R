## This script explores if transformations are needed to normalize variable
## distributions
##
## 2022-08-05
## Peter Regier
##
# ########### #


# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, tidymodels, bestNormalize, cowplot, 
       corrplot, randomForest, ranger, hydroGOF, party, 
       ggfortify) ##prcomp

## Character variable vector
chars <- c("kit_id", "type", "transect_location")

## Set theme
theme_set(theme_bw())


# 2. Bring in data -------------------------------------------------------------

df_raw <- read_csv("data/master_data.csv")

df_selected <- df_raw %>% 
  dplyr::select(kit_id, type, transect_location, 
         delta_do_hr, contains("d_p"), 
         sal_psu, ph, orp_mv, npoc_mgl, tdn_mgl, nitrate_ppm, sulfate_ppm, 
         tss_mg_perl, gwc_perc, loi_perc) %>% 
  drop_na()
  

# 3. Transform data ------------------------------------------------------------

my_list <- df_selected %>% select_if(is.numeric) %>% colnames()

yj_normalize <- function(var){
  x <- df_selected %>% dplyr::pull({{var}})
  bc <- bestNormalize::yeojohnson(x) #Boxcox can't handle negative values
  p <- predict(bc)
  return(p)
}

bc_list <- list()
for(i in 1:length(my_list)){
  bc_list[[i]] <- df_selected %>% 
    mutate(x = yj_normalize(my_list[[i]])) %>% 
    select(x)
}

yj_df <- do.call(cbind.data.frame, bc_list)
colnames(yj_df) <- my_list

df_n <- df_selected %>% select(chars) %>% 
  add_column(yj_df)


# 4. Multivariate exploration --------------------------------------------------


df_clean <- df_n %>%
  mutate(transect_fact = case_when(transect_location == "Sediment" ~ 1, 
                                   transect_location == "Wetland" ~ 2, 
                                   transect_location == "Transect" ~ 3, 
                                   transect_location == "Upland" ~ 4))


## PCA
df_subset <- df_clean %>% 
  select(delta_do_hr, d_pco2, d_pch4, d_pn2o, gwc_perc, loi_perc, 
         sal_psu, npoc_mgl, nitrate_ppm) %>% 
  drop_na() 

df_pca <- prcomp(df_subset, scale. = TRUE)

autoplot(df_pca, 
         loadings = TRUE, 
         loadings.label = TRUE, 
         loadings.label.size = 3)


## CORRPLOT
df_subset  %>% 
  #select(-chars) %>% 
  cor() %>% 
  corrplot()

## Follow-up graph (not great)
ggplot(df_clean, aes(gwc_perc, delta_do_hr)) + 
  geom_point(aes(color = transect_location)) + 
  geom_smooth()

df_rf <- df_clean %>% select(-chars)

set.seed(42)
rf_do <- randomForest(delta_do_hr ~ ., data = df_rf)
rf_co2 <- randomForest(d_pco2 ~ ., data = df_rf)
rf_ch4 <- randomForest(d_pch4 ~ ., data = df_rf)
rf_n2o <- randomForest(d_pn2o ~ ., data = df_rf)


plot_fi <- function(rf, dependent){
  
  r2 <- round(mean(rf$rsq), 2)
  
  plot_title = paste0(dependent, " (R2 = ", r2, ")")
  fi <- as.data.frame(rf$importance) %>% 
    tibble::rownames_to_column() %>% 
    as_tibble() 
  
  ggplot(fi, aes(x = IncNodePurity, y = reorder(rowname, IncNodePurity))) + 
    geom_col() + 
    labs(x = "Rel. Importance", y = "Predictor", title = plot_title)
}
  
plot_grid(plot_fi(rf_do, "Delta DO"), 
          plot_fi(rf_co2, "CO2"), 
          plot_fi(rf_ch4, "CH4"), 
          plot_fi(rf_n2o, "N2O"))

do_cart <- party::ctree(delta_do_hr ~ ., data = df_rf)

plot(party::ctree(d_pn2o ~ ., data = df_rf))





