

require(pacman)
p_load(tidyverse,
       mdatools,
       car,
       janitor,
       ggConvexHull,
       mixOmics,
       pls)


## Load in all data
df_raw <- read_csv("data/230623_master_data.csv") %>% 
  mutate(transect_location = str_to_sentence(transect_location)) %>% 
  mutate(transect_location = fct_relevel(as.factor(transect_location), 
                                         c("Sediment", "Wetland", "Transition", "Upland")), 
         transect_num = case_when(transect_location == "Sediment" ~ 1, 
                                  transect_location == "Wetland" ~ 2,
                                  transect_location == "Transition" ~ 3,
                                  transect_location == "Upland" ~ 4))

climate_raw <- read_csv("data/230223_climate_data.csv")

soil_raw <- read_csv("data/230223_soil_data.csv")

data_all <- inner_join(df_raw, climate_raw, by = "kit_id") %>% 
  inner_join(soil_raw, by = "kit_id") %>% 
  filter(do_uM_hr < 1000) %>% 
  dplyr::select(-c(do_mgL_hr, lat, lon, kit_id, region, campaign)) %>% 
  #dplyr::select(-c(sal_psu, bd, doc_mgl, tdn_mgl)) %>% 
  #dplyr::select(-c(contains("alk"), tss_mgl, monthly_precip, ph)) %>% 
  #dplyr::select(transect_location, contains("uM_hr"), gwc_perc, tc_perc, elevation_m) %>% 
  drop_na()

x0 <- data_all %>% dplyr::select(-transect_location) %>% 
  dplyr::select(-c(contains("uM_hr"))) %>% as.data.frame()
y0 <- data_all %>% dplyr::select(-transect_location) %>% 
  #dplyr::select(contains("uM_hr")) %>% as.data.frame()
  dplyr::select(do_uM_hr) %>% as.data.frame()

pls_model <- mdatools::pls(x0, y0, ncomp = 2)

## All vars: 6%, R2 = 0.04 - 0.2

summary(pls_model)

plotVIPScores(pls_model, ncomp = 2, type = "h", show.labels = TRUE)

x_loadings <- as.data.frame(pls_model$loadings$X) %>% 
  rownames_to_column() %>% 
  as_tibble()

y_loadings <- as.data.frame(pls_model$loadings$Y) %>% 
  rownames_to_column() %>% 
  as_tibble()

ggplot() + 
  geom_label(data = x_loadings, aes(comp1, comp2, label = rowname)) + 
  
plotScores(pls_model)


x <- data_all %>% dplyr::select(-transect_location) %>% as.data.frame()
y <- data_all %>% pull(transect_location)

plsda_model <- mdatools::plsda(x, y, ncomp = 2, scale = T)

summary(plsda_model)
plsda_model$xloadings

scores <- as.data.frame(pred$y.pred) %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(transect_location = y) %>% 
  mutate(comp1 = case_when(transect_location == "Sediment" ~ comp_1_sediment, 
                           transect_location == "Wetland" ~ comp_1_wetland, 
                           transect_location == "Transition" ~ comp_1_transition, 
                           transect_location == "Upland" ~ comp_1_upland)) %>% 
  mutate(comp2 = case_when(transect_location == "Sediment" ~ comp_2_sediment, 
                           transect_location == "Wetland" ~ comp_2_wetland, 
                           transect_location == "Transition" ~ comp_2_transition, 
                           transect_location == "Upland" ~ comp_2_upland))

loadings <- bind_rows(as.data.frame(plsda_model$xloadings) %>% 
            rownames_to_column() %>% 
            as_tibble() %>% 
            clean_names() %>% 
            mutate(type = "Variable"), 
          as.data.frame(plsda_model$yloadings) %>% 
            rownames_to_column()  %>% 
            clean_names() %>% 
            mutate(type = "Location"))

ggplot(data = scores, aes(comp1, comp2, color = transect_location)) + 
  geom_point() +
  geom_convexhull(aes(fill = transect_location), alpha = 0.2)
  #geom_segment(data = loadings, aes(comp_1, comp_2, color = type), xend = 0, yend = 0) + 
  #geom_label(data = loadings, aes(comp_1, comp_2, label = rowname)) 
  
??lda

plotXYScores(plsda_model)




pred <- predict(plsda_model, x)

plot(plsda_model)

plotVIPScores(plsda_model, ncomp = 2, type = "h", show.labels = TRUE)

plsda_model2 <- mixOmics::plsda(x, y, ncomp = 2, scale = T)

plotIndiv(plsda_model2, ellipse = TRUE, legend =TRUE)


plotMisclassified(plsda_model)

biplot(plsda_model)

plsda_model$yloadings



scores(plsda_model)


#####

## Load packages
require(pacman)
p_load(pls, caret, e1071)

## Set up dataset
predictors = c("gwc_perc", "tc_perc", "tn_perc", "doc_mgl", "tdn_mgl", "ph", 
               "sal_psu", "alk_mgl_caco3", "tss_mgl", "latitude", "longitude",
               "elevation_m", "mat_c", "monthly_precip", "bd", "clay", "sand", 
               "silt", "transect_num")

pls_data <- data_all %>% 
  dplyr::select(contains("uM_hr"), all_of(predictors))

## Make initial model
set.seed(42)  # For reproducibility
trainIndex <- createDataPartition(data_all$do_uM_hr, p = 0.8, list = FALSE)
trainData <- data_all[trainIndex, ]
testData <- data_all[-trainIndex, ]

ctrl <- trainControl(method = "cv", number = 10)

# Create the PLS model with hyperparameter tuning (number of PLS components)
pls_model <- train(do_uM_hr ~ ., data = trainData, method = "pls",
                   trControl = ctrl)

pls_model1 <- train(do_uM_hr ~ ., data = data_all, method = "pls",
                    trControl = ctrl)

lm_model <- lm(do_uM_hr ~ ., data = pls_data)

vif_values <- vif(lm_model)

high_vif_threshold <- 5
low_vif_vars <- rownames(vif_values[vif_values < high_vif_threshold])

## OOPS. Big problem, we have aggressive collinearity

p_load(klaR)

predictors = pls_data %>% dplyr::select(-transect_num) %>% 
  as.data.frame()

write_csv(pls_data, "data/230801_pls_ex.csv")





#################

p_load(randomForest, 
       ranger,
       tidymodels)

rf_data <- pls_data


plot_fi <- function(data){
  rf_model <- ranger(transect_num ~ ., data = data, importance = "impurity")
  
  print(rf_model)
  
  var_names <- rf_model$variable.importance
  col_names <- c("predictor", "raw_fi")
  
  ## Convert feature importance to a tibble with variables as a column
  fi0 <- as.data.frame(var_names) %>% 
    tibble::rownames_to_column() %>% 
    as_tibble()
  
  ## Rename columns
  colnames(fi0) = col_names
  
  fi0 %>% 
    arrange(raw_fi) %>% 
    ggplot(aes(raw_fi, fct_reorder(predictor, raw_fi))) + 
    geom_col()
  
}

plot_fi(rf_data)

vars_to_keep <- fi0 %>% 
  filter(raw_fi > max(fi0$raw_fi) / 10) %>% 
  filter(predictor != "latitude" & predictor != "longitude")

plot_fi(rf_data %>% dplyr::select(transect_num, all_of(vars_to_keep %>% pull(predictor))))


### PCA based on RF loadings

theme_set(theme_bw())

p_load(ggfortify,
       ggpubr,
        ggConvexHull)

pca_data = rf_data %>% dplyr::select(all_of(vars_to_keep %>% pull(predictor))) %>% 
                                       mutate(transect_location = data_all$transect_location)

pca0 <- prcomp(pca_data %>% dplyr::select(-transect_location), scale. = TRUE, retx = T)

loadings <- as_tibble(pca0$x) %>% 
  dplyr::select(PC1, PC2) %>% 
  mutate(PC1 = abs(PC1), 
         PC2 = abs(PC2), 
         total = PC1 + PC2) %>% 
  arrange(total)

loadings %>% 
  mutate(transect_location = data_all$transect_location) %>% 
  ggplot(aes(transect_location, PC1)) + 
  geom_boxplot() + 
  stat_compare_means()


## 230720 - figure out how to iteratively remove variables in a way that maximizes
## useful information from PCA

rf_data %>% 
  mutate()

autoplot(pca0, data = pca_data, colour = 'transect_location',
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, size = 3) +
  geom_convexhull(aes(group = transect_location, fill = transect_location), alpha = 0.2) 


