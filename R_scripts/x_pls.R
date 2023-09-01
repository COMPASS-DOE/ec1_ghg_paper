## Exploring PLS as an alternative to PCA, where we use our 4 gas rates as our
## dependents, and our other variables as independents


require(pacman)
p_load(tidyverse,
       ggConvexHull,
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
  select(-c(do_mgL_hr, lat, lon)) %>% 
  drop_na()

data <- data_all %>%
  select(where(is.numeric))


## Structured PLS attempt
# 1. Pre-process dataset - tbd what this means
# 2. Construct an all-variables model - reference point
# 3. Use VIF to determine which variables to include
# 4. Construct revised model
# 5. ID assumptions to test, and test
# 6. Restructure model / data as needed to meet assumptions
## Some background, per https://personal.utdallas.edu/~herve/Abdi-PLS-pretty.pdf
## PLS combines PCA and multiple regression
## 




### Initial PLS attempt

normalized_data <- as_tibble(as.data.frame(scale(data)))

pls_model <- plsr(do_uM_hr + co2_uM_hr + ch4_uM_hr + n2o_uM_hr ~ ., data = normalized_data, ncomp = 2)

summary(pls_model)

predicted_values <- fitted(pls_model)

scores_df <- as_tibble(matrix(scores, ncol = 2, dimnames = list(NULL, c("Comp.1", "Comp.2")))) %>% 
  mutate(transect_location = data_all$transect_location)

ggplot(scores_df, aes(x = Comp.1, y = Comp.2, color = transect_location)) +
  geom_point(size = 3) +
  geom_convexhull(aes(group = transect_location, fill = transect_location), alpha = 0.2) +
  labs(x = "Component 1", y = "Component 2") +
  theme_minimal()



############

p_load(MASS, glmnet, corrr)

data_trim <- data %>% 
  dplyr::select(-c(alk_mgl_caco3, latitude, longitude, 
                   mat_c, monthly_precip))

cor(data_trim)






x <- as.matrix(data %>% dplyr::select(-transect_num))
y <- data$transect_num






dependent_var <- normalized_data %>% dplyr::select(transect_num)

independent_vars <- data %>% dplyr::select(contains("uM_hr"))

lda_model <- lda(transect_num ~ ., data = as.data.frame(normalized_data))
#lda_model <- lda(transect_num ~ do_uM_hr + co2_uM_hr + ch4_uM_hr + n2o_uM_hr, data = as.data.frame(normalized_data))

summary(lda_model)

scaled_scores <- predict(lda_model)$x %>% 
  as_tibble() %>% 
  mutate(transect_location = data_all$transect_location)

p <- ggplot(scaled_scores, aes(LD1, LD2, color = transect_location)) + 
  geom_point() +
  geom_convexhull(aes(group = transect_location, fill = transect_location), alpha = 0.2)

coefficients <- coef(lda_model) %>% 
  as.data.frame() %>%
  rownames_to_column() 

p + geom_point(data = coefficients, aes(x = LD1, y = LD2), color = "black")
  
  
  geom_segment(data = coefficients, aes(xend = 0, yend = 0, x = LD1, y = LD2),
                 arrow = arrow(length = unit(0.2, "inches")), color = "blue", size = 1)

############

p_load(plspm)

## Dependents
outer_model <- cbind("do_uM_hr", "co2_uM_hr", "ch4_uM_hr", "n2o_uM_hr")

num_dependent <- ncol(outer_model)

num_independent <- ncol(data) - num_dependent # Assuming num_independent is the number of independent variables

# Create the inner model matrix dynamically
inner_model <- matrix(0, nrow = num_independent, ncol = num_independent)
diag(inner_model) <- 1  # Set the diagonal elements to 1

inner_model <- matrix(0, nrow = num_independent, ncol = num_dependent)
colnames(inner_model) <- colnames(data)[1:num_dependent]  # Set column names (dependent variables)
rownames(inner_model) <- colnames(data)[(num_dependent + 1):ncol(data)]

diag(inner_model) <- 1

# Print the inner model matrix
print(inner_model)

# Perform PLS-FS
pls_fs <- plspm(data, outer_model, inner_model, scaled = TRUE)

# Get the variable weights (importance) from the PLS-FS results
variable_weights <- pls_fs$loadings$weights

# View the variable weights
print(variable_weights)





# Assuming the formula for the PLS model is defined as follows:
formula <- cbind(A, B, C, D) ~ E1 + E2 + E3 + ... # Add all independent variables

# Fit the initial PLS model with all independent variables
pls_model <- plsr(formula, data = data, ncomp = 3) # Specify the desired number of components

# Function to perform stepwise regression based on AIC
stepwise_remove <- function(pls_model, data) {
  current_formula <- formula(pls_model)
  
  while (length(coef(pls_model)) > 1) { # Keep iterating until only the intercept is left
    aic_values <- c()
    coef_names <- names(coef(pls_model))
    
    for (coef_name in coef_names) {
      if (coef_name == "(Intercept)") {
        next
      }
      # Temporarily remove the current independent variable from the formula
      new_formula <- update(current_formula, . ~ . - eval(parse(text = coef_name)))
      
      # Fit the PLS model with the updated formula
      temp_model <- plsr(new_formula, data = data, ncomp = 3) # Specify the desired number of components
      
      # Calculate the AIC value for the temporary model
      aic_values <- c(aic_values, AIC(temp_model))
    }
    
    # Find the independent variable with the smallest AIC value and remove it from the formula
    min_aic_index <- which.min(aic_values)
    if (min_aic_index != 0) {
      coef_to_remove <- coef_names[min_aic_index + 1] # Add 1 to account for "(Intercept)"
      current_formula <- update(current_formula, . ~ . - eval(parse(text = coef_to_remove)))
      pls_model <- plsr(current_formula, data = data, ncomp = 3) # Refit the PLS model
    } else {
      break # No variable to remove, exit the loop
    }
  }
  
  return(pls_model)
}

# Call the stepwise_remove function to perform stepwise regression
pls_model_optimized <- stepwise_remove(pls_model, data)

# View the final formula and summary of the optimized PLS model
print(formula(pls_model_optimized))
summary(pls_model_optimized)

