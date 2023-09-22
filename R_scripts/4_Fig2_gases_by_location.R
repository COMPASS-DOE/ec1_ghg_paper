## New and improved script to make a new gas figure that includes everything
##
## 2022-04-22 (updated 6/22/23)
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
       janitor,
       ggConvexHull,
       confintr,
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
df <- read_csv("data/230623_master_data.csv") %>% 
  select(all_of(common_cols), contains("uM_hr")) %>% 
  mutate(transect_location = str_to_sentence(transect_location)) %>% 
mutate(transect_location = fct_relevel(transect_location, c("Sediment", "Wetland", "Transition", "Upland")))

## Also read in seawater concentrations and calculate means
sw <- read_csv("data/230623_ghg_saltwater.csv") %>% 
  select(contains("uM_hr")) %>% 
  summarize(across(everything(), mean))


# 3. Make figures and export ---------------------------------------------------

make_boxplot <- function(var, y_label){
  # library(agricolae)
  # 
  # # do hsd
  # ## 1. create a new dataframe selecting the variables
  # df_new <- df %>% dplyr::select({{var}}, transect_location) %>% rename(variable = {{var}})
  # 
  # ## 2. calculate Tukey's HSD
  # a = aov(variable ~ transect_location, data = df_new)
  # h = HSD.test(a, "transect_location")
  # 
  # ## 2b. convert HSD results into a dataframe for the ggplot
  # h_df <- h$groups %>% 
  #   as.data.frame() %>% 
  #   rownames_to_column("transect_location") %>% 
  #   rename(hsd = groups) %>% 
  #   dplyr::select(transect_location, hsd)
  # 
  # ## 3. calculate the maximum value of the variable. 
  # ## will be used to determine the y-axis position of the HSD letters
  # ## setting it dynamic as ymax + ymax/10 (will be done in the ggplot code)
  # ymax <- df %>% dplyr::select({{var}}) %>% max()
  # 
  # # boxplot
  # ggplot(df, aes(transect_location, {{var}}, fill = transect_location)) + 
  #   geom_boxplot(show.legend = F, outlier.alpha = 0, width = 0.5) +
  #   geom_jitter(color = "black", show.legend = F, width = 0.05) + 
  #   scale_fill_manual(values = color_theme) + 
  #   labs(x = "Location", y = y_label) +
  #   # add HSD letters using geom_text, and set the y-position to be dynamic
  #   geom_text(data = h_df, aes(y = ymax+ymax/10, label = hsd))
  
  ggplot(df, aes(transect_location, {{var}}, fill = transect_location)) + 
    geom_boxplot(show.legend = F, outlier.alpha = 0, width = 0.5) +
    geom_jitter(color = "black", show.legend = F, width = 0.05) + 
    scale_fill_manual(values = color_theme) + 
    labs(x = "Location", y = y_label) + 
    stat_compare_means(comparisons = compare_transect, label = "p.signif")
  }


p_do <- make_boxplot(do_uM_hr, "DO consumption (µM/hr)")
p_co2 <- make_boxplot(co2_uM_hr, "CO2 production (µM/hr)") + 
  geom_hline(yintercept = sw$co2_uM_hr, linetype = "dashed")
p_ch4 <- make_boxplot(ch4_uM_hr, "CH4 production (µM/hr)") + 
  geom_hline(yintercept = sw$ch4_uM_hr, linetype = "dashed")
p_n2o <- make_boxplot(n2o_uM_hr, "N2O production (µM/hr)") + 
  geom_hline(yintercept = sw$n2o_uM_hr, linetype = "dashed")

# p_co2_rates <- make_boxplot(pco2_mgL_hr_wet, "CO2 production (mg/L/hr)")
# p_ch4_rates <- make_boxplot(pch4_mgL_hr_wet, "CH4 production(mg/L/hr)")
# p_n2o_rates <- make_boxplot(pn2o_mgL_hr_wet, "N2O production(mg/L/hr)")

plot_grid(p_do, p_co2, p_ch4, p_n2o, nrow = 1, labels = c("A", "B", "C", "D"), align = "hv")
ggsave("figures/2_Fig2_gases_by_location.png", width = 12, height = 5)


# # 3.5. Test out Tukey's HSD letters (will incorporate if it improves figure) ---
# 
# library(multcomp)
# 
# # Compute Tukey's HSD test
# tukey_results <- TukeyHSD(aov(do_uM_hr ~ transect_location, data = df))
# 
# # Create the boxplot
# p <- ggplot(df, aes(transect_location, {{var}}, fill = transect_location)) + 
#   geom_boxplot(show.legend = FALSE, outlier.alpha = 0, width = 0.5) +
#   geom_jitter(color = "black", show.legend = FALSE, width = 0.05) + 
#   scale_fill_manual(values = color_theme) + 
#   labs(x = "Location", y = y_label)
# 
# # Add significance letters
# p + geom_text(data = tukey_results$`transect_location`,
#                    aes(x = as.numeric(rownames(tukey_results$`transect_location`)),
#                        y = max({{var}}) + 0.1, label = .$grp),
#                    inherit.aes = FALSE, show.legend = FALSE)




# 4. Calculate statistics (mean and standard error) for Figure 2 ---------------

## Set up a helper function to paste mean and se together
n_sigfigs = 3
se <- function(var){paste0(round(mean({{var}}), n_sigfigs), 
                          "±", 
                          round(sd({{var}}) / sqrt(length({{var}})), n_sigfigs))}

mean_range <- function(var){paste0(signif(mean({{var}}), n_sigfigs), 
                                   " (",
                                   signif(min({{var}}), n_sigfigs), 
                                   " - ", 
                                   signif(max({{var}}), n_sigfigs), 
                                   ")")}

# sd_ <- function(var){paste0(round(mean({{var}}), n_sigfigs), 
#                             "±", 
#                             round(sd({{var}}), n_sigfigs))}
# 
# df %>% filter(transect_location == "Sediment") %>% 
#   select(do_uM_hr) %>% 
#   summarize(mean = mean(do_uM_hr), 
#             sd = sd(do_uM_hr),
#             n = n()) %>% 
#   mutate(se = sd / sqrt(n), 
#          lower_ci = mean - qt(0.975, df = n - 1) * se, 
#          upper_ci = mean + qt(0.975, df = n - 1) * se)


## Broken out by location
transect_stats_by_location <- df %>% 
  select(transect_location, contains("_uM_hr")) %>% 
  group_by(transect_location) %>% 
  summarize(across(is.numeric, mean_range))
  
## Across all locations
transect_stats <- df %>% 
  select(contains("_uM_hr")) %>% 
  summarize(across(is.numeric, mean_range)) %>% 
  mutate(transect_location = "All locations") %>% 
  relocate(transect_location)

sw_stats <- read_csv("data/230623_ghg_saltwater.csv") %>% 
  select(contains("uM_hr")) %>% 
  summarize(across(is.numeric, mean_range)) %>% 
  mutate(do_uM_hr = NA) %>% 
  mutate(transect_location = "Saltwater") %>% 
  relocate(transect_location)

fig2_stats <- bind_rows(transect_stats,
                        transect_stats_by_location, 
                        sw_stats)

write_csv(fig2_stats, "data/230920_Table_S1.csv")


my_comparisons = list(c("Saltwater", "Sediment"),
                   c("Saltwater", "Wetland"),
                   c("Saltwater", "Transition"),
                   c("Saltwater", "Upland"))

bind_rows(df %>% 
            select(transect_location, contains("_uM_hr")), 
          read_csv("data/230623_ghg_saltwater.csv") %>% 
            select(transect_location, contains("_uM_hr"))) %>% 
  ggplot(aes(transect_location, n2o_uM_hr)) + 
  geom_boxplot() + 
  stat_compare_means(comparisons = my_comparisons)


