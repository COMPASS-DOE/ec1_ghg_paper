## This script takes raw Firesting data and ingests, processes, and plots it.
## There are a couple key things: 1) making the assumption that 0.01 mg/L is not
## distinguishable from 0 mg/L due to Firesting sensitivity means we cut time-series
## at 0.01 mg/L. This is distinct from the manual's reported 0.1 mg/L accuracy
## at higher DO values. https://www.pyroscience.com/en/products/all-sensors/oxrob10
## Firesting oxygen drawdown data collected by Alex I for his second SULI project.
##
## 2021-12-08 (Updated 2023-04-17)
## Peter Regier
##
# #############
# #############

# 1. SETUP ---------------------------------------------------------------------

## load pacman, then load packages
require(pacman)
p_load(tidyverse, 
       magrittr, ## 'set_colnames'
       googlesheets4, 
       googledrive,
       plotly,
       ggpubr, ## 'stat_compare_means'
       parsedate, ## 'parse_date'
       stringr, ## extract string parts
       broom, ## fit lm models for each time-series
       tictoc)

## set ggplot theme
theme_set(theme_bw())

## set firesting names
firesting_names <- c("date", "time", "dt_s", "do_mgl", "dphi", 
                     "intensity_mv", "light_mv", "do_status", "temp_date", "temp_time", 
                     "temp_dt_s", "temp_c", "temp_status", "press_date", "press_time", 
                     "press_dt_s", "pressure_mbar", "press_status")

## Set directory for raw firesting data
file_location <- "data/firesting_raw/"

## Set a minimum value for DO based on instrument sensitivity
do_threshold = 0.01

# 2. FUNCTIONS -----------------------------------------------------------------

import_firesting_data <- function(folder) {
  
  ## Grab kit name
  kit_id <- str_match(folder, "[K][0-9]{3}")[,1]
  
  ## First, set up 4 filepaths for the four channels (each is a dataset)
  ## For SULI2 exps: Ch1 = sediment, Ch2 = upland, Ch3 = wetland, Ch4 = transition
  sed_filepath <- paste0(file_location, folder, "/ChannelData/Firesting O2 (4 Channels)_(A Ch.1)_Oxygen.txt")
  up_filepath <- paste0(file_location, folder, "/ChannelData/Firesting O2 (4 Channels)_(A Ch.2)_Oxygen.txt")
  wet_filepath <- paste0(file_location, folder, "/ChannelData/Firesting O2 (4 Channels)_(A Ch.3)_Oxygen.txt")
  tr_filepath <- paste0(file_location, folder, "/ChannelData/Firesting O2 (4 Channels)_(A Ch.4)_Oxygen.txt")
  
  ## Next, create four dataframes for the four channels
  sed_data <- read_delim(sed_filepath, delim = "\t", skip = 25,  col_names = F) %>% 
    magrittr::set_colnames(firesting_names) %>% 
    mutate(datetime = parse_date(paste(date, time))) %>% 
    select(datetime, dt_s, temp_c, do_mgl) %>% 
    mutate(site = "Sediment", 
           kit_id = kit_id)
  
  up_data <- read_delim(up_filepath, delim = "\t", skip = 25,  col_names = F) %>% 
    magrittr::set_colnames(firesting_names) %>% 
    mutate(datetime = parse_date(paste(date, time))) %>% 
    select(datetime, dt_s, temp_c, do_mgl) %>% 
    mutate(site = "Upland", 
           kit_id = kit_id)
  
  wet_data <- read_delim(wet_filepath, delim = "\t", skip = 25,  col_names = F) %>% 
    magrittr::set_colnames(firesting_names) %>% 
    mutate(datetime = parse_date(paste(date, time))) %>% 
    select(datetime, dt_s, temp_c, do_mgl) %>% 
    mutate(site = "Wetland", 
           kit_id = kit_id)
  
  tr_data <- read_delim(tr_filepath, delim = "\t", skip = 25,  col_names = F) %>% 
    magrittr::set_colnames(firesting_names) %>% 
    mutate(datetime = parse_date(paste(date, time))) %>% 
    select(datetime, dt_s, temp_c, do_mgl) %>% 
    mutate(site = "Transition", 
           kit_id = kit_id)
  
  ## Finally, combine into a single dataframe
  df <- bind_rows(sed_data, up_data, wet_data, tr_data)
  
  print(head(df))
  
  return(df)
}

# 2. IMPORT DATA ---------------------------------------------------------------

## Create a list of folders (each is a separate experiment) to import
folders <- list.files(file_location)

## Import all data and merge
tic("read in firesting data") ## time how long this takes, just for fun
df <- map(folders, import_firesting_data) %>% 
  bind_rows()
toc()

## import metadata to match kit to region
region_metadata <- read_sheet("https://docs.google.com/spreadsheets/d/19F1oS-DBvxQlU1EYXtciYkhu6TNA7fuCQ0Ts2NRWQlk/edit#gid=735420340", 
                              sheet = 1) %>% 
  rename("kit_id" = `Kit ID`, 
         "region" = "Region") %>% 
  select(kit_id, region)


# 3. Initial cleaning ----------------------------------------------------------

## Clean up data (limit to 24 hours, then trim to first minimum do value)
df_initial_clean <- df %>% 
  filter(dt_s < 60 * 60 * 24) %>% ## trim to max 24hrs
  rename("transect_location" = site) %>% 
  filter(do_mgl >= do_threshold)


# 4. Clean time-series starts --------------------------------------------------

## We have to decide how to take care of values logged prior to O2 drawdown.
## This point is easy to identify visually for many but hard for some. I'm going
## to use a manual eyeballs-based approach, but will document why choices
## are made. Values will be selected for dt_s for each time-series, put in the
## dataframe below, and then time-series will be trimmed

trim_start_raw <- expand_grid(kit_id = unique(df_initial_clean$kit_id), 
                              transect_location = unique(df_initial_clean$transect_location)) 

## Let's list all time-series, by kit, with the time things should be >= 
##K018: all have clear drop-offs
##K025: ditto
##K015: ditto
##K017: first tricky one: W/TR have clear drops, S/U don't need trimming?
##K022: So, this is a weird one: both S and W start below 1, so should probably
### scratch. Neither T or U need to be trimmed
##K037: generally okay, weak dropoffs for T and U but timing generally matches order
##K021: all clear
##K050: ditto
##K019: ditto
##K026: ditto
##K034: ditto
##K030: Transition is a little unclear, others are clear
##K059: no trim on S, others clear
##K041: same as K059
##K054: same as K059
##K044: all clear
##K062: ditto
##K052: W shows no clear 

## Create vector for trimming (order: )
dt_s_trim <- c(128, 309, 158, 249, ## K018
               37, 217, 97, 187, ##K025 
               164, 224, 194, 224, ##K015
               0, 44, 0, 44, ##K017
               0, 0, 0, 0, ##K022
               35, 95, 65, 0, ## K037
               44, 194, 74, 164, ##K021
               49, 410, 260, 350, ##K050
               66, 276, 126, 216, ##K019
               54, 204, 84, 174, ##K026
               36, 187, 97, 157, ##K034
               59, 90, 150, 240, ##K030
               0, 175, 85, 145, ##K059
               0, 320, 170, 260, ##K041
               0, 245, 34, 95, ##K054
               33, 183, 92, 124, ##K044
               59, 179, 88, 150, ##K062
               34, 123, 62, 154) ##K052 - note that W is a tricky call here

trim_df <- tibble(trim_start_raw, 
                  dt_s_trim)

## Using this plot, we manually identify where to trim each time-series
i = 18
p <- df_initial_clean %>% 
  filter(kit_id == unique(trim_df$kit_id)[[i]]) %>% 
  ggplot(aes(dt_s, do_mgl, color = transect_location)) + 
  geom_line() + 
  scale_x_log10() + 
  ggtitle(unique(trim_df$kit_id)[[i]])

ggplotly(p)


## Now, construct a tibble for trimming the start of each time-series
trim_start <- trim_start_raw %>% 
  mutate(to_trim = dt_s_trim)

df_trim_start <- inner_join(df_initial_clean, trim_start, by = c("kit_id", "transect_location")) %>% 
  group_by(kit_id, transect_location) %>% 
  filter(dt_s >= to_trim)


# 5. Trim end ------------------------------------------------------------------
  
## Trim each time-series based on the first instance of the min. DO value
df_trim_end <- df_trim_start  %>% 
  filter(dt_s <= dt_s[which.min(do_mgl)])

# 6. Remove nonsensical time-series --------------------------------------------

## Based on visual inspection, only two time-series need to be removed: 
## K022: both starting < 1 mg/L: can't reliably estimate drawdown
df_trimmed <- df_trim_end %>% 
  filter(!(kit_id == "K022" & transect_location == "Sediment")) %>% 
  filter(!(kit_id == "K022" & transect_location == "Wetland"))


df_rates <- df_trimmed %>% 
  group_by(kit_id, transect_location) %>% 
  dplyr::summarize(max_do = max(do_mgl),
                   min_do = min(do_mgl), 
                   min_dt = min(dt_s), 
                   max_dt = max(dt_s)) %>% 
  mutate(delta_do_s = (max_do - min_do) / (max_dt - min_dt)) %>% 
  mutate(transect_location = as.factor(transect_location)) %>%
  select(kit_id, transect_location, delta_do_s) %>% 
  mutate(do_mgL_hr = delta_do_s * 60 * 60, 
         # (mg/L)*(g/1000mg)*(mol/32g)*(10^6umol/mol)
         do_uM_hr = do_mgL_hr * (1/1000) * (1/32) * 10^6,
         transect_location = fct_relevel(transect_location, c("Sediment", "Wetland", "Transition", "Upland"))) %>% 
  left_join(., region_metadata, by = "kit_id") 

ggplot(df_rates, aes(transect_location, do_uM_hr)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.1, alpha = 0.5) + 
  scale_y_log10()

df_rates_final <- df_rates %>% 
  dplyr::select(-delta_do_s)

# 7. Write out final dataset ---------------------------------------------------

write_csv(df_rates_final, "data/230417_do_consumption_final.csv")


