## This script imports raw data for greenhouse gases measured during 24-hour soil
## and sediment incubations conducted by Alex Izquierdo during his second SULI
## term. Gases (carbon dioxide, methane, and nitrous oxide) were measured on a 
## Picarro gas analyzer for wet (soils/sediments inundated with seawater), dry
## (no inundation, acting as a control), and seawater (control) at PNNL
## MCRL. This script exports clean, Level 1 QC'ed data as partial pressures.
## Data are read in from Google Sheets.
## 
## Created: 2022-01-05 (Updated 2023-04-17 for EC1 ESS-DIVE package)
## Also updated 2023-06-22 to include timestamps
## Peter Regier
##
## NOTES: There are some issues with the raw data that need to be corrected. 
## Rather than edit the raw data, these corrections are noted here and in the 
## code where they are remedied
## 
## 
# #############
# #############


# 1. Setup ---------------------------------------------------------------------

# load packages
require(pacman)
pacman::p_load(tidyverse, 
               parsedate,
               lubridate,
               googlesheets4) # read_sheet 

## URL for data
ghg_path = "https://docs.google.com/spreadsheets/d/16EQnZtlljXpAF9RbxaG9pRzaFcI3kzFtMMCsVdA_LM8/edit#gid=844000402"


# 2. Functions -----------------------------------------------------------------

## Because the read_sheets() function makes columns into lists (and therefore 
## hard to work with) if it is confused about col_type, we first need to assemble
## strings to tell read_sheets() how to set col_types
set_col_types <- function(data, sheet_no){
  length <- read_sheet(data, sheet = sheet_no) %>% ncol() ## number of columns
  col_types_raw <- data.frame(unlist(lapply(read_sheet(data, sheet = sheet_no), class)))[,1] ## read_sheets' best guess
  col_types_list <- c() #empty vector to fill
  
  ## see ?read_sheets() for col_type abbreviations
  for(i in 1:length){
    x = col_types_raw[[i]]
    y <- ifelse(grepl("POSIX", x), "T", ## datetime
                ifelse(grepl("char", x), "c", "d")) ## character else double
    col_types_list <- append(col_types_list, y, after = length(col_types_list))
  }
  
  paste(col_types_list, collapse = "")
}

## Function to read in Picarro datasets
format_ghg_data <- function(data) {
  data %>% 
    rename("date" = `Date started`, 
           "sample_id" = `Sample name`, 
           "pco2" = `Corrected pCO2 (ppm)`, 
           "pch4" = `Corrected pCH4 (ppm)`, 
           "pn2o" = `Corrected pN2O (ppm)`) %>% 
    filter(!grepl('BC', sample_id)) %>%
    dplyr::mutate(site = as.factor(str_match(sample_id, ".*[_]([^.]+)[_].*")[,2]),
                  site = fct_recode(site, "Sediment" = "Sed",  
                                    "Transition" = "Trans"),
                  kit_id = substr(sample_id, 1, 4), 
                  year = paste0("20", substr(date, 1,2)), 
                  day = substr(date, 3, 4), 
                  month = substr(date, 5, 6), 
                  start_time = as.character(`Time started`), 
                  end_time = as.character(`Time Extracted`), 
                  date = as.Date(parse_date(paste0(year, "-", month, "-", day)))) %>%
    dplyr::select(date, start_time, end_time, kit_id, site, pco2, pch4, pn2o)
}

# 3. Import data ---------------------------------------------------------------

## raw unbinned data - we are no longer using the "dry" treatments since they're 
## difficult to compare directly to to the "wet" treatments
# ghg_dry <- read_sheet(ghg_path, sheet = 'dry', col_types = set_col_types(ghg_path, sheet = 'dry')) %>% 
#   format_ghg_data(.) %>% dplyr::mutate(type = "dry")
ghg_wet <- read_sheet(ghg_path, sheet = 'wet', col_types = set_col_types(ghg_path, sheet = 'wet')) %>% 
  format_ghg_data(.) %>% dplyr::mutate(type = "wet")

## combine into a single raw dataframe and convert negative values to NA
ghg_raw <- ghg_wet
  #bind_rows(ghg_dry, ghg_wet) %>% 
  #mutate(pco2 = ifelse(pco2 < 0, NA, pco2), 
  #       pch4 = ifelse(pch4 < 0, NA, pch4), 
   #      pn2o = ifelse(pn2o < 0, NA, pn2o))

## Time to deal with the time-stamps which are not going to be friendly
ghg_calculate_times <- ghg_raw %>% 
  mutate(start_time = ifelse(start_time == "N/A", NA, start_time), ## First, scrub non-conventional NAs
         end_time = ifelse(end_time == "N/A", NA, end_time)) %>% 
  mutate(start_datetime = parse_date(paste(date, paste0(start_time, "M"))), ## Second, add "M" and convert to datetime
         end_datetime = parse_date(paste(date + hours(24), paste0(end_time, "M")))) %>% 
  mutate(timespan = as.numeric(end_datetime - start_datetime)) #defaults to minutes # now subtract

## Based on this, there are some that are very low (~12) which are missing NAs
## so can be scrubbed, some that are ~28 that are real and were started in the 
## afternoon and run the following morning, and some that are high (>35), which
## are missing start times (defaults to midnight)
ggplot(ghg_calculate_times, aes(timespan)) + geom_density()

ghg_calculate_times %>% 
  filter(timespan < 20)

mean_timespan <- ghg_calculate_times %>% 
  filter(timespan > 15 & timespan < 30) %>% 
  summarize(mean(timespan)) %>% pull() %>% 
  round(., digits = 0)

ghg_fix_timespan_issues <- ghg_calculate_times %>% 
  filter(!is.na(pco2)) %>% ## Remove rows without GHG data (I checked this doesn't scrub any GHG data)
  mutate(start_datetime = as_datetime(ifelse(is.na(start_time), end_datetime - hours(mean_timespan), start_datetime))) %>% 
  mutate(timespan_h = as.numeric(end_datetime - start_datetime)) ## Recalculating for all to correct is.na(start_time) rows
  
## Our distribution now looks good!
ggplot(ghg_fix_timespan_issues, aes(timespan)) + geom_density()


## convenience function to ignore NAs and calculate mean
mean_   <- function(...) mean(..., na.rm = T)

## Summarize data by kit_id and site
ghg_mean <- ghg_fix_timespan_issues %>% 
  group_by(date, kit_id, site, type) %>% 
  summarize(pco2_ppm_raw = mean_(pco2),
            pch4_ppm_raw = mean_(pch4),
            pn2o_ppm_raw = mean_(pn2o),
            timespan_h = mean_(timespan_h)) %>% 
  ungroup() %>% 
  filter(kit_id != "TEST") %>% 
  filter(date < "2022-01-01")

## Calculate mean seawater gas concentrations
ghg_seawater <- ghg_mean %>% 
  filter(grepl("water", site)) %>% 
  drop_na() %>% 
  summarize(pco2_ppm_raw = mean(pco2_ppm_raw), 
            pch4_ppm_raw = mean(pch4_ppm_raw), 
            pn2o_ppm_raw = mean(pn2o_ppm_raw))


ghg_mean %>% 
  filter(type == "wet")%>% 
  filter(!(grepl("water", site))) %>% 
  drop_na() %>% 
  group_by(site) %>% 
  summarize(pco2_ppm_raw = mean(pco2_ppm_raw), 
            pch4_ppm_raw = mean(pch4_ppm_raw), 
            pn2o_ppm_raw = mean(pn2o_ppm_raw)) 


ghg_mean %>% 
  filter(grepl("water", site)) %>% 
  drop_na() %>% 
  select(-c(date))
  ggplot()

## Things to change
## Take out the seawater subtraction: we're wanting to understand the system, not the soil
## Reframe in intro: we wnat to know what happens when the landscape floods, and materials 
## are mobilized off different portions of the landscape


## Correct data for seawater gas concentrations
ghg_cor <- ghg_mean %>% 
  filter(!grepl('water', site)) %>% ## Filter out seawater rows
  mutate(pco2_ppm_c = ifelse(type == "wet", pco2_ppm_raw - ghg_seawater$pco2_ppm_raw, pco2_ppm_raw),
         pch4_ppm_c = ifelse(type == "wet", pch4_ppm_raw - ghg_seawater$pch4_ppm_raw, pch4_ppm_raw),
         pn2o_ppm_c = ifelse(type == "wet", pn2o_ppm_raw - ghg_seawater$pn2o_ppm_raw, pn2o_ppm_raw)) #%>%
# mutate(pco2_ppm_c = ifelse(type == "wet", pco2_ppm_raw - ghg_seawater$pco2_ppm_raw, pco2_ppm_raw),
#        pch4_ppm_c = ifelse(type == "wet", pch4_ppm_raw - ghg_seawater$pch4_ppm_raw, pch4_ppm_raw),
#        pn2o_ppm_c = ifelse(type == "wet", pn2o_ppm_raw - ghg_seawater$pn2o_ppm_raw, pn2o_ppm_raw)) #%>%
 # mutate(pco2_ppm_c = ifelse(pco2_ppm_c < 0, 0, pco2_ppm_c), #assuming concentrations are below detection (0) if less than seawater
  #       pch4_ppm_c = ifelse(pch4_ppm_c < 0, 0, pch4_ppm_c), 
   #      pn2o_ppm_c = ifelse(pn2o_ppm_c < 0, 0, pn2o_ppm_c))


# 4. Export dataset ------------------------------------------------------------
write_csv(ghg_cor, "data/ghg_raw.csv")

range(ghg_cor$pn2o_ppm_c, na.rm = T)


## Writing data for ESS-DIVE package
# ghg_ess_dive <- ghg_cor %>% 
#   select(-date, -contains("raw")) %>% 
#   mutate(campaign = "EC1") %>% 
#   relocate(kit_id, campaign)
# 
# write_csv(ghg_ess_dive %>% filter(site == "Sediment"), "data/ec1_ghg_L2_ess_dive/EC1_Sediment_GHG_L2.csv")
# write_csv(ghg_ess_dive %>% filter(site != "Sediment"), "data/ec1_ghg_L2_ess_dive/EC1_Soil_GHG_L2.csv")

