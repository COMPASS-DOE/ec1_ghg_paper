## This script imports raw data for greenhouse gases measured during 24-hour soil
## and sediment incubations conducted by Alex Izquierdo during his second SULI
## term. Gases (carbon dioxide, methane, and nitrous oxide) were measured on a 
## Picarro gas analyzer for wet (soils/sediments inundated with seawater), dry
## (no inundation, acting as a control), and seawater (also a control) at PNNL
## MCRL. This script exports clean, Level 1 QC'ed data.
## Data are read in from Google Sheets.
## 
## Created: 2022-01-05
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
           "time" = `Time started`, 
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
                  date = as.Date(parse_date(paste0(year, "-", month, "-", day)))) %>% 
    dplyr::select(date, kit_id, site, pco2, pch4, pn2o)
}

# 3. Import data ---------------------------------------------------------------

## raw unbinned data
ghg_dry <- read_sheet(ghg_path, sheet = 'dry', col_types = set_col_types(ghg_path, sheet = 'dry')) %>% 
  format_ghg_data(.) %>% dplyr::mutate(type = "dry")
ghg_wet <- read_sheet(ghg_path, sheet = 'wet', col_types = set_col_types(ghg_path, sheet = 'wet')) %>% 
  format_ghg_data(.) %>% dplyr::mutate(type = "wet")

## combine into a single raw dataframe and convert negative values to NA
ghg_raw <- bind_rows(ghg_dry, ghg_wet) %>% 
  mutate(pco2 = ifelse(pco2 < 0, NA, pco2), 
         pch4 = ifelse(pch4 < 0, NA, pch4), 
         pn2o = ifelse(pn2o < 0, NA, pn2o))

## convenience function to ignore NAs and calculate mean
mean_   <- function(...) mean(..., na.rm=T)

## Summarize data by kit_id and site
ghg_mean <- ghg_raw %>% 
  group_by(date, kit_id, site, type) %>% 
  summarize(pco2 = mean_(pco2),
            pch4 = mean_(pch4),
            pn2o = mean_(pn2o)) %>% 
  ungroup() %>% 
  filter(kit_id != "TEST") %>% 
  filter(date < "2022-01-01")

## Calculate mean seawater gas concentrations
ghg_seawater <- ghg_mean %>% 
  filter(grepl("water", site)) %>% 
  drop_na() %>% 
  summarize(pco2 = mean(pco2), 
            pch4 = mean(pch4), 
            pn2o = mean(pn2o))

## Correct data for seawater gas concentrations
ghg_cor <- ghg_mean %>% 
  filter(!grepl('water', site)) %>% ## Filter out seawater rows
  mutate(pco2_c = ifelse(type == "wet", pco2 - ghg_seawater$pco2, pco2),  
         pch4_c = ifelse(type == "wet", pch4 - ghg_seawater$pch4, pch4),  
         pn2o_c = ifelse(type == "wet", pn2o - ghg_seawater$pn2o, pn2o)) %>% 
  mutate(pco2_c = ifelse(pco2_c < 0, 0, pco2_c), 
         pch4_c = ifelse(pch4_c < 0, 0, pch4_c), 
         pn2o_c = ifelse(pn2o_c < 0, 0, pn2o_c))


# 4. Export dataset ------------------------------------------------------------
write_csv(ghg_cor, "data/ghg.csv")
