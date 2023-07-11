## This script imports raw data for greenhouse gases measured during 24-hour soil
## and sediment incubations conducted by Alex Izquierdo during his second SULI
## term. Gases (carbon dioxide, methane, and nitrous oxide) were measured on a 
## Picarro gas analyzer for soil/sediment samples flooded with seawater equilibrated
## to atmosphere. Gas samples were collected at the end of each incubation.
##
## Largely based on archive/0_prep_ghg_data.R
##
## Created: 2022-06-23
## Peter Regier
## 
# #############
# #############


# 1. Setup ---------------------------------------------------------------------

# load packages
require(pacman)
pacman::p_load(tidyverse, 
               parsedate,
               janitor,
               lubridate) 

theme_set(theme_bw())


# 2. Read data -----------------------------------------------------------------

## Read in raw data and extract kit_id and transect_location then trim to useful columns
ghg_raw <- read_csv("data/230623_ghg_raw_dataset.csv") %>% 
  clean_names() %>% 
  filter(!grepl('BC', sample_name)) %>%
  dplyr::mutate(transect_location = as.factor(str_match(sample_name, ".*[_]([^.]+)[_].*")[,2]),
                transect_location = fct_recode(transect_location, "Sediment" = "Sed",  
                                  "Transition" = "Trans"),
                kit_id = substr(sample_name, 1, 4), 
                transect_location = case_when(transect_location == "Seawater" ~ "Saltwater", 
                                              TRUE ~ transect_location)) %>% 
  select(kit_id, transect_location, contains("date"), contains("time"), contains("datetime"), contains("corrected")) %>% 
  select(-contains("_sd_")) # remove standard deviations


# 3. Calculate timespans -------------------------------------------------------

## Do some funky formatting to get timespans for each sample
ghg_timespans_corrected <- ghg_raw %>% 
  mutate(time_started = as.character(ifelse(time_started == "N/A", NA, time_started)), ## First, scrub non-conventional NAs
         time_extracted = as.character(ifelse(time_extracted == "N/A", NA, time_extracted))) %>% 
  mutate(start_datetime = parse_date(paste(date_started, paste0(time_started, "M"))), ## Second, add "M" and convert to datetime
         end_datetime = parse_date(paste(date_started, paste0(time_extracted, "M"))) + days(1)) %>% 
  mutate(timespan = as.numeric(end_datetime - start_datetime)) %>% #defaults to minutes # now subtract
  mutate(across(contains("corrected"), as.numeric))

## Plot, notice some time-stamps well below 20 hrs
ggplot(ghg_timespans_corrected, aes(timespan)) + geom_density()

## Looks like these were run in the morning, so we're good to go on timespans
ghg_timespans_corrected %>% 
  filter(timespan < 20)


# 4. Calculate means because samples were run in triplicate --------------------

## convenience function to ignore NAs and calculate mean
mean_ <- function(...){mean(..., na.rm = T)}

## Summarize data by kit_id and site
ghg_ppm_means <- ghg_timespans_corrected %>% 
  group_by(kit_id, transect_location) %>% 
  summarize(pco2_ppm = mean_(corrected_p_co2_ppm),
            pch4_ppm = mean_(corrected_p_ch4_ppm),
            pn2o_ppm = mean_(corrected_p_n2o_ppm),
            timespan_hr = mean_(timespan)) %>% 
  ungroup()


# 5. Calculate concentrations from partial pressures ---------------------------

## Set constants: lab temp, and average salinity
temp_c = 18 # based on personal experience with MCRL...
temp_k = temp_c + 273.15
sequim_salinity_psu = 30.5 # based on average Sequim Bay salinity during the study period

## Values sourced from Nick's spreadsheet
gas_constants <- tibble(constant = c("A1", "A2",	"A3",	"B1",	"B2",	"B3"), 
                        ch4 = c(-68.8862,	101.4956,	28.7314, -0.076146, 0.04397, -0.006872),
                        co2 = c(-58.0931,	90.5069,	22.294,	0.027766,	-0.025888, 0.0050578), 
                        n2o = c(-62.7062,	97.3066,	24.1406,	-0.05842,	0.033193,	-0.0051313))

## Function to convert gas from ppm to uM
ppm_to_umolL <- function(raw_gas, salinity, gas){
  
  constants <- gas_constants %>% select(constant, gas)
  
  a1 = constants %>% filter(constant == "A1") %>% pull(gas)
  a2 = constants %>% filter(constant == "A2") %>% pull(gas)
  a3 = constants %>% filter(constant == "A3") %>% pull(gas)
  b1 = constants %>% filter(constant == "B1") %>% pull(gas)
  b2 = constants %>% filter(constant == "B2") %>% pull(gas)
  b3 = constants %>% filter(constant == "B3") %>% pull(gas)
  
  exp(a1 + a2 * (100/temp_k) + a3 * log(temp_k/100) +
        {{salinity}} *
        (b1 + b2 * (temp_k/100) + b3 * (temp_k/100)^2)) * {{raw_gas}}
}

## Of course, CH4 has to be different, and requires its own function....
convert_ch4 <- function(raw_gas, salinity){
  
  gas = "ch4"
  constants <- gas_constants %>% select(constant, gas)
  
  a1 = constants %>% filter(constant == "A1") %>% pull(gas)
  a2 = constants %>% filter(constant == "A2") %>% pull(gas)
  a3 = constants %>% filter(constant == "A3") %>% pull(gas)
  b1 = constants %>% filter(constant == "B1") %>% pull(gas)
  b2 = constants %>% filter(constant == "B2") %>% pull(gas)
  b3 = constants %>% filter(constant == "B3") %>% pull(gas)
  
  solubility <- exp(a1 + a2 * (100/temp_k) + a3 * log(temp_k/100) +
                      {{salinity}} *
                      (b1 + b2 * (temp_k/100) + b3 * (temp_k/100)^2))
  
  nM_raw <-(({{raw_gas}}/1000000)/(0.082057*temp_k))*1000000000
  
  nM_raw * solubility
}

ghg_concentrations <- ghg_ppm_means %>% 
  mutate(co2_uM = ppm_to_umolL(pco2_ppm, sequim_salinity_psu, "co2"), 
         ch4_nM = convert_ch4(pch4_ppm, sequim_salinity_psu),
         n2o_uM = ppm_to_umolL(pn2o_ppm, sequim_salinity_psu, "n2o")) 


# 6. Calculate rates -----------------------------------------------------------

## First, we set starting concentrations, based on atmospheric conditions since
## we equilibrated seawater with the atmosphere prior to adding to incubation vials
## Values used are CO2 = 420 ppm, CH4 = 1.9 ppm, N2O = 0.3 ppm
atm_co2 = ppm_to_umolL(420, sequim_salinity_psu, "co2")
atm_ch4 = convert_ch4(1.9, sequim_salinity_psu)
atm_n2o = ppm_to_umolL(0.3, sequim_salinity_psu, "n2o")

## Now, calculate rates, assuming that at time = 0, concentrations were atmospheric
ghg_rates_all <- ghg_concentrations %>% 
  mutate(co2_uM_hr = (co2_uM - atm_co2) / timespan_hr, 
         ch4_uM_hr = (ch4_nM - atm_ch4) / timespan_hr, 
         n2o_uM_hr = (n2o_uM - atm_n2o) / timespan_hr)

## The last step is separating Saltwater (i.e. blanks) from incubations
ghg_rates_salwater <- ghg_rates_all %>% 
  filter(transect_location == "Saltwater")

ghg_rates_incubations <- ghg_rates_all %>% 
  filter(transect_location != "Saltwater") %>% 
  drop_na()  # drops K041 Upland which wasn't run
  
write_csv(ghg_rates_incubations, "data/230623_ghg_final_dataset.csv")
write_csv(ghg_rates_salwater, "data/230623_ghg_saltwater.csv")





