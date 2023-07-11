## This script converts GHG concentrations in ppm into umol/nmol, so that we can
## directly compare water and air concentrations. Using code adapted from ASV
## LDRD project.
##
## 2022-11-03 (Updated 2023-04-17 for eSS-DIVE prep)
## Peter Regier
## 
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse)

## set ggplot theme
theme_set(theme_bw())

## Set constants: lab temp, and average salinity
temp_c = 18 # based on personal experience with MCRL...
temp_k = temp_c + 273.15
sequim_salinity_psu = 30.5 # based on average Sequim Bay salinity during the study period


# 2. Read in data --------------------------------------------------------------

## Load GHG data
ghg_raw <- read_csv("data/ghg_raw.csv") %>% 
  mutate(sal_psu = ifelse(type == "wet", sequim_salinity_psu, 0))


# 3. Set up constants for conversion from ppm to umol --------------------------

## Values sourced from Nick's spreadsheet
gas_constants <- tibble(constant = c("A1", "A2",	"A3",	"B1",	"B2",	"B3"), 
                        ch4 = c(-68.8862,	101.4956,	28.7314, -0.076146, 0.04397, -0.006872),
                        co2 = c(-58.0931,	90.5069,	22.294,	0.027766,	-0.025888, 0.0050578), 
                        n2o = c(-62.7062,	97.3066,	24.1406,	-0.05842,	0.033193,	-0.0051313))

## Constants for calculating Schmidt's constant (Table 1, 10.4319/lom.2014.12.351)
## Actually don't need this, but leaving in for posterity
# sc_constants = tibble(constant = c("A", "B",	"C",	"D",	"E"), 
#                       co2 = c(2116.8, -136.25, 4.7353, -0.092307, 0.0007555), 
#                       ch4 = c(2101.2, -131.54, 4.4931, -0.08676, 0.00070663), 
#                       n2o = c(2356.2, -166.38, 6.3952, -0.13422, 0.0011506))


# 4. Actually correct values following Henry's Law -----------------------------

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

ghg_cor <- ghg_raw %>% 
  mutate(pco2_uM = ppm_to_umolL(pco2_ppm_c, sal_psu, "co2"), 
         pch4_nM = convert_ch4(pch4_ppm_c, sal_psu),
         pn2o_uM = ppm_to_umolL(pn2o_ppm_c, sal_psu, "n2o")) %>% 
  rename("transect_location" = site) %>% 
  select(kit_id, transect_location, type, timespan_h, pco2_uM, pch4_nM, pn2o_uM) %>% 
  # (umol or nmol /L)*(mol/umol or nmol)*(g/mol)*(mg/g)*(1/hr). Note molar mass of CO2 = N2O - not used anymore
  mutate(pco2_uM_hr = pco2_uM * (1/timespan_h), 
         pch4_uM_hr = pch4_nM * (1 / 1000) * (1/timespan_h), #convert nM to uM
         pn2o_uM_hr = pn2o_uM * (1/timespan_h))


# 5. Check columns for outliers ------------------------------------------------

## Removing outliers that 1) fail grubbs.test() and 2) are more than 2x the nearest value
## Note: for pn2o_uM_wet, there are values at 14.1 and 13.8 (nearest neighbor: 2.4)
## Not removing for the time being cause it might be real, but that's sketch?

## Code for checking outliers
### Run Grubb's test
# grubbs.test(master_raw$pn2o_uM_wet)
# ### Plot to visualize
# ggplot(master_raw, aes(pn2o_uM_wet)) + geom_boxplot()
# ### Pull offending rows
# master_raw %>% filter(pn2o_uM_wet > 2)

## Currently (6/22/23) leaving these two outliers in
ghg_final <- ghg_cor #%>% 
  #filter(pco2_uM < 4000) %>% # remove 8641 uM (K015 UP) - next is 3627
  #filter(pch4_nM < 100) # remove 25.7 (K050 W) - next is 7.83


# 6. Write out final dataset and make check graphs -----------------------------

write_csv(ghg_final, "data/230417_ghg_final.csv")
