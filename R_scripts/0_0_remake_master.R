## This is a convenience script used to remake the master dataset that calls
## scripts in order
##
## 2023-06-22
## Peter Regier
## 
# ########### #
# ########### #

library(tictoc)

tic("remake master data") # Takes about 43s
source("R_scripts/0_prep_ghg_dataset.R")
source("R_scripts/1_prep_o2_data.R")
source("R_scripts/2_create_master_dataset.R")
toc()

