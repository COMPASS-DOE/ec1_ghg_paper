## Correlation matrices
##
## 2022-10-07 (Updated 2023-02-09)
## Peter Regier
##
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, 
       cowplot, 
       corrplot, 
       googledrive,
       ggcorrplot, 
       ggfortify, 
       ggpubr)

## Character variable vector
chars <- c("kit_id", "transect_location")
common_cols <- c("delta_do_hr", "d_pco2", "d_pch4", "d_pn2o")

## Set theme
theme_set(theme_bw())


# 2. Bring in data -------------------------------------------------------------

## This is KEY: it avoids user input, and will automatically find the token. 
## This has, I think, timed out before. If so, just run drive_auth() and select
## the proper email.
options(gargle_oauth_email = "peter.regier@pnnl.gov")

## Set two paths for the two folders with data we want
data_path <- "https://drive.google.com/drive/folders/1r3sP7tvhG2btZACvxZUdtrtSiLVGUUGp"

## Next, create two lists of file names we'll use to download files
data_filenames_all <- drive_ls(data_path) %>% pull(name)

## Need to trim to only CSVs because GD likes to create duplicate files with the
## same name in Sheets format
data_filenames0 <- data_filenames_all[grepl(".csv", data_filenames_all)]

data_filenames <- data_filenames0[grepl("GWC|LOI", data_filenames0)]

lapply(data_filenames, drive_download, overwrite = T)

gwc <- read_csv(data_filenames[grepl("GWC", data_filenames)])
loi <- read_csv(data_filenames[grepl("LOI", data_filenames)])

kits <- c("K034", "K035", "K036")

make_plot <- function(data, var, y_label){
  x <- data %>% 
    mutate(transect_location = fct_relevel(transect_location, c("Sediment", "Wetland", "Transition", "Upland")))
  
  ggplot(x, aes(transect_location, {{var}}, label = kit_id)) + 
    geom_boxplot(outlier.alpha = 0, fill = "gray90", color = "gray70") + 
    geom_jitter(width = 0.1, alpha = 0.5) + 
    geom_point(data = x %>% filter(kit_id %in% kits), color = "black", size = 3) +
    geom_point(data = x %>% filter(kit_id %in% kits), color = "red", size = 2.5) +
    geom_text(data = x %>% filter(kit_id %in% kits), hjust = -0.2, vjust = 0.2, size = 2.5) +
    labs(x = "", y = y_label)
}

plot_grid(make_plot(gwc, gwc_perc, "Gravimetric water content (%)"), 
          make_plot(loi, loi_perc, "Loss on ignition (%)"),
           nrow = 1)

ggsave("figures/230308_hp_gwc_loi.png", width = 7, height = 4)


