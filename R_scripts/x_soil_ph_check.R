

data_path <- "https://drive.google.com/drive/folders/1r3sP7tvhG2btZACvxZUdtrtSiLVGUUGp"

data_filenames_all <- drive_ls(data_path) %>% pull(name)

filename <- data_filenames_all[grepl("Soil_pH", data_filenames_all)]

drive_download(filename, overwrite = T)

kits <- unique(read_csv("data/ghg.csv") %>% 
  pull(kit_id))

soil_ph <- read_csv(filename) %>% 
  filter(kit_id %in% kits)

ggplot(soil_ph, aes(kit_id, transect_location, fill = ph)) + 
  geom_tile()