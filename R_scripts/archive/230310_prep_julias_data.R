## Bring in and homogenize Julia's spatial land cover data
## pjr 2023-03-10

require(pacman)
p_load(tidyverse, 
       sf)

kits <- unique(read_csv("data/ghg.csv") %>% select(kit_id) %>% pull())

#ccap <- read_sf("data/julia/all_sample_sites_ccap.shp")
ccap <- read_csv('data/julia/CCAP_land_cover.csv') %>% 
  rename("ccap_landcover" = ccap_landcover_value, 
         "ccap_class" = point_land_cover_class) %>% 
  select(kit_id, transect_location, ccap_landcover, ccap_class) %>% 
  filter(kit_id %in% kits)

evt <- read_csv("data/julia/LF_EVT_ecosystem_classes.csv") %>% 
  #select(kit_id, transect_location) %>% 
  filter(kit_id %in% kits)

## To decide which variables to use, look at unique values and geom_tile()
colnames(evt)
unique(evt$point_ecological_system_name)


ggplot(ccap %>% filter(transect_location != "Water"), 
       aes(kit_id, transect_location, fill = ccap_landcover)) + 
  geom_tile(show.legend = F)

df_raw <- inner_join(ccap, evt, by = c("kit_id", "transect_location"))

write_csv(df_raw, "data/230315_lulc.csv")






