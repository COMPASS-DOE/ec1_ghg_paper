## This script creates the map figure
##
## 2022-06-09
## Peter Regier
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load package
require(pacman)
p_load(tidyverse, sf, 
       googledrive, 
       ggthemes)

## Set CRS
common_crs <- 4326

# 2. Load datasets -------------------------------------------------------------

## Bring in 
master_raw <- read_csv("data/master_data.csv") %>% 
  filter(region != "Great Lakes") %>% 
  add_row(kit_id = "K030", latitude = 39.0884, longitude = -75.4374) %>% 
  add_row(kit_id = "K037", latitude = 37.5569, longitude = -76.9731) %>% 
  add_row(kit_id = "K041", latitude = 37.455876, longitude = -75.833467) %>% 
  group_by(kit_id) %>% 
  summarize(Salinity = mean(sal_psu, na.rm = T), 
            lat = mean(latitude, na.rm = T), 
            long = mean(longitude, na.rm = T))


# 3. Set up map layers for plotting --------------------------------------------

## Set regional and CB (inset) bounding boxes
cb_bbox <- c(xmin = -77.8, xmax = -74.5, ymin = 36.8, ymax = 40.2)

## Make US states map cropped to GL/CB region
cb_states <- read_sf("/Users/regi350/OneDrive - PNNL/Documents/GitHub/COMPASS-DOE/EXCHANGE/Data/gis/cb_2018_us_state_5m/cb_2018_us_state_5m.shp") %>% 
  st_transform(., crs = common_crs) %>% 
  st_crop(., y = cb_bbox)

## Convert master into sf object
master <- master_raw %>% 
  st_as_sf(., coords = c("long", "lat"), crs = common_crs)


ggplot() + 
  geom_sf(data = cb_states) + 
  geom_sf(data = master, size = 4, color = "white") + 
  geom_sf(data = master, size = 3, aes(color = Salinity), alpha = 0.8) + 
  scale_color_viridis_c() + 
  theme_map() + 
  theme(legend.position = c(0.8, 0.1), 
        legend.background = element_blank())
ggsave("figures/A_map.pdf", width = 3, height = 4)



