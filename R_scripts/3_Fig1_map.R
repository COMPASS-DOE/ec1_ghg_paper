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
       ggsflabel,
       ggthemes)

## Set CRS
common_crs <- 4326

# 2. Load datasets -------------------------------------------------------------

## Bring in master

df <- read_csv("data/230623_master_data.csv") %>% 
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

## Convert data into sf object
df_sf <- df %>% 
  st_as_sf(., coords = c("long", "lat"), crs = common_crs)

## Set bounding boxes
bbox <- st_bbox(cb_states)
bbox_inset <- c(xmin = -76.6, 
                   ymin = 38.8, 
                   xmax = -76.5, 
                   ymax = 38.95)

asterisk <- tibble(long = -76.57, lat = 39) %>% 
  st_as_sf(coords = c("long", "lat"), crs = common_crs)

ggplot() + 
  geom_rect(aes(xmin = bbox[[1]], 
                ymin = bbox[[2]], 
                xmax = bbox[[3]], 
                ymax = bbox[[4]]), 
            color = NA, fill = "lightblue") + 
  geom_sf(data = cb_states) + 
  geom_sf(data = df_sf, size = 5, color = "white") + 
  geom_sf(data = df_sf, size = 3.8, aes(color = Salinity), alpha = 0.8) + 
  geom_sf(data = asterisk, shape = "*", size = 8, color = "red") +
  scale_color_viridis_c() + 
  # geom_rect(aes(xmin = bbox_inset[[1]], 
  #               ymin = bbox_inset[[2]], 
  #               xmax = bbox_inset[[3]], 
  #               ymax = bbox_inset[[4]]), 
  #           color = "black", fill = NA) + 
  theme_map() + 
  theme(legend.position = c(0.8, 0.1), 
        legend.background = element_blank())
ggsave("figures/1A_map.pdf", width = 4, height = 5)


ggplot() +
  geom_rect(aes(xmin = bbox_inset[[1]],
                ymin = bbox_inset[[2]],
                xmax = bbox_inset[[3]],
                ymax = bbox_inset[[4]]),
            color = NA, fill = "lightblue") +
  geom_sf(data = st_crop(cb_states, bbox_inset)) +
  geom_sf(data = st_crop(df_sf, bbox_inset), size = 5, color = "white") +
  geom_sf(data = st_crop(df_sf, bbox_inset), size = 3.8,
          aes(color = Salinity), alpha = 0.8, show.legend = F) +
  scale_color_viridis_c(limits = c(min(df_sf$Salinity), max(df_sf$Salinity))) +
  theme_bw()
ggsave("figures/231030_overlapping_points_from_Fig1A.pdf", width = 3, height = 4)




