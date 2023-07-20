## Try and pull some spatial datasets in to calculate landscape-scale metrics
## for Chesapeake Bay

# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(tidyverse, 
       sf, 
       janitor,
       nasapower, 
       soilDB) #https://rspatialdata.github.io/rainfall.html

## read in lat-longs
df_raw <- read_csv("data/master_data.csv") %>% 
  filter(region != "Great Lakes") %>% 
  add_row(kit_id = "K030", latitude = 39.0884, longitude = -75.4374) %>% 
  add_row(kit_id = "K037", latitude = 37.5569, longitude = -76.9731) %>% 
  add_row(kit_id = "K041", latitude = 37.455876, longitude = -75.833467) %>% 
  group_by(kit_id) %>% 
  summarize(Salinity = mean(sal_psu, na.rm = T), 
            lat = mean(latitude, na.rm = T), 
            long = mean(longitude, na.rm = T))

# 2. Climate data --------------------------------------------------------------

get_climate_data <- function(kit_id, lat, long){
  
  message(paste("working on", kit_id))
  power_raw <- get_power(community = "ag",
                         pars = c("T2M", "PRECTOTCORR"),
                         lonlat = c(long, lat),
                         temporal_api = "climatology")
  power_raw %>% 
    clean_names() %>% 
    select(lat, lon, parameter, ann) %>% 
    pivot_wider(names_from = "parameter", values_from = "ann") %>% 
    rename("mat_c" = T2M, 
           "monthly_precip" = PRECTOTCORR) %>% 
    mutate(kit_id)
}

climate_df <- df_raw %>% 
  select(-Salinity) %>% 
  pmap(get_climate_data) %>% 
  bind_rows()# %>% 
  #rename("kit_id" = kit)

write_csv(climate_df, "data/230223_climate_data.csv")


# 3. Soil data -----------------------------------------------------------------

# K018, K050, K052, K059 and K062 all returned NaNs for soil type, so manually
## moving each to a nearby location with data
df_raw %>% filter(kit_id == "K062") %>% 
  as.data.frame()

df_edited_coordinates <- df_raw %>% 
  filter(!(kit_id %in% c("K018", "K050", "K052", "K059", "K062"))) %>% 
  add_row(kit_id = "K018", lat = 37.220229, long = -76.411529) %>% 
  add_row(kit_id = "K050", lat = 37.335316, long = -77.204869) %>% 
  add_row(kit_id = "K052", lat = 38.159709, long = -75.792628) %>% 
  add_row(kit_id = "K059", lat = 38.430009, long = -76.235256) %>% 
  add_row(kit_id = "K062", lat = 38.874523, long = -76.551718)

x <- df_edited_coordinates %>% 
  select(-Salinity)

y <- fetchSoilGrids(
    x,
    loc.names = c("kit_id", "lat", "long"),
    verbose = T,
    progress = T
  )

# make a tibble of the relevant results  
soil_properties <- tibble(depth = y$label, 
       kit_id = y$id,
       bd = y$bdodmean,
       clay = y$claymean,
       sand = y$sandmean,
       silt = y$siltmean) %>% 
  group_by(kit_id) %>% 
  summarize(across(where(is.numeric), mean, na.rm = T))

write_csv(soil_properties, "data/230223_soil_data.csv")


