require(raster)
require(tidyverse)
require(magrittr)
require(lubridate)
require(readxl)
require(creahelpers)
require(sf)

source('project_workflows/read_IESR_emissions.R')

emissions_dir <- 'G:/Shared drives/CREA-HIA/Projects/Indonesia_JETP'
readRDS(file.path(emissions_dir, 'indonesia_iesr_emission_pathways v2.RDS')) -> emis

c('emissions inputs, with clustering.csv', 'emissions inputs, with clustering, additions.csv') %>% 
  file.path(emissions_dir, .) %>%
  lapply(read_csv) %>% bind_rows() -> stackdata


loc <- tibble(lon=106.8456, lat=-6.2088) %>% to_spdf() %>% st_as_sf()

emis %>% filter(year==2022, scenario=='BAU', Status=='operating', pollutant %in% c('SOx', 'NOx', 'PM')) ->
  current_emis

require(sf)
current_emis %>% cluster(5) -> current_emis$cluster

stack_regex <- 'Height|Diameter|Temp|Velocity'
stackdata %>% select(CFPP.name, matches(stack_regex)) %>%
  right_join(current_emis) -> current_emis

pm25_potential = tibble(pollutant = c('SOx', 'NOx', 'PM'),
                        pm25_potential = c(132/64, 80/46, 24/80))

current_emis %>% 
  left_join(plant_names) %>%
  group_by(cluster) %>%
  left_join(pm25_potential) %>%
  summarise(emissions_t = sum(emissions_t*pm25_potential),
            across(c(Latitude, Longitude, matches(stack_regex)), ~mean(.x, na.rm=T)),
            plants=paste(unique(plant_name), collapse='; '),
            plant_units=paste(CFPP.name, collapse='; ')) ->
  emis_bycluster

#https://web.iitd.ac.in/~arunku/files/CEL795_Y13/AirPollutionExamples2.pdf
#Holland's formula
plume_rise <- function(flue_velocity_ms,
                                     stack_diameter_m,
                                     wind_speed_ms,
                                     pressure_kPa=101,
                                     flue_temperature_K,
                                     air_temperature_K) {
  (flue_velocity_ms/wind_speed_ms) *
    (1.5 + (2.68e-2*pressure_kPa) * 
       (flue_temperature_K / air_temperature_K) / 
       air_temperature_K * 
       stack_diameter_m)
}

emis_bycluster %>% to_spdf() %>% st_as_sf() %>% st_distance(loc) -> emis_bycluster$distance_to_loc

emis_bycluster %<>% 
  rename(stack_height=Height..m.) %>% 
  mutate(plume_rise = plume_rise(flue_velocity_ms=Velocity..m.s.,
                                 stack_diameter_m=Diameter..m.,
                                 wind_speed_ms=4,
                                 flue_temperature_K=Exit.Temp....+273.15,
                                 air_temperature_K=27),
         release_height_low = stack_height + plume_rise/2,
         release_height_high = stack_height + plume_rise*2)

emis_bycluster %>% filter(distance_to_loc<units::set_units(300, 'km'),
                          distance_to_loc<units::set_units(50, 'km') | emissions_t > 2000,
                          distance_to_loc<units::set_units(100, 'km') | emissions_t>10000) ->
  selected_clusters


require(ggspatial)
ggplot(selected_clusters) + 
  geom_point(aes(Longitude, Latitude, size=emissions_t)) +
  layer_spatial(loc, color='red')

selected_clusters %>% 
  mutate(plants=ifelse(grepl('Suralaya', plants), 'Suralaya', plants),
         across(distance_to_loc, as.vector)) %>%
  select(plants, plant_units, distance_to_loc, Latitude, Longitude, 
         stack_height, plume_rise, 
         release_height_low, release_height_high, emissions_t) %>%
  write_csv(file.path(emissions_dir, 'plants_around_Jakarta_for_HYSPLIT.csv'))
