plant_file_new='~/../Downloads/Indonesia Captive - filter for CELIOS (13).xlsx'
plant_file_old='~/../Downloads/Indonesia Captive - filter for CELIOS (12).xlsx'

read_xlsx(plant_file_new, sheet='Metal smelters', .name_repair = make.names) %>% 
  rename(capacity_input_tpa=contains('Capacity.Input'),
         capacity_output_tpa__=contains('Capacity.Output'),
         Commodity.detailed__=contains('Commodity.detailed'),
         lat_smelter=Latitude, lon_smelter=Longitude) %>% 
  mutate(across(matches('^(capacity|lat|lon)'), as.numeric)) -> smelters_new

read_xlsx(plant_file_old, sheet='Metal smelters', .name_repair = make.names) %>% 
  rename(capacity_input_tpa=contains('Capacity.Input'),
         capacity_output_tpa__=contains('Capacity.Output'),
         Commodity.detailed__=contains('Commodity.detailed'),
         lat_smelter=Latitude, lon_smelter=Longitude) %>% 
  mutate(across(matches('^(capacity|lat|lon)'), as.numeric)) -> smelters_old


emis_old <- file.path(output_dir, 'emissions', 'emissions_with_cluster v2.csv') %>% read_csv()
emis_new <- file.path(output_dir, 'emissions', 'emissions_with_cluster v3.csv') %>% read_csv()

emis_old %>% group_by(pollutant) %>% summarise(across(emissions_tpa, ~sum(.x, na.rm=T)))
emis_new %>% group_by(pollutant, type) %>% summarise(across(emissions_tpa, ~sum(.x, na.rm=T)))



bind_rows(smelters_new %>% mutate(version='new'),
          smelters_old %>% mutate(version='old')) %>% 
  filter(Province %in% emis_new$Province) %>% 
  group_by(Commodity, version) %>% 
  summarise(across(c(capacity_output_tpa__1, Capacity..MW.), ~sum(.x, na.rm=T)))