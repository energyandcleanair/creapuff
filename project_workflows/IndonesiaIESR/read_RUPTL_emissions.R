if(!exists('emissions_dir')) emissions_dir <- 'G:/Shared drives/CREA-HIA/Projects/Indonesia_JETP/emissions'
if(!exists('emission_file')) emission_file = 'indonesia_RUPTL_emission_pathways.RDS'

read_csv(file.path(emissions_dir, 'emissions, clustered.csv')) -> emissions_data
read_csv(file.path(emissions_dir, 'emissions inputs, with clustering, missing sources.csv')) -> missed_sources



stack_cols <- names(emissions_data) %>% grep('Height|Diameter|Temp|Velocity', ., value=T)
missed_sources %>%
  group_by(emission_names, cluster) %>%
  summarise(across(c(Longitude, Latitude, all_of(stack_cols)), mean, na.rm=T),
            across(c(MW, ends_with('pa')), sum, na.rm=T)) %>%
  bind_rows(emissions_data, .) -> emissions_data

bind_rows(read_csv(file.path(emissions_dir, 'emissions inputs, with clustering.csv')),
          missed_sources) %>%
  distinct(emission_names, CFPP.name) -> emissions_clustering

emissions_data %>% select(emission_names, ends_with('pa')) %>%
  pivot_longer(-emission_names, names_to='emitted_species', values_to='modeled_emissions') %>%
  mutate(cluster=tolower(emission_names),
         emitted_species=gsub('_.*', '', emitted_species)) ->
  modeled_emissions

emis <- readRDS(file.path(emissions_dir, emission_file)) %>%
  left_join(emissions_clustering) %>% mutate(cluster=tolower(emission_names)) %>%
  group_by(Latitude, Longitude) %>% fill(cluster, .direction = 'downup')


#add province based on coordinates
adm1 <- readRDS(file.path(gis_dir, 'boundaries', 'gadm36_1_low.RDS')) %>% st_as_sf()

#creapuff.env <- list()
#creapuff.env$llproj <- '+proj=longlat +datum=WGS84 +no_defs'
sf_use_s2(F)

emis %<>% ungroup %>% 
  distinct(Latitude, Longitude) %>% 
  to_spdf %>% st_as_sf() %>% 
  mutate(province=adm1$NAME_1[st_nearest_feature(., adm1)]) %>% 
  st_drop_geometry() %>% 
  left_join(emis %>% select(-province))

plants_to_combine <- c("Cilacap", "Suralaya", "Jeneponto", "Sulsel Barru", "Asam-Asam", "Amurang")

emis %>% distinct(CFPP.name, Latitude, Longitude, COD) %>% 
  mutate(plant_name = CFPP.name %>% gsub(' Unit.*| Phase.*| Expansion', '', .), 
         unit_name = CFPP.name %>% gsub(' NA| Andalas.*', '', .)) %>% 
  group_by(Latitude, Longitude) %>% 
  mutate(plant_name = plant_name %>% (function(x) {
    for(plantname in plants_to_combine) if(any(grepl(plantname, x, ignore.case=T))) x[] <- plantname
    return(x)
  }),
  plant_name = ifelse(grepl('Parit Baru', CFPP.name) & COD>2020, 'Parit Baru Expansion', plant_name)) %>% 
  ungroup %>% select(contains('name')) ->
  plant_names

plant_names %>% write_csv(file.path(emissions_dir, 'plant_names.csv'))
