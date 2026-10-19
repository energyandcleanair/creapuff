emissions_dir <- 'G:/IndonesiaIESR/emissions'
emission_file = 'indonesia_iesr_emission_pathways v2.RDS'

output_dir <- file.path(emissions_dir, 'to Trivia')

read_csv(file.path(emissions_dir, 'emissions, clustered.csv')) -> emissions_data
read_csv(file.path(emissions_dir, 'emissions inputs, with clustering, missing sources.csv')) -> missed_sources



stack_cols <- names(emissions_data) %>% grep('Height|Diameter|Temp|Velocity', ., value=T)
missed_sources %>%
  group_by(emission_names, cluster) %>%
  summarise(across(c(Longitude, Latitude, all_of(stack_cols)), mean, na.rm=T),
            across(c(MW, ends_with('pa')), sum, na.rm=T)) %>%
  bind_rows(emissions_data, .) -> emissions_data

emissions_data %>% write_csv(file.path(emissions_dir, 'emissions, clustered, with missing sources.csv'))

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


emis %>% distinct(CFPP.name, GEM.ID, cluster, Latitude, Longitude, MW) %>% write_csv(file.path(output_dir, "CFPP_clustering.csv"))
modeled_emissions %>% write_csv(file.path(output_dir, "modeled_emissions_by_cluster.csv"))

emis %>% group_by(scenario, CFPP.name) %>% filter(year==pmax(2022, COD)) %>% write_csv(file.path(output_dir, 'emissions, COD and retirement for all units and scenarios.csv'))



