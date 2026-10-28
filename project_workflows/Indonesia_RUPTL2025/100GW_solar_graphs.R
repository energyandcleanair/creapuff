project_dir='G:/Shared drives/CREA-HIA/Projects/2026_Indonesia_100GW_solar_HIA'
emissions_dir=file.path(project_dir, 'emissions')
output_dir=file.path(project_dir, 'HIA')

emis_long <- readRDS(file.path(emissions_dir, 'indonesia_RUPTL_emission_pathways_October_2026.RDS'))

#avoided emissions from 100GW solar
emis_long %>% 
  #filter(COD<=year, year_retire>year) %>% 
  group_by(pollutant, year, scenario) %>% 
  summarise(across(emissions_t, sum)) %>% 
  mutate(delta=ifelse(year<2026, 0, emissions_t[scenario=='RUPTL 2025'] - emissions_t)) %>% 
  filter(scenario=='100GW solar', delta>0) %>% 
  ggplot(aes(year, delta)) + geom_col() + facet_wrap(~pollutant, scales='free_y') +
  labs() +
  theme_crea_new()


emis_long %>% 
  filter(COD<=year, year_retire>year, year>2025) %>% 
  group_by(pollutant, scenario) %>% 
  summarise(across(emissions_t, sum)) %>% 
  #mutate(delta=emissions_t[scenario=='RUPTL 2025'] - emissions_t) %>% 
  #filter(scenario=='100GW solar', delta>0) %>% 
  ggplot(aes(scenario, emissions_t)) + 
  geom_col() + 
  facet_wrap(~pollutant, scales='free_x') +
  coord_flip() +
  labs() +
  theme_crea_new()


hia_totals <- read_csv(file.path(output_dir, 'annual HIA.csv'))

hia_totals %>% 
  filter(outcome=='deaths, total') %>% 
  group_by(year, scenario) %>% 
  summarise(across(c(central, low, high), sum)) %>% 
  mutate(across(c(central, low, high), list(delta=~ifelse(year<2026, 0, .x[scenario=='RUPTL 2025'] - .x)))) %>% 
  filter(scenario=='100GW solar', central_delta>0) %>% 
  ggplot(aes(year, central_delta)) + 
  geom_col() +
  geom_errorbar(aes(ymin=low_delta, ymax=high_delta), width=.25) +
  labs() +
  theme_crea_new()
