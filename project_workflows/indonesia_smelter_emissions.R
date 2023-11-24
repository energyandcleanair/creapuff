require(raster)
require(tidyverse)
require(magrittr)
require(lubridate)
require(readxl)
require(creapuff)
require(creahelpers)
require(rcrea)

#Pollutant inputs - nickel smelting - EIA reporting data
#https://docs.google.com/spreadsheets/d/1corCzD9ThiryQ6rDmU-h7pCdb5nY9R60Ogd00hwg7DY/edit#gid=0
emis_file='~/../Downloads/Pollutant inputs - nickel smelting - EIA reporting data (3).xlsx'

#Indonesia Captive - filter for CELIOS
#https://docs.google.com/spreadsheets/d/1LP_DmK353mFoKP19dtSAHkv_MkKqvwThDLQoxipSWT0/edit#gid=602516471
plant_file='~/../Downloads/Indonesia Captive - filter for CELIOS (9).xlsx'

output_dir='G:/Shared drives/CREA-HIA/Projects/Indonesia-Nickel'

read_wide_xlsx(emis_file,
               header_row_names = c('measurement_unit', 'pollutant'),
               info_columns = c(1:6,16,18,30:40),
               fill_header_rows=F) %>% 
  rename(O2_reference=contains('O2_adjustment'), O2_measured=contains('O2_sample'), 
         Company=contains('Company'),
         release_point=contains('Equipment'),
         industrial_park=contains('Industrial Park'),
         m3_per_min=contains('Flowrate'),
         fgc=value, 
         lat_release_point=Latitude, lon_release_point=Longitude) %>% 
  mutate(across(matches('^(lat|lon|O2_|m3_)'), as.numeric),
         pollutant=recode(pollutant, NO2='NOx')) -> emis

read_xlsx(emis_file, sheet='EF', .name_repair = make.names) %>% 
  filter(!grepl('secondary', Activity)) %>% 
  mutate(emissions_t_per_t = Value * case_when(grepl('kg/Mg', Unit)~1e-3,
                                               grepl('^g/Mg', Unit)~1e-6,
                                               grepl('µg.*/Mg', Unit)~1e-12),
         Commodity = Activity %>% gsub(' production.*', '', .)) %>% 
  rename(pollutant=Pollutant) %>% 
  mutate(pollutant=recode(pollutant, SOx='SO2', TSP='PM'),
         Commodity=recode(Commodity,Aluminium='Aluminum')) %>% 
  group_by(Commodity, pollutant) %>% 
  filter(grepl('primary', Activity) | !any(grepl('primary', Activity))) %>% ungroup -> ef

read_xlsx(plant_file, sheet='Metal smelters', .name_repair = make.names) %>% 
  rename(capacity_input_tpa=contains('Capacity.Input'),
         capacity_output_tpa__=contains('Capacity.Output'),
         Commodity.detailed__=contains('Commodity.detailed'),
         lat_smelter=Latitude, lon_smelter=Longitude) %>% 
  mutate(across(matches('^(capacity|lat|lon)'), as.numeric)) -> smelters

#geocode missing coordinates
readLines('~/google_api_key.txt') %>% register_google()

smelters %>% filter(is.na(lat_smelter+lon_smelter)) %>% mutate(query=paste(Location, Province, 'Indonesia', sep=', ')) -> misloc
queries = misloc$query %>% unique

if(length(queries)>0) {
  locs <- NULL
  locs_cached <- NULL
  if(file.exists('cache/geocoded_locations.csv')) {
    locs_cached <- read_csv('cache/geocoded_locations.csv')
    queries %<>% subset(. %notin% locs_cached$query)
  }
  
  if(length(queries)>0) geocode(queries) %>% mutate(query=queries) -> locs
  locs <- bind_rows(locs, locs_cached)
  dir.create('cache')
  locs %>% write_csv('cache/geocoded_locations.csv')
  
  misloc %>% select(-lat_smelter, -lon_smelter) %>% 
    left_join(locs %>% rename(lat_smelter=lat, lon_smelter=lon)) %>% 
    select(-query) %>% 
    bind_rows(smelters %>% filter(!is.na(lat_smelter+lon_smelter))) ->
    smelters
}

for(electricity_source in c('coal', 'PLN')) {
  colname <- paste0(electricity_source, '_MW')
  ix <- grepl(electricity_source, smelters$PLN.or.Captive.CFPP, ignore.case = T)
  smelters[[colname]] <- as.numeric(NA)
  smelters[[colname]][ix] <- smelters$Capacity..MW.[ix] %>% as.numeric
}

sources <- smelters$Captive_non.coal %>% unique() %>% gsub('.*\\(|)', '', .) %>% na.omit

for(electricity_source in sources) {
  colname <- paste0(electricity_source, '_MW')
  ix <- grepl(electricity_source, smelters$Captive_non.coal, ignore.case = T)
  smelters[[colname]] <- as.numeric(NA)
  smelters[[colname]][ix] <- smelters$Capacity_2..MW.[ix] %>% as.numeric
}

sources %>% subset(.!='Diesel') %>% c('PLN', 'coal') %>% paste0('_MW') -> non_diesel
smelters$diesel_as_backup_only <- rowSums(smelters[,non_diesel], na.rm=T)>0 & !is.na(smelters$Diesel_MW)

emis$Province[emis$Company=="Gunbuster Nickel Industry"] <- "Central Sulawesi"
smelters$Commodity %<>% recode(Aluminium='Aluminum')

smelters %>% separate(Commodity, paste0('Commodity__', 1:3), sep=', ') %>% 
  pivot_longer(matches('Commodity_|capacity_output_|Commodity.detailed__'), names_to=c(".value", "Var"), names_sep='__') %>% 
  filter(!is.na(Commodity)) -> smelters_long



#### smelter emissions
#check that all companies in the emission data are found in the smelter list
emis %>% anti_join(smelters_long) %>% distinct(Company)

#check that there aren't companies with multiple commodities in emission data - the script isn't designed to handle this
smelters_long %>% group_by(Company) %>% filter(length(unique(Commodity))>1) %>% inner_join(emis %>% group_by(Company) %>% filter(!all(is.na(fgc)))) %>% distinct(Company)

emis %<>% full_join(smelters_long, .)

emis %>% group_by(Commodity) %>% summarise(has_data=!all(is.na(fgc)), number_of_plants=length(unique(Company))) -> commodities_with_data
smelters_long %>% group_by(Commodity) %>% summarise(across(contains('capacity_'), ~sum(.x, na.rm=T))) %>% 
  left_join(commodities_with_data) %>% na.omit %>% arrange(desc(capacity_output_tpa))



##calculate emissions volumes
emis %<>% mutate(O2_reference=na.cover(O2_reference, O2_measured),
                 Nm3_per_min=m3_per_min*(21-O2_measured/100)/(21-O2_reference/100),
                 emissions_tpa=fgc*Nm3_per_min*60*8760/1e9)

emis %>% filter(emissions_tpa>0) %>% group_by(Company, Commodity, pollutant) %>% 
  summarise(across(emissions_tpa, ~sum(.x, na.rm=T))) ->
  emis_by_company

#add PM speciation
ef %>% filter(grepl('^PM', pollutant)) %>% 
  complete(Commodity, pollutant) %>% 
  group_by(Commodity) %>% 
  mutate(share = Value/Value[pollutant=='PM']) %>% 
  group_by(pollutant) %>% 
  mutate(share = na.cover(share, mean(share, na.rm=T))) %>% 
  select(Commodity, PM_fraction=pollutant, share) %>% mutate(pollutant='PM') -> pm_fractions

pm_fractions %<>% filter(Commodity=='Nickel') %>% mutate(Commodity='NPI') %>% bind_rows(pm_fractions)

emis_by_company %<>% left_join(pm_fractions) %>% 
  mutate(across(emissions_tpa, ~.x*ifelse(pollutant=='PM', share, 1)), 
         pollutant=na.cover(PM_fraction, pollutant)) %>% 
  select(-PM_fraction, -share)

emis %>% filter(emissions_tpa>0) %>% group_by(Company) %>% summarise(across(c(lat_release_point, lon_release_point), ~weighted.mean(.x, Nm3_per_min)))

#calculate emissions per output
emis %>% filter(Nm3_per_min>0) %>% 
  mutate(Nm3_per_t=Nm3_per_min*60*8760/capacity_output_tpa) %>% 
  group_by(Company, 
           Commodity=ifelse(grepl('CFPP|Diesel Generator', release_point), 'electricity', Commodity), 
           pollutant, 
           emission_type=ifelse(release_point=='CFPP', 'CFPP', 'process')) %>% 
  summarise(across(Nm3_per_t, ~sum(.x[.x>0], na.rm=T)),
            across(fgc, ~weighted.mean(.x[.x>0], Nm3_per_min[.x>0], na.rm=T))) %>% #filter(emissions_kg_per_h>0) %>% 
  group_by(Commodity, pollutant) %>% summarise(across(c(fgc, Nm3_per_t), mean), n=n()) %>% 
  mutate(emissions_t_per_t=fgc*Nm3_per_t/1e9) -> ef_measured


###add emissions factors

ef %>% anti_join(ef_measured %>% select(Commodity, pollutant)) %>% bind_rows(ef_measured) -> ef_all

emis_by_company %<>% inner_join(smelters_long) %>% ungroup

smelters_long %>% 
  mutate(Commodity_EF = ifelse(Commodity %in% ef_all$Commodity, Commodity, "Other metal")) %>% 
  left_join(ef_all %>% select(Commodity_EF=Commodity, pollutant, emissions_t_per_t)) %>% 
  mutate(emissions_tpa=emissions_t_per_t*capacity_output_tpa) %>% 
  anti_join(emis_by_company %>% ungroup %>% select(Company, pollutant)) %>% 
  bind_rows(emis_by_company) -> emis_smelter

emis_by_company %>% group_by(pollutant) %>% summarise(across(emissions_tpa, ~sum(.x, na.rm=T)))
emis_smelter %>% group_by(pollutant) %>% summarise(across(emissions_tpa, ~sum(.x, na.rm=T)))

require(ggspatial); require(ggmap)
emis_smelter %>% filter(pollutant %in% c('SO2', 'NOx', 'PM'), emissions_tpa>0) %>% to_sf_points() -> emis_smelter_sf
gis_dir='~/GIS/'
adm<-get_adm(0, 'coarse')

map_bb <- emis_long %>% to_spdf() %>% extent %>% multiply_by(1.1) %>% as.matrix() %>% as.vector
basemap <- ggmap::get_map(map_bb, zoom=4)

ggmap(basemap) + 
  layer_spatial(emis_sf, aes(size=emissions_t, col=Status), alpha=.5) + facet_wrap(~pollutant, ncol=1) +
  coord_sf(xlim=map_bb[c(1,3)], ylim=map_bb[c(2,4)])

ggplot() + annotation_spatial(adm, fill='white') + layer_spatial(emis_smelter_sf, aes(color=Commodity, size=emissions_tpa), alpha=.7) + 
  facet_wrap(~pollutant, ncol=1) +
  theme_crea() + 
  theme(panel.grid = element_blank(), panel.background = element_rect(fill='lightblue')) +
  scale_color_crea_d() +
  labs(title='Air pollutant emissions from metal smelters in Indonesia',
       size='Emissions, t/year') -> p
quicksave(file.path(output_dir, 'emissions/smelters_map.png'), plot=p)



####Captive plants

read_xlsx(plant_file, sheet=1, .name_repair = make.names) -> captive_pp

smelters %>% group_by(Company) %>% 
  group_modify(function(df, ...) {
    df %>% select(starts_with('GEM.ID')) %>% unlist() %>% na.omit %>% (function(x) if(length(x)==0) {NA} else x) %>% 
      tibble(df %>% select(-starts_with('GEM.ID')), Tracker.ID=.)
  }) -> smelter_data_for_captive


captive_pp %>% filter(Tracker.ID %notin% smelter_data_for_captive$Tracker.ID) %>% 
  select(Tracker.ID, Plant, Unit)

#pull emission data from IESR project
project_dir='G:/Shared drives/CREA-HIA/Projects/Indonesia_JETP'
cfpp_emis <- readRDS(file.path(project_dir, 'indonesia_iesr_emission_pathways v2.RDS'))
'indonesia_iesr_emission_pathways v2, with stack data.RDS'

#calculate average emission factors to fill in missing

#calculate power capacity per output and fill missing

#add emission factors for non-coal