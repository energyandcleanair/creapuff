require(raster)
require(tidyverse)
require(magrittr)
require(lubridate)
require(readxl)
require(creapuff)
require(creahelpers)
require(rcrea)
require(ggmap)

#pollutants to process
polls=c('SO2', 'NOx', 'PM', 'PM10', 'PM2.5', 'Hg')


#Pollutant inputs - nickel smelting - EIA reporting data
#https://docs.google.com/spreadsheets/d/1corCzD9ThiryQ6rDmU-h7pCdb5nY9R60Ogd00hwg7DY/edit#gid=0
emis_file='~/../Downloads/Pollutant inputs - nickel smelting - EIA reporting data (6).xlsx'

#Indonesia Captive - filter for CELIOS
#https://docs.google.com/spreadsheets/d/1LP_DmK353mFoKP19dtSAHkv_MkKqvwThDLQoxipSWT0/edit#gid=602516471
plant_file='~/../Downloads/Indonesia Captive - filter for CELIOS (13).xlsx'

output_dir='G:/Shared drives/CREA-HIA/Projects/Indonesia-Nickel'

read_wide_xlsx(emis_file,
               header_row_names = c('measurement_unit', 'pollutant'),
               info_columns = c(1:6,16,18,30:40),
               fill_header_rows=F) %>% 
  select(-Year) %>% 
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
  filter(!grepl('secondary', Activity) | grepl('Alumin', Activity)) %>% 
  mutate(emissions_t_per_t = Value * case_when(grepl('kg/Mg', Unit)~1e-3,
                                               grepl('^g/Mg', Unit)~1e-6,
                                               grepl('mg/Mg', Unit)~1e-9,
                                               grepl('µg.*/Mg', Unit)~1e-12),
         emissions_g_per_GJ = Value * case_when(Unit=='g/GJ'~1,
                                                Unit=='mg/GJ'~1e-3,
                                                Unit=='µg/GJ'~1e-6),
         Commodity = Activity %>% gsub(' production(, primary)?', '', .) %>% gsub('Aluminium', 'Aluminum', .) %>% 
           gsub('Aluminum, secondary', 'Aluminum (secondary)', .) %>% gsub('.*ferronickel', 'Nickel', .)) %>% 
  rename(pollutant=Pollutant) %>% 
  mutate(pollutant=recode(pollutant, SOx='SO2', TSP='PM'),
         Commodity=recode(Commodity,Aluminium='Aluminum')) %>% 
  group_by(Commodity, pollutant) %>% slice_max(Value, n=1) %>% 
  filter(grepl('primary|secondary', Activity) | !any(grepl('primary', Activity))) %>% 
  ungroup -> ef

read_xlsx(plant_file, sheet='Metal smelters', .name_repair = make.names) %>% 
  rename(capacity_input_tpa=contains('Capacity.Input'),
         capacity_output_tpa__=contains('Capacity.Output'),
         Commodity.detailed__=contains('Commodity.detailed'),
         lat_smelter=Latitude, lon_smelter=Longitude) %>% 
  mutate(across(matches('^(capacity|lat|lon)'), as.numeric)) -> smelters

#complete years of operation
smelters$Evaluation.Year %<>% na.cover(statmode(., na.rm=T))
smelters %<>% mutate(Year=case_when(!is.na(Finish.Year)~Finish.Year,
                                    !is.na(Construction.Progress....)~Evaluation.Year+round(1-Construction.Progress..../100)*4))

smelters$Finish.Year %<>% na.cover(statmode(., na.rm=T))

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
smelters$diesel_as_backup_only <- rowSums(smelters[,non_diesel], na.rm=T)>smelters$Diesel_MW & !is.na(smelters$Diesel_MW)

smelters %<>% mutate(fuel=case_when(grepl('Coal', PLN.or.Captive.CFPP)~'Coal',
                                    grepl('Diesel', Captive_non.coal) & !diesel_as_backup_only~'Diesel',
                                    grepl('Hydro', Captive_non.coal)~'Hydro',
                                    grepl('Gas', Captive_non.coal)~'Gas'))

emis$Province[emis$Company=="Gunbuster Nickel Industry"] <- "Central Sulawesi"
smelters$Commodity %<>% recode(Aluminium='Aluminum', "Aluminium (secondary)"="Aluminum (secondary)", "Iron"="Iron & steel")

smelters %>% separate(Commodity, paste0('Commodity__', 1:3), sep=', ') %>% 
  pivot_longer(matches('Commodity_|capacity_output_|Commodity.detailed__'), names_to=c(".value", "Var"), names_sep='__') %>% 
  filter(!is.na(Commodity), !grepl('Power', Commodity)) -> smelters_long



#### smelter emissions
#check that all companies in the emission data are found in the smelter list
emis %>% filter(!is.na(fgc) | !is.na(m3_per_min)) %>% anti_join(smelters_long %>% select(Company)) %>% distinct(Company)

#check that there aren't companies with multiple commodities in emission data - the script isn't designed to handle this
smelters_long %>% group_by(Company) %>% filter(length(unique(Commodity))>1) %>% inner_join(emis %>% group_by(Company) %>% filter(!all(is.na(fgc)))) %>% distinct(Company)

emis %<>% left_join(smelters_long, .)

emis %>% group_by(Commodity) %>% summarise(has_data=!all(is.na(fgc)), number_of_plants=length(unique(Company))) -> commodities_with_data
smelters_long %>% group_by(Commodity) %>% summarise(across(contains('capacity_'), ~sum(.x, na.rm=T))) %>% 
  left_join(commodities_with_data) %>% na.omit %>% arrange(desc(capacity_output_tpa))



##calculate emissions volumes
emis %<>% mutate(O2_reference=na.cover(O2_reference, O2_measured),
                 Nm3_per_min=m3_per_min*(21-O2_measured/100)/(21-O2_reference/100),
                 emissions_tpa=fgc*Nm3_per_min*60*8760/1e9)

emis %>% filter(emissions_tpa>0) %>% 
  group_by(Company, Commodity, pollutant) %>% 
  summarise(across(emissions_tpa, ~sum(.x, na.rm=T))) %>% 
  left_join(smelters_long) %>% ungroup %>% mutate(release_height='m') ->
  emis_by_company


#calculate emissions per output
emis %>% filter(Nm3_per_min>0) %>% 
  mutate(Nm3_per_t=Nm3_per_min*60*8760/capacity_output_tpa) %>% 
  group_by(Company, 
           Commodity=ifelse(grepl('CFPP|Diesel Generator', release_point), 'electricity', Commodity), 
           pollutant, 
           emission_type=ifelse(release_point=='CFPP', 'CFPP', 'process')) %>% 
  summarise(across(Nm3_per_t, ~sum(.x[.x>0], na.rm=T)),
            across(fgc, ~weighted.mean(.x[.x>0], Nm3_per_min[.x>0], na.rm=T))) %>% 
  filter(!grepl('Tsingshan|Ruipu', Company) | Commodity=='Nickel') %>% 
  group_by(Commodity, pollutant) %>% summarise(across(c(fgc, Nm3_per_t), mean), n=n()) %>% 
  mutate(emissions_t_per_t=fgc*Nm3_per_t/1e9) -> ef_measured


###add emissions factors
#add fugitive dust from handling and crushing; PM2.5 to PM10 ratio from https://gaftp.epa.gov/ap42/ch11/s1902/draft/ref_15db11s1902%20_june2003.pdf
ef %>% anti_join(ef_measured %>% select(Commodity, pollutant)) %>% bind_rows(ef_measured) %>% 
  mutate(release_height='m') %>% 
  bind_rows(tibble(Commodity='Nickel', emissions_t_per_t=c(0.5184, 0.34867, 0.34867*.25)/1e3, pollutant=c('PM', 'PM10', 'PM2.5'),
                   release_height='l', Source="Environment Australia",
                   Reference="Emission Estimation Technique Manual for Nickel Concentrating, Smelting and Refining. Table 3 - Emission Factors for Dust Generation"))-> 
  ef_all


smelters_long %>% 
  mutate(Commodity_EF = case_when(Commodity %in% ef_all$Commodity~Commodity, 
                                  grepl('steel', Commodity, ignore.case=T)~"Iron & steel",
                                  T~"Other metal")) %>% 
  left_join(ef_all %>% select(Commodity_EF=Commodity, pollutant, release_height, emissions_t_per_t)) %>% 
  mutate(emissions_tpa=emissions_t_per_t*capacity_output_tpa) %>% 
  anti_join(emis_by_company %>% ungroup %>% select(Company, pollutant)) %>% 
  bind_rows(emis_by_company) -> emis_smelter


#add PM speciation
ef %>% filter(grepl('^PM', pollutant)) %>% 
  complete(Commodity, pollutant) %>% 
  group_by(Commodity) %>% 
  mutate(share = Value/Value[pollutant=='PM']) %>% 
  group_by(pollutant) %>% 
  mutate(share = na.cover(share, mean(share, na.rm=T))) %>% 
  select(Commodity, PM_fraction=pollutant, share) %>% mutate(pollutant='PM') -> pm_fractions

steel_fractions = tibble(pollutant='PM', PM_fraction=c('PM', 'PM10', 'PM2.5'), share=c(1,180/300,140/300))
steel_fractions %>% cross_join(tibble(Commodity=c('Iron & steel', 'Steel'))) %>% bind_rows(pm_fractions) -> pm_fractions

pm_fractions %<>% filter(Commodity=='Nickel') %>% mutate(Commodity='NPI') %>% bind_rows(pm_fractions)

emis_smelter %<>% 
  mutate(Commodity_for_partitioning=case_when(Commodity %in% pm_fractions$Commodity~Commodity, T~'Other metal')) %>% 
  group_by(Company, Commodity, release_height) %>% 
  group_modify(function(df, ...) {
    if(!('PM10' %in% df$pollutant)) df %<>% filter(pollutant=='PM') %>% mutate(pollutant='PM10', emissions_tpa=NA) %>% bind_rows(df)
    if(!('PM2.5' %in% df$pollutant)) df %<>% filter(pollutant=='PM') %>% mutate(pollutant='PM2.5', emissions_tpa=NA) %>% bind_rows(df)
    return(df)
  }) %>% 
  left_join(pm_fractions %>% select(Commodity_for_partitioning=Commodity, pollutant=PM_fraction, share)) %>% 
  mutate(across(emissions_tpa, ~na.cover(.x, .x[pollutant=='PM']*share))) %>% 
  select(-Commodity_for_partitioning, -share)



emis_by_company %>% group_by(pollutant) %>% summarise(across(emissions_tpa, ~sum(.x, na.rm=T)))
emis_smelter %>% filter(pollutant %in% polls) %>% group_by(Commodity, pollutant, release_height) %>% 
  summarise(across(emissions_tpa, ~sum(.x, na.rm=T))) %>% 
  spread(pollutant, emissions_tpa)




####Captive plants
#
read_xlsx(plant_file, sheet=1, .name_repair = make.names) -> captive_pp

read_xlsx('~/RPackages/gcpt-analysis/data/Global-Coal-Plant-Tracker-July-2023_ChinaBriefRev.xlsx',
          sheet='Units', .name_repair = make.names) %>% 
  filter(Country=='Indonesia') %>% rename(Tracker.ID=matches('GEM.unit.*ID'), Plant=Plant.name) %>% 
  mutate(GJ_per_MWh=Heat.rate..Btu.per.kWh.*1.05506/1e3) -> gcpt




smelters %>% group_by(Company) %>% 
  group_modify(function(df, ...) {
    df %>% select(starts_with('GEM.ID')) %>% unlist() %>% na.omit %>% (function(x) if(length(x)==0) {NA} else x) %>% 
      tibble(df %>% select(-starts_with('GEM.ID')), Tracker.ID=.)
  }) -> smelter_data_for_captive



smelter_data_for_captive %<>% left_join(gcpt %>% select(Tracker.ID, MW=Capacity..MW.)) %>% 
  mutate(MW=case_when(!is.na(MW)~MW,
                      grepl('MW', Tracker.ID)~force_numeric(Tracker.ID),
                      T~ifelse(fuel=='Coal', Capacity..MW., Capacity_2..MW.)))

###add "imputed" coal capacity for smelters lacking information on captive capacity
#mean capacity at smelters with data
smelter_data_for_captive %>% group_by(Commodity, Company) %>% 
  summarise(across(MW, sum), across(capacity_output_tpa__1, unique)) %>% 
  mutate(MW_per_tpa=MW/capacity_output_tpa__1) %>% 
  summarise(across(MW_per_tpa, mean, na.rm=T)) %>% 
  filter(Commodity %in% c('Nickel', 'Iron & steel')) -> MW_per_tpa


ix <- (is.na(smelter_data_for_captive$MW) & 
         !(smelter_data_for_captive$Related.Industrial.Parks.or.Captive.Plants %eqna% "IMIP") &
         !(smelter_data_for_captive$PLN.or.Captive.CFPP %eqna% 'PLN'))

smelter_data_for_captive %<>% left_join(MW_per_tpa)
smelter_data_for_captive$MW[ix] <- smelter_data_for_captive$capacity_output_tpa__1[ix] * smelter_data_for_captive$MW_per_tpa[ix]
smelter_data_for_captive$coal_MW[ix] <- smelter_data_for_captive$MW[ix]
smelter_data_for_captive$fuel[ix] <- "Coal"
smelter_data_for_captive %<>% ungroup %>% 
  mutate(basis_for_captive_capacity = case_when(PLN.or.Captive.CFPP=="PLN"~"reported to buy from PLN",
                                                Related.Industrial.Parks.or.Captive.Plants =="IMIP" & is.na(MW)~"assume linkage to existing capacity in IMIP", 
                                                !ix~"reported", 
                                                is.na(capacity_output_tpa__1)~"missing metal production capacity data",
                                                T~"estimated based on metal production capacity"))

ix <- which(!grepl('^G', smelter_data_for_captive$Tracker.ID))
smelter_data_for_captive$Tracker.ID[ix] <- paste0('CREA', seq_along(ix))


smelter_data_for_captive %>% 
  filter(Province %in% c('Southeast Sulawesi', 'Central Sulawesi', 'North Maluku'), Company!="Vale Indonesia (Pomalaa)") %>% 
  rowwise %>% 
  mutate(power_sources = c(PLN.or.Captive.CFPP, Captive_non.coal) %>% na.omit %>% paste(collapse='; ')) %>% 
  select(Tracker.ID, Company, Province, Commodity, matches('capacity_output|Commodity.detailed'), 
         Related.Industrial.Parks.or.Captive.Plants,
         power_sources,
         contains("_MW"), diesel_as_backup_only, basis_for_captive_capacity) %>% 
  write_csv(file.path(output_dir, "captive power overview.csv"))

#add plants not in captive list
captive_pp %<>% left_join(smelter_data_for_captive %>% select(Tracker.ID, smelter_company=Company))

smelter_data_for_captive %>% 
  filter(!is.na(Tracker.ID), fuel!='Diesel' | !diesel_as_backup_only, !is.na(MW)) %>% 
  anti_join(captive_pp %>% select(Tracker.ID)) %>% 
  left_join(gcpt %>% select(Tracker.ID, Start.year, MW_GCPT=Capacity..MW., Latitude, Longitude, Combustion.technology, Plant, Unit=Unit.name)) %>% 
  mutate(Year=na.cover(Start.year, Finish.Year),
         Latitude=na.cover(Latitude, lat_smelter),
         Longitude=na.cover(Longitude, lon_smelter),
         MW=na.cover(MW_GCPT, MW)) %>% 
  select(Tracker.ID, Plant, Unit,Province, Commodity,
         smelter_company=Company, Year, Combustion.technology, Latitude, Longitude, MW, fuel,
         basis_for_captive_capacity) %>% 
  bind_rows(captive_pp %>% rename(MW=Capacity..MW., Province=contains('Province')) %>% 
              mutate(across(c(Latitude, Longitude), as.numeric), fuel='Coal',
                     Commodity=Commodity %>% capitalize_first %>% gsub('Aluminium', 'Aluminum', .))) ->
  captive_pp_all



#add heat rate data from GCPT
captive_pp_all %<>% left_join(gcpt %>% select(Tracker.ID, GJ_per_MWh, Combustion.technology))


#calculate median FGC and FGV/MWhth in pollutant input data for coal and diesel
emis %>% filter(grepl('CFPP|Diesel', release_point)) %>% mutate(fuel=ifelse(grepl('Diesel', release_point), 'Diesel', 'Coal')) ->
  emis_pp

emis_pp %>% 
  group_by(fuel, pollutant) %>%
  mutate(Nm3_per_MWh=Nm3_per_min*60/coal_MW) %>% 
  summarise(across(c(default_fgc=fgc, Nm3_per_MWh, O2_reference), ~median(.x, na.rm=T))) %>% 
  filter(!is.na(default_fgc)) ->
  default_fgc

emis_pp %>% group_by(Company, fuel, pollutant) %>% 
  summarise(across(fgc, ~mean(.x, na.rm=T))) %>% na.omit ->
  emis_pp_measured

#apply heat rate based on regression from GCPT
gcpt %>% rename(Year=Start.year) %>% lm(GJ_per_MWh~Year+Combustion.technology, data=.) -> m
summary(m)

captive_pp_all %<>% mutate(Combustion.technology=case_when(Combustion.technology!='unknown'~Combustion.technology,
                                                           MW<250~'subcritical',
                                                           T~'supercritical'),
                           Year=case_when(H1_2023.Status=='operating'~Year,
                                          H1_2023.Status=='construction'~pmax(Year, 2024, na.rm=T),
                                          T~pmax(Year, 2027, na.rm=T)))

captive_pp_all %<>% ungroup() %>% 
  mutate(GJ_per_MWh=case_when(!is.na(GJ_per_MWh)~GJ_per_MWh,
                              fuel=='Coal'~predict(m, .),
                              fuel=='Diesel'~3.6/.35,
                              fuel=='Gas'~3.6/.5,))



#add emission factors for non-coal
ef %>% filter(grepl('electricity', Activity)) %>% 
  mutate(fuel=case_when(grepl('Gaseous', Activity)~'Gas',
                        grepl('Gas Oil|Diesel', Activity)~'Diesel')) %>% 
  select(fuel, pollutant, g_per_GJ_default=emissions_g_per_GJ) ->
  ef_pp


#fgv: 3.6/.38 GJ/MWh * 350 Nm3/GJ = 3315.789 Nm3/MWh
#fgv: coal 350*(21-6)/(21-7) Nm3/GJ = 375 Nm3/GJ

#link pollutant input data to captive PP data through company name

captive_pp_all %>% cross_join(tibble(pollutant=polls)) %>% 
  left_join(emis_pp_measured %>% rename(smelter_company=Company)) %>% 
  left_join(default_fgc) %>% 
  left_join(ef_pp) %>% 
  mutate(fgc=na.cover(fgc, default_fgc),
         fgv=case_when(fuel=='Coal'~375,
                       fuel=='Diesel'~315), #https://wiki.prtr.thru.de/images/1/12/EURELECTRIC-VGB_E-PRTR_june2010.pdf
         utilization=.8,
         g_per_GJ=case_when(!is.na(fgc)~fgv*fgc/1e3,
                            T~g_per_GJ_default),
         emissions_tpa=g_per_GJ*GJ_per_MWh*MW*8760*utilization/1e6) ->
  emis_pp_all

emis_pp_all %<>% mutate(Commodity_broad=Commodity %>% gsub('( \\(|, ).*', '', .) %>% gsub('Iron$', 'Iron & steel', .),
                        release_height=ifelse(fuel=='Coal', 'h', 'm'))
emis_smelter %<>% mutate(Commodity_broad=Commodity %>% gsub('( \\(|, ).*', '', .) %>% gsub('Iron$|^Steel', 'Iron & steel', .))

#add PM fractions
emis_pp_all %<>% group_by(Tracker.ID) %>% 
  mutate(emissions_tpa=case_when(!is.na(emissions_tpa)~emissions_tpa,
                                 pollutant=='PM10'~emissions_tpa[pollutant=='PM']*54/80,
                                 pollutant=='PM2.5'~emissions_tpa[pollutant=='PM']*24/80)) %>% ungroup



bind_rows(emis_pp_all %>% mutate(type='captive power'),
          emis_smelter %>% mutate(type='process', fuel=NA) %>% rename(Longitude=lon_smelter, Latitude=lat_smelter, smelter_company=Company)) -> emis_all


#clustering
emis_all %>% 
  filter(pollutant %in% polls, Province %in% c('Southeast Sulawesi', 'Central Sulawesi', 'North Maluku')) ->
  emis_modeled

clusters <- read_csv(file.path(output_dir, 'emissions', 'emissions_by_cluster.csv')) %>% 
  group_by(loc_cluster) %>% summarise(across(c(lon, lat), mean))

require(sf)

clusters %>% to_sf_points %>% 
  st_distance(emis_modeled %>% to_sf_points()) %>% 
  apply(2, function(x) clusters$loc_cluster[x==min(x)]) -> emis_modeled$loc_cluster

emis_modeled %<>% mutate(Year=na.cover(Year, quantile(Year, .75, na.rm=T)))

emis_modeled %>% write_csv(file.path(output_dir, 'emissions', 'emissions_with_cluster v3.csv'))


emis_modeled %>% mutate(emissions_tpa=emissions_tpa * case_when(type=='process' & grepl('^PM', pollutant)~.05,
                                                                type=="captive power" & pollutant=='SO2'~.15,
                                                                type=="captive power" & pollutant=='NOx'~.5,
                                                                type=="captive power" & pollutant=='Hg'~.7,
                                                                T~1)) %>% 
  write_csv(file.path(output_dir, 'emissions', 'emissions_with_cluster_APC v3.csv'))
