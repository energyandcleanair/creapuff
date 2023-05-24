#plant data
read_xlsx(emissions_file, sheet='PLN & IPP', skip=1) %>% 
  rename(Latitude=matches('Latitu.e'), gas_unit='Unit for gases', APC = starts_with('Emission Control'), 
         MW=Capacity,
         cofiring_name=matches('name.*co.firing')) %>% 
  set_names(make.names(names(.))) ->
  emis

read_xlsx(emissions_file, sheet='Captive', skip=0) %>% mutate(Owner='captive') %>% 
  set_names(make.names(names(.))) %>% 
  rename(COD=Year, MW=contains('MW'), Steam.Tech.=Combustion.technology, province=contains('province')) %>% 
  mutate(CFPP.name=paste(Plant, Unit)) -> captive

#combine captive data
emis %<>% bind_rows(captive)

#add province based on coordinates
require(sf)
getadm(1, 'low') %>% subset(NAME_0=='Indonesia') %>% st_as_sf() -> adm1
emis %<>% 
  distinct(Latitude, Longitude) %>% 
  spdf %>% st_as_sf() %>% 
  mutate(province=adm1$NAME_1[st_nearest_feature(., adm1)]) %>% 
  st_drop_geometry() %>% 
  left_join(emis %>% select(-province))


#housekeeping
emis %<>% 
  mutate(across(Status, tolower),
         COD=case_when(!is.na(COD)~COD,
                       Status=='construction'~year(today())+2,
                       Status=='pre-permit'~year(today())+5,
                       Status=='operating'~year(today())-2,
                       Status %in% c('cancelled', 'shelved')~Inf),
         Hg.in.coal..ppb = na.cover(Hg.in.coal..ppb, median(Hg.in.coal..ppb, na.rm=T)),
         Status=case_when(!is.na(Status)~Status, COD<=2022~'operating', COD<=2025~'construction', T~'announced'))


#add cofiring data
read_xlsx(emissions_file, sheet='Co-firing inputs', skip=0) %>% 
  set_names(make.names(names(.))) %>% 
  select(cofiring_name=Name.of.CPP, 
         cofiring_status=Status,
         cofiring_share=Co.firing.Portion, 
         biomass_type=Type.of.Biomass,
         cofiring_notes=Notes.Updates,
         cofiring_MW=contains('MW'),
         cofiring_date=contains('.date.'),
         cofiring_future_target=Planned.future.percentage,
         cofiring_target_date=Target.year) %>% 
  mutate(across(cofiring_MW, ~sapply(.x, function(x) x %>% gsub('x', '*', .) %>% parse(text=.) %>% eval)),
         across(cofiring_share, as.numeric),
         cofiring_share=na.cover(cofiring_share, median(cofiring_share, na.rm=T))) %>% 
  filter(!is.na(cofiring_name)) -> cofiring_data

cofiring_data %>% left_join(emis, .) -> emis

#convert units and add categories
emis %<>% 
  mutate(
    across(c(SOx, NOx, PM), force.numeric),
    #convert ppm
    SOx = SOx * ifelse(gas_unit=='ppm', 2.62, 1),
    NOx = NOx * ifelse(gas_unit=='ppm', 1.81, 1),
    
    #exclude implausible values
    SOx=ifelse(SOx<5, NA, SOx),
    NOx=ifelse(NOx<35, NA, NOx),
    PM=ifelse(PM<5, NA, PM),
    
    SO2_control = case_when(grepl('FGD|Limestone', APC) | grepl('CFB', Boiler.Tech.) ~'FGD',
                            SOx<200 & COD > 2015 ~ 'FGD',
                            T~'none'),
    NOx_control = case_when(grepl('SCR', APC)~'SCR',
                            grepl('Low NOx', APC, ignore.case=T)~'LNB',
                            !is.na(APC)~'none'),
    is_new = COD>2023,
    is_small = MW<=100
  )


#1.5 degree trajectory
read_xlsx(file.path(emissions_dir, 'GCAM_elec_gen_and_capacity.xlsx'), sheet=1) %>% 
  pivot_longer(matches('[0-9]{4}'), names_to='year') %>% 
  mutate(across(year, as.numeric)) %>% 
  separate(Variable, c('variable', 'energy', 'source', 'source_subtype'), sep='\\|') %>% 
  mutate(variable=ifelse(variable=='Capacity', 'GW', 'EJ')) -> cap

read_xlsx(file.path(emissions_dir, 'GCAM_elec_gen_and_capacity.xlsx'), sheet=2) %>% 
  rename(source=Variable, value=TWh) %>% 
  mutate(value=value*3600/1e6,
         variable='EJ') -> gen

gen %<>% bind_rows(cap)

gen %>% filter(grepl('Coal', source)) %>% 
  mutate(source_subtype = case_when(!is.na(source_subtype)~source_subtype,
                                    grepl('ccs', source)~source %>% gsub('Coal ', '', .) %>% gsub('ccs', 'CCS', .)),
         source='Coal') %>% 
  group_by(Scenario, year, variable, source) %>% 
  #add CCS+non-CCS total if missing
  group_modify(function(df, ...) {
    if(!any(is.na(df$source_subtype)))
      df %<>% summarise(across(value, sum)) %>% bind_rows(df)
    return(df)
  }) -> coal_gen


#fill missing values and align past values between scenarios
coal_gen %<>% ungroup %>% 
  complete(Scenario, year, variable, source, source_subtype) %>% 
  group_by(variable, source, source_subtype, year) %>% 
  mutate(value=case_when((is.na(value) & year<=2050) | year<=2020~max(value, na.rm=T), T~value),
         value=ifelse(!is.finite(value), NA, value),
         value=ifelse(year<=2020 & grepl('w/ CCS', source_subtype), 0, value)) %>% 
  group_by(variable, source, source_subtype) %>% 
  fill(value, .direction = 'downup')

#add CCS share
coal_gen %<>% filter(variable=='EJ', !is.na(source_subtype)) %>% 
  group_by(Scenario, year) %>% 
  summarise(value=sum(value[grepl('w/ CCS', source_subtype)], na.rm=T)/sum(value)) %>% 
  mutate(variable='ccs_share') %>% 
  bind_rows(coal_gen) %>% 
  filter(is.na(source_subtype))

#add LF
coal_gen %<>% group_by(Scenario, year) %>% 
  summarise(value = value[variable=='EJ']/(value[variable=='GW']*8760*3600/1e9)) %>% 
  mutate(variable='load_factor', value=pmin(.9, value)) %>% 
  bind_rows(coal_gen)

#retrieve base utilization
coal_gen %>% filter(year==2020, variable=='load_factor') %>% use_series(value) %>% unique -> base_utilization_gcam
utilization_uplift = 0.675 / base_utilization_gcam #actual utilization calculated from BP generation data

coal_gen %>% ggplot(aes(year, value, col=Scenario)) + facet_wrap(~variable, scales='free_y') + geom_line() +
  expand_limits(y=0)

coal_gen %<>% select(Scenario, year, variable, value) %>% spread(variable, value)
