#plant data
read_xlsx(emissions_file, sheet='PLN & IPP', skip=0) %>% 
  rename(Latitude=matches('Latitu.e'), gas_unit='Unit for gases', APC = starts_with('Emission Control'), 
         MW=contains('MW'),
         cofiring_name=matches('name.*co.firing')) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(across(c(Latitude, Longitude), as.numeric)) ->
  emis

read_xlsx(emissions_file, sheet='Captive', skip=0) %>% mutate(Owner='captive') %>% 
  set_names(make.names(names(.))) %>% 
  rename(COD=matches('Year|COD'), MW=contains('MW'), province=contains('province'),
         Status.July.2024=Status.Jul.2024) %>% 
  #remove duplicate entries
  filter(!(GEM.ID %in% emis$GEM.ID & GEM.ID!='NA' & !is.na(GEM.ID)),
         !(CFPP.name %in% emis$CFPP.name)) -> captive
  #mutate(CFPP.name=paste(Plant, Unit)) 

#combine captive data
emis %<>% bind_rows(captive)
emis$GEM.ID[emis$GEM.ID=='NA'] <- NA


#derive new GEM IDs
emis %<>% mutate(gem_unit_phase_id=ifelse(nchar(GEM.ID)==13, GEM.ID, GEM.ID %>% gsub('G', 'G100000', .)))

#set missing COD when 0
emis$COD[emis$COD==0] <- NA

#add GCPT data
read_xlsx(gcpt_file, sheet='Units') %>% set_names(make_names(names(.))) %>% filter(country_area=='Indonesia') -> gcpt

#designate a location for "Sumatera Hybrid"
ind <- grep('Sumatra Hybrid', emis$CFPP.name)
emis[ind, c('gem_unit_phase_id', 'Latitude', 'Longitude')] <- gcpt[grepl('Mulut Tambang', gcpt$plant_name), c('gem_unit_phase_id', 'latitude', 'longitude')]
emis$CFPP.name[ind] %<>% gsub('\\(TBD) ', '', .) %>% paste('Unit', seq_along(.))
emis$Steam.Tech.[ind] <- 'supercritical'

#remove new plants not included in RUPTL
emis %<>% filter(Owner=='captive' | COD<=2024 | !is.na(COD_RUPTL.2025))

gcpt %>% mutate(gem_plant_unit_name = plant_name %>% gsub(' power station', '', .) %>% paste(unit_name)) %>% 
  select(gem_unit_phase_id, gem_plant_unit_name, Status=status, Steam.Tech._GEM=combustion_technology, COD_GEM=start_year) %>% 
  left_join(emis, .) %>% 
  mutate(Status=na.cover(Status, Status.July.2024),
         COD=COD_RUPTL.2025 %>% na.cover(COD_GEM) %>% na.cover(COD),
         Steam.Tech.=na.cover(Steam.Tech., Steam.Tech._GEM)) ->
  emis

#correct duplicate names
emis %<>% group_by(CFPP.name) %>% 
  mutate(CFPP.name=case_when(n()>1 | grepl('Jinchuan', CFPP.name) ~gem_plant_unit_name, T~CFPP.name)) %>% 
  ungroup

#add province based on coordinates
require(sf)
get_adm(1, 'low') %>% subset(NAME_0=='Indonesia') %>% st_as_sf() -> adm1
emis %<>% 
  distinct(Latitude, Longitude) %>% 
  to_spdf %>% st_as_sf() %>% 
  mutate(province_GIS=adm1$NAME_1[st_nearest_feature(., adm1)]) %>% 
  st_drop_geometry() %>% 
  left_join(emis, .)

emis %>% to_sf_points() %>% 
  ggplot() + 
  geom_sf(data=adm1) +
  geom_sf(aes(size=MW, color=Status))
  
emis %<>% select(-province) %>% rename(province=province_GIS)
  
#housekeeping
emis %<>% 
  mutate(across(Status, tolower),
         COD=case_when(COD>0~COD,
                       Status=='construction'~year(today())+2,
                       Status=='pre-permit'~year(today())+5,
                       Status=='operating'~year(today())-2,
                       Status %in% c('cancelled', 'shelved')~Inf),
         Hg.in.coal..ppb = na.cover(Hg.in.coal..ppb, median(Hg.in.coal..ppb, na.rm=T)),
         Status=case_when(!is.na(Status)~Status, COD<=year(today())-1~'operating', COD<=year(today())+3~'construction', T~'announced'))


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

#add name matching from old data
read_xlsx(file.path(emissions_dir, 'MASTERLIST_Indonesia Coal.xlsx'), sheet='PLN & IPP', skip=1) %>% 
  set_names(make.names(names(.))) %>% 
  select(CFPP.name, cofiring_name=matches('name.*co\\.firing')) %>% 
  distinct() ->
  plant_name_dict

emis %<>% left_join(plant_name_dict) %>% mutate(cofiring_name=ifelse(grepl('Sofifi', CFPP.name), 'Sofifi', cofiring_name)) %>% left_join(cofiring_data)

names(emis) %<>% gsub('Conc\\.\\.', '', .)

#convert units and add categories
emis %<>% 
  mutate(
    across(c(SOx, NOx, PM), function(x) x %>% gsub(' .*', '', .) %>% as.numeric),
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

#RUPTL and JET-P trajectories
read_wide_xlsx(file.path(emissions_dir, "RUPTL 2025.xlsx"), sheet='coal & gas',
               header_row_names = c('variable', 'scenario', 'fuel'),
               info_columns = 1) %>% filter(fuel!='Year') %>% 
  rename(year=Year) -> ruptl_cap

ruptl_cap %>% ggplot(aes(year, value, col=fuel, linetype=scenario)) + facet_wrap(~variable, scales='free_y') + geom_line()

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

gen %>% filter(grepl('Coal|Gas', source)) %>% 
  mutate(source_subtype = case_when(!is.na(source_subtype)~source_subtype,
                                    grepl('ccs', source)~source %>% gsub('(Coal|Gas) ', '', .) %>% gsub('ccs', 'CCS', .)),
         source=source %>% gsub(' .*', '', .)) %>% 
  group_by(Scenario, year, variable, source) %>% 
  #add CCS+non-CCS total if missing
  group_modify(function(df, ...) {
    if(!any(is.na(df$source_subtype)))
      df %<>% summarise(across(value, sum)) %>% bind_rows(df)
    return(df)
  }) -> coal_gen


ruptl_cap %>% rename(Scenario=scenario, source=fuel) %>% 
  mutate(value=value*case_when(grepl('MW', variable)~1e-3,
                               grepl('GWh', variable)~3600/1e9),
         variable=case_when(grepl('MW', variable)~'GW',
                            grepl('GWh', variable)~'EJ')) %>% 
  bind_rows(coal_gen) ->
  coal_gen

coal_gen %>% replace_na(list(source_subtype='Total')) %>% 
  ggplot(aes(year, value, col=Scenario, linetype=source_subtype)) + 
  geom_line() + facet_wrap(~source+variable, scales='free_y')

#fill missing values and align past values between scenarios

#use RUPTL until 2025
#phase in the divergence between scenarios by 2030
#coal phased out linearly from 2040 to 2050
#gas stabilized at 2040 level to 2050

fade <- function(x1, x2, year, from, to) {
  w=((year-from)/(to-from)) %>% pmax(0) %>% pmin(1)
  x1 * (1-w) + x2 * w
}

coal_gen %<>% ungroup %>% 
  complete(Scenario, year, variable, source, source_subtype) %>% 
  group_by(variable, source, source_subtype, Scenario) %>% 
  mutate(value=zoo::na.approx(value, year, year, na.rm=F, rule=1:2))

coal_gen %>% 
  filter(is.na(source_subtype)) %>% 
  group_by(variable, source, source_subtype) %>% 
  mutate(value=case_when(year<=2030~fade(value[grepl('RUPTL', Scenario)], value, year, from=2025, to=2030),
                         source=='Coal' & year>=2040~fade(value, 0, year, from=2040, to=2050),
                         T~value)) ->
  coal_gen_total

#add CCS share
coal_gen %>% filter(variable=='EJ') %>% 
  group_by(source, Scenario, year) %>% 
  summarise(value=sum(value[grepl('w/ CCS', source_subtype)], na.rm=T)/sum(value[!is.na(source_subtype)])) %>% 
  mutate(variable='ccs_share', value=na.cover(value, 0)) %>% 
  bind_rows(coal_gen_total) ->
  coal_gen

#add LF
coal_gen %<>% group_by(source, Scenario, year) %>% 
  summarise(value = value[variable=='EJ']/(value[variable=='GW']*8760*3600/1e9)) %>% 
  mutate(variable='load_factor', value=pmin(.9, value)) %>% 
  bind_rows(coal_gen)

#retrieve base utilization
coal_gen %>% filter(year==2024, variable=='load_factor') %>% ungroup %>% distinct(source, value) -> base_utilization
#utilization_uplift = 0.675 / base_utilization_gcam #actual utilization calculated from BP generation data
utilization_uplift = 1 #not needed when using RUPTL data
  
coal_gen %>% 
  filter(Scenario!='cpol', year<=2050, !is.na(value)) %>% 
  ggplot(aes(year, value, col=Scenario, linetype=source)) + facet_wrap(~variable, scales='free_y') + geom_line() +
  expand_limits(y=0)

coal_gen %<>% select(source, Scenario, year, variable, value) %>% spread(variable, value)
