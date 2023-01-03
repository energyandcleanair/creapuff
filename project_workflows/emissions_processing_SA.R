stack_file = file.path(emissions_dir, 'South Africa Coal Power Plant Emission data for modeling20190620-Lauri.xlsx')


read_xlsx(stack_file,
          sheet='ByStack', skip=1, n_max=50) %>% 
  select(-contains('UTM')) %>% 
  select(plant=starts_with('Power'),
         stack=Stack,
         lat=starts_with('Lat'),lon=starts_with('Lon'),
         stack.height=starts_with('Stack height'),
         diameter=contains('diameter'),
         temperature=contains('temperature'),
         velocity=contains('velocity')) ->
  stacks


emisfile=file.path(emissions_dir, 'Eskom Monthly Emissions April 21 -Mar 22.xlsx')

read_xlsx(emisfile, skip=2, n_max=12, col_names = F) -> emis

read_xlsx(stack_file, skip=1, n_max=20, sheet='Mercury') %>% select(1:10) %>% select(plant='Power Station', value=starts_with('kg/a'), AQCS='PM control') %>% 
  mutate(pollutant='Hg', FGD=grepl('FGD', AQCS)) -> hg

#ESP+wFGD / ESP / FF / none / CFBC

read_xlsx(emisfile, skip=0, n_max=2, col_names = F) -> plantnames

plantnames %<>% t %>% as_tibble() %>% set_names(c('plant', 'pollutant')) %>% 
  fill(plant, .direction='down') %>% 
  mutate(col=names(plantnames),
         pollutant=recode(pollutant, NO2='NOx', SOx='SO2'),
         newname=paste(plant,pollutant, sep='_')) %>% 
  na.omit

names(emis)[1] <- 'month'
names(emis)[match(plantnames$col, names(emis))] <- plantnames$newname

emis %<>% select(month, contains('_')) %>% 
  pivot_longer(-month) %>% 
  separate(name, c('plant', 'pollutant')) %>% 
  filter(pollutant!='N20') %>% 
  mutate(month=month(month))

emis %<>% 
  group_by(plant, pollutant) %>% 
  mutate(value = value %>% coalesce(mean(value, na.rm=T)),
         monthscaling=value/mean(value)) %>% 
  ungroup

emis %>% 
  group_by(plant, pollutant) %>% 
  summarise(across(value, sum)) %>%
  bind_rows(hg %>% select(plant, pollutant, value)) %>% 
  left_join(hg %>% select(plant, AQCS, FGD)) %>% 
  left_join(stacks) %>% 
  group_by(plant, pollutant) %>% 
  mutate(value=value/length(value),
         AQCS = case_when(grepl('50\\%', AQCS)~ifelse(stack==1, 'ESP', 'FF'),
                          grepl('FF', AQCS)~'FF',
                          AQCS=='ESP'~'ESP')) %>% 
  spread(pollutant, value) ->
  emissions_data



read_xlsx(file.path(emissions_dir, 'LCPP_Emissions.xlsx'), sheet='emissions for modeling') %>% 
  select(-contains('unmitigated')) %>% filter(source != 'Total') %>% 
  rename(PPM25=PM2.5, NOx_tpa=NOx, Hg_kgpa=Hg) %>% 
  mutate(PM10=PM10-PPM25, PM15=TSP-PM10) -> emis_lepha

read_xlsx(file.path(emissions_dir, 'LCPP_Emissions.xlsx'), sheet='Coordinates') %>% fill(feature) %>% rename(lat=latitude, lon=longitude) -> coords_lepha

emis_lepha %>% filter(source=='IPP') %>% rename(emission_names = source) -> emis_ipp

coords_lepha %>% rename(emission_names = feature) %>% 
  inner_join(emis_ipp) ->
  emis_ipp

st_read(file.path(emissions_dir, 'Lephalale.kml')) %>% 
  group_by(Name) %>% mutate(emission_names=Name %>% unique %>% make_srcnam()) %>% 
  rename(source=Name) -> polys_mine

emis_lepha %>% filter(PM10>0) %>% mutate(emission_names=make_srcnam(source)) %>% inner_join(polys_mine, .) %>% 
  select(-matches('SO2|NOx|Hg')) -> emis_mine

emis_ipp %>% mutate(plant='Lephalale IPP') %>%  
  bind_rows(emissions_data) %>% 
  group_by(plant) %>% summarise(across(c(lon, lat), mean)) %>% 
  creahelpers::to_spdf() ->
  point_sources
