require(ggspatial); require(ggmap)
emis_smelter %>% filter(pollutant %in% c('SO2', 'NOx', 'PM'), emissions_tpa>0) %>% to_sf_points() -> emis_smelter_sf
gis_dir='~/GIS/'
adm<-get_adm(0, 'coarse')

ggplot() + annotation_spatial(adm, fill='white') + layer_spatial(emis_smelter_sf, aes(color=Commodity, size=emissions_tpa), alpha=.7) + 
  facet_wrap(~pollutant, ncol=1) +
  theme_crea() + 
  theme(panel.grid = element_blank(), panel.background = element_rect(fill='lightblue')) +
  scale_color_crea_d() +
  labs(title='Air pollutant emissions from metal smelters in Indonesia',
       size='Emissions, t/year') -> p
quicksave(file.path(output_dir, 'emissions/smelters_map.png'), plot=p)


ggplot() + annotation_spatial(adm, fill='white') + 
  layer_spatial(to_sf_points(emis_pp_all), aes(color=Commodity_broad, size=emissions_tpa), alpha=.7) + 
  facet_wrap(~pollutant, ncol=1) +
  theme_crea() + 
  theme(panel.grid = element_blank(), panel.background = element_rect(fill='lightblue')) +
  scale_color_crea_d('dramatic') +
  labs(title='Air pollutant emissions',
       subtitle='from captive power plants linked to metal smelters in Indonesia',
       size='Emissions, t/year') -> p
quicksave(file.path(output_dir, 'emissions/captive_pp_map.png'), plot=p)

emis_pp_all %>% filter(emissions_tpa>0) %>% 
  group_by(fuel, Commodity_broad, pollutant) %>% 
  summarise(across(emissions_tpa, ~sum(.x, na.rm=T))) %>% 
  ggplot(aes(Commodity_broad, emissions_tpa, fill=fuel)) + geom_col() + 
  facet_wrap(~pollutant, scales='free_y') +
  theme_crea() + 
  scale_fill_crea_d('dramatic') + 
  x_at_zero(labels=scales::comma) +
  labs(title='Air pollutant emissions from captive power plants linked to metal smelters in Indonesia',
       y='t/year', x='Commodity') -> p
quicksave(file.path(output_dir, 'emissions/captive pp emissions by commodity.png'), plot=p)


emis_all %>% 
  filter(pollutant %in% polls) %>% 
  group_by(type, Commodity_broad, pollutant) %>% 
  summarise(across(emissions_tpa, ~sum(.x, na.rm=T))) %>% 
  ggplot(aes(Commodity_broad, emissions_tpa, fill=type)) + geom_col() + 
  facet_wrap(~pollutant, scales='free_y') +
  theme_crea(axis.text.x=element_text(angle=30, hjust=1)) + scale_fill_crea_d('dramatic') + 
  x_at_zero(labels=scales::comma) +
  labs(title='Air pollutant emissions from metal smelters and linked captive power plants in Indonesia',
       y='t/year', x='Commodity') -> p
quicksave(file.path(output_dir, 'emissions/smelter and captive pp emissions by commodity.png'), plot=p)

emis_all %>% 
  filter(pollutant %in% polls, emissions_tpa>0) %>% 
  group_by(Province, Commodity_broad, pollutant) %>% 
  summarise(across(emissions_tpa, ~sum(.x, na.rm=T))) %>% 
  ggplot(aes(Province, emissions_tpa, fill=Commodity_broad)) + geom_col() + 
  facet_wrap(~pollutant, scales='free_x') +
  coord_flip() +
  theme_crea() + scale_fill_crea_d('CREA', col.index = c(1:4, 8:12)) + 
  x_at_zero(labels=scales::comma) +
  labs(title='Air pollutant emissions from metal smelters and linked captive power plants in Indonesia',
       subtitle='by province',
       y='t/year', x='Province') -> p
quicksave(file.path(output_dir, 'emissions/smelter and captive pp emissions by province and commodity.png'), plot=p)



emis_all %>% filter(pollutant %in% polls) %>% 
  group_by(Province, type, pollutant) %>% 
  summarise(across(emissions_tpa, ~sum(.x, na.rm=T))) %>% 
  ggplot(aes(Province, emissions_tpa, fill=type)) + geom_col() + 
  facet_wrap(~pollutant, scales='free_x') +
  coord_flip() +
  theme_crea() + scale_fill_crea_d('dramatic') + 
  x_at_zero(labels=scales::comma) +
  labs(title='Air pollutant emissions from metal smelters and linked captive power plants in Indonesia',
       subtitle='by province',
       y='t/year', x='Province', fill='') -> p
quicksave(file.path(output_dir, 'emissions/smelter and captive pp emissions by province.png'), plot=p)
