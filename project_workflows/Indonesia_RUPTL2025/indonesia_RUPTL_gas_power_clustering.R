read_xlsx(file.path(emissions_dir, "MASTER_Indonesia gas fleet_092025.xlsx")) %>% set_names(make_names(names(.))) ->
  pltg

pltg %<>% mutate(across(matches('start_year|COD'), as.numeric)) %>% replace_na(list(start_year=2010))
pltg %<>% rename(Latitude=latitude, Longitude=longitude)

#assign cluster by coordinates
pltg %>% ungroup %>% distinct(plant_name, Latitude, Longitude) %>% 
  filter(!is.na(Latitude+Longitude)) %>% 
  to_sf_points() %>% 
  mutate(cluster=clusters$cluster[st_nearest_feature(., clusters)],
         distance_to_cluster=st_distance(., clusters) %>% apply(1, min)) ->
  pltg_clustering

clustering %<>% left_join(clusters %>% st_drop_geometry() %>% select(cluster, emission_names))

pltg_clustering %>% filter(distance_to_cluster>25e3) %>% mutate(cluster=cluster(., 25)) %>% use_series(cluster) %>% max
pltg_clustering %>% mutate(cluster=cluster(., 25)) %>% use_series(cluster) %>% max



bind_rows(pltg %>% filter(status=='operating') %>% mutate(category='existing'),
          pltg %>% filter(status!='operating' & !grepl('RUPTL', status)) %>% mutate(category='new - GEM'),
          pltg %>% filter(!is.na(ruptl_2025_re_base_cod)) %>% mutate(category='new - RUPTL-base', start_year=ruptl_2025_re_base_cod),
          pltg %>% filter(!is.na(ruptl_2025_ared_cod)) %>% mutate(category='new - RUPTL-ARED', start_year=ruptl_2025_ared_cod)) %>%
  group_by(category, cluster, start_year) %>%
  summarise(across(c(MW=contains('MW')), sum)) ->
  pltg_by_cluster