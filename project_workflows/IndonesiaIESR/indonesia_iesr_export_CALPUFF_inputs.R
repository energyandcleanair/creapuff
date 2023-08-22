project_dir='G:/Shared drives/CREA-HIA/Projects/Indonesia_JETP'
output_dir=project_dir

emis_long <- readRDS(file.path(output_dir, 'indonesia_iesr_emission_pathways v2, with stack data.RDS'))

#export base year emissions for modeling
stack_cols <- names(emis_long) %>% grep('Height|Diameter|Temp|Velocity', ., value=T)
emis_long %<>%
  mutate(across(all_of(stack_cols), as.numeric),
         Velocity..m.s.=ifelse(Velocity..m.s.>50, NA, Velocity..m.s.),
         Diameter..m.=ifelse(Diameter..m.>10, NA, Diameter..m.))

emis_long %>% group_by(CFPP.name) %>% filter(scenario=='BAU') %>% 
  filter(year==pmax(2022, COD)) ->
  emis_base

emis_base %>% 
  distinct(CFPP.name, .keep_all=T) ->
  stack_training_data

stack_models <- list()
for(param in stack_cols) {
  ind <- !is.na(stack_training_data[[param]]) & 
    !(grepl('Temp', param) & 
        (stack_training_data$Exit.Temp....>300 |
           stack_training_data$Exit.Temp....<30))
  
  stack_training_data[ind, ] -> indata
  
  #prevent extrapolation to values outside the range of input data
  indata$MW -> indata$MW_clipped
  emis_base %<>% mutate(MW_clipped = MW %>% pmin(max(indata$MW)) %>% pmax(min(indata$MW)))
  
  indata$COD -> indata$COD_clipped
  emis_base %<>% mutate(COD_clipped = COD %>% pmin(max(indata$COD)) %>% pmax(min(indata$COD)))
  
  indys = case_when(grepl('Temp', param)~"SO2_control",
                    T~"MW_clipped + COD_clipped")
  
  f = as.formula(paste(param,"~",indys))
  
  lm(f, indata) -> m
  ind = is.na(emis_base[[param]])
  
  emis_base[[param]][ind] <- predict(m, emis_base[ind, ]) %>% round(1) %>% multiply_by(2) %>% signif(2) %>% divide_by(2)
  
  stack_models[[param]] <- m
  emis_base %<>% select(-ends_with('_clipped'))
}


stack_training_data %>% 
  pivot_longer(all_of(stack_cols)) %>% 
  ggplot(aes(MW, value, col=SO2_control)) + geom_point() + geom_smooth(method='lm') + facet_wrap(~name, scales='free_y')

emis_base %>% 
  pivot_longer(all_of(stack_cols)) %>% 
  ggplot(aes(MW, value, col=SO2_control)) + geom_point() + facet_wrap(~name, scales='free_y')


#cluster emissions
emis_base %>% to_spdf %>% cluster(1) -> emis_base$loc_cluster

cut_stack_params <- function(x) {
  n_breaks = round((max(x)/min(x)-1)*1.5, 0)+1
  cuts = 'all'
  if(n_breaks>1) cuts=cut(x, breaks=n_breaks) %>% as.character
  return(cuts)
}

emis_base %<>% 
  group_by(loc_cluster) %>% select(-matches('_cut')) %>% 
  mutate(across(matches('height|temp'), cut_stack_params, .names="{.col}_cut")) %>%
  group_by(loc_cluster, across(matches('_cut'))) %>% 
  mutate(cluster=cur_group_id()) %>% ungroup

emis_base %>% group_by(cluster) %>% 
  mutate(Plant = na.cover(Plant, gsub(' Unit.*', '', CFPP.name)),
         emission_names=paste0(substr(gsub(' ', '', first(Plant)), 1, 5), unique(cluster))) %>% 
  group_by(CFPP.name, emission_names, cluster) %>% 
  select(Longitude, Latitude, all_of(stack_cols), MW, COD, pollutant, emissions_t) %>% 
  mutate(pollutant = paste0(pollutant, ifelse(pollutant=='Hg', '_kgpa', '_tpa'))) %>% 
  spread(pollutant, emissions_t) %>% 
  mutate(Hg_kgpa = Hg_kgpa*1e3) %>% 
  write_csv(file.path(emissions_dir,'emissions inputs, with clustering.csv')) ->
  emis_base_wide

emis_base_wide %>% 
  group_by(emission_names, cluster) %>% 
  summarise(across(c(Longitude, Latitude, all_of(stack_cols)), mean, na.rm=T),
            across(c(MW, ends_with('pa')), sum, na.rm=T)) %>% 
  write_csv(file.path(emissions_dir,'emissions, clustered.csv')) ->
  emissions_clustered

emis_base_wide %>% ungroup %>% summarise(across(ends_with('pa'), sum))

emis_base_wide %>% pivot_longer(c(all_of(stack_cols), ends_with('pa'))) %>% 
  ggplot(aes(MW, value)) + geom_point() + facet_wrap(~name, scales='free_y')
