require(terra)
require(sf)
require(raster)
require(tidyverse)
require(magrittr)
require(lubridate)
require(readxl)
#require(creapuff)
require(creahelpers)
require(rcrea)

emissions_dir='~/CALPUFF/creapuff/project_workflows/Indonesia_RUPTL2025/data'
emissions_file=file.path(emissions_dir, 'MASTERLIST_Indonesia CFPPs - PLN & IPP and captive, July 2026.xlsx')
gcpt_file=file.path(emissions_dir, 'Global-Coal-Plant-Tracker-January-2026.xlsx')

project_dir='G:/Shared drives/CREA-HIA/Projects/Indonesia_JETP'
#output_dir=project_dir
output_dir <- "~/CALPUFF/creapuff/project_workflows/Indonesia_RUPTL2025/data"

gis_dir = '~/GIS'

emissions_dir_old <- "~/CALPUFF/creapuff/project_workflows/IndonesiaIESR/data"
source(file.path(emissions_dir, '../indonesia_RUPTL_read_plantdata.R'))

emis %>% group_by(SO2_control, is_new, is_small) %>% summarise(across(c(SOx_default=SOx), median, na.rm=T)) %>% 
  mutate(across(SOx_default, ~pmin(.x, 550, na.rm=T))) ->
  so2_defaults

emis %>% group_by(NOx_control, is_new, is_small) %>% summarise(across(c(NOx_default=NOx), median, na.rm=T)) %>% 
  mutate(across(NOx_default, ~pmin(.x, 550, na.rm=T))) ->
  nox_defaults

emis %>% group_by(is_small) %>% summarise(across(c(PM_default=PM), median, na.rm=T)) %>% 
  mutate(across(PM_default, ~pmin(.x, 100, na.rm=T))) ->
  pm_defaults

emis %<>% left_join(so2_defaults) %>% left_join(nox_defaults) %>% left_join(pm_defaults) %>% 
  mutate(SOx = na.cover(SOx, SOx_default),
         NOx = na.cover(NOx, NOx_default),
         PM  = na.cover(PM,  PM_default))

emis %>% 
  mutate(Hg..t=NA, CO2..t=NA) %>% 
  pivot_longer(matches('SOx$|NOx$|PM$|\\.\\.t', ignore.case=F)) %>% 
  mutate(pollutant = disambiguate(name, c('SOx', 'NOx', 'PM', 'Hg', 'CO2')),
         name=ifelse(grepl('\\.\\.t', name), 'emissions_t', 'mg.Nm3')) %>% 
  pivot_wider() %>% 
  mutate(
    #add thermal efficiency
    thermal_efficiency = case_when(!is.na(thermal_efficiency)~thermal_efficiency,
                                   MW<100 ~ .3,
                                   grepl('SC|^supercritical', Steam.Tech.) ~ .42,
                                   grepl('USC|ultrasuper', Steam.Tech.) ~ .44,
                                   COD>=2010~.38,
                                   T~.35),
    #add utilization
    base_utilization = case_when(Owner=='captive'~.8, T~base_utilization$value[base_utilization$source=='Coal']*utilization_uplift),
    #add SFGV
    sfgv=354*(21-6)/(21-7),
    fuel_input_GJ=MW * base_utilization / thermal_efficiency * 8760 * 3.6,
    fuel_kcal=ifelse(grepl('MM', Steam.Tech.), 4000, 5000),
    hg_capture = case_when(grepl('SCR', APC)~.25,
                           grepl('FGD', APC)~.4,
                           T~.1),
    emissions_t = case_when(!is.na(emissions_t)~emissions_t,
                            pollutant=='CO2'~fuel_input_GJ*94.6/1000,
                            pollutant=='Hg'~fuel_input_GJ/29.3*(7000/fuel_kcal) * Hg.in.coal..ppb/1e9 * (1-hg_capture),
                            T~fuel_input_GJ * sfgv * mg.Nm3 / 1e9),
    GWh = MW*base_utilization*8.76,
    emissions_t_per_GWh = emissions_t / GWh
  ) -> emis_long

#remove cancelled and shelved units; postpone units not yet under construction but COD that is "too soon"
#PLN construction not in RUPTL
emis_long %<>% #filter(Status %notin% c('shelved', 'cancelled', 'retired') | !is.na(COD_RUPTL.2025)) %>% 
  mutate(COD = case_when(Status %in% c('pre-permit', 'planned')~pmax(COD, year(today())+5),
                         Status=='permitted'~pmax(COD, year(today())+4),
                         Status=='announced'~pmax(COD, year(today())+6),
                         Status=='construction'~pmax(COD, year(today())),
                         T~COD))


#add power grid areas for retirement
emis_long %<>% 
  mutate(region = case_when(grepl('Java|Jawa|Sumate?ra|Bali|Jambi|Aceh|Riau$|Lampung|Jakarta|Belitung|Banten|Bengkulu', province)~'Java-Bali-Sumatra',
                          grepl('Kalimantan', province)~'Kalimantan',
                          grepl('Sulawesi|Gorontalo', province)~'Sulawesi',
                          T~"Others"))
  
emis_long %<>% group_by(region, province) %>% 
  group_modify(function(df, group) {
    df %>% mutate(grid = case_when(group$region=='Others'~paste(group$province, cluster(df, 10)),
                                   T~group$region))
  })
   
#add health impact per GWh for prioritization
emissions_dir_old <- "G:/IndonesiaIESR/emissions"
#emissions_dir <- "G:/Shared drives/CREA-HIA/Projects/Indonesia_JETP/"

#read clusters
clusters <- read_csv(file.path(emissions_dir, 'RUPTL emissions by unit, with cluster.csv')) %>% to_sf_points() %>% 
  mutate(cluster=tolower(emission_names))

#add clustering
emis_long %>% ungroup %>% distinct(CFPP.name, Latitude, Longitude) %>% 
  #group_by(CFPP.name, Latitude, Longitude) %>% summarise(MW=sum(MW), n=n()) %>% ungroup %>% 
  to_sf_points() %>% 
  mutate(cluster=clusters$cluster[st_nearest_feature(., clusters)],
         distance_to_cluster=st_distance(., clusters) %>% apply(1, min)) ->
  clustering

clustering %<>% left_join(clusters %>% st_drop_geometry() %>% distinct(cluster, emission_names))

clusters$emission_names %>% force_numeric() %>% max -> last_cluster_no
clustering %<>% mutate(is_new_cluster=distance_to_cluster>5e3)

clustering %<>% filter(is_new_cluster) %>% 
  group_by(new_cluster_number=cluster(., 1)) %>% 
  mutate(emission_names=paste0(CFPP.name[1] %>% gsub(' ', '', .) %>% substr(1,5) %>% 
                                 paste0(last_cluster_no+new_cluster_number)),
         cluster=tolower(emission_names)) %>% 
  ungroup %>%  
  bind_rows(clustering %>% filter(!is_new_cluster)) %>% 
  select(-new_cluster_number, -distance_to_cluster)

emis_long %<>% left_join(clustering %>% st_drop_geometry())

emis_long %>% write_csv(file.path(emissions_dir, 'RUPTL emissions by unit, with cluster, July 2026 update.csv'))

#add stack information
project_dir=emissions_file
output_dir=project_dir
source("project_workflows/Indonesia_RUPTL2025/indonesia_RUPTL_export_CALPUFF_inputs.R")


####start from here once CALPUFF is run
emis_long <- read_csv(file.path(emissions_dir, 'RUPTL emissions by unit, with cluster, July 2026 update.csv'))

source('~/creahia/project_workflows/Indonesia_RUPTL/do_hia_function.R')
hia_plants <- emis_long %>% mutate(year=2025, utilization=base_utilization, scenario='base_year') %>% 
  do_hia(project_dir='G:/IndonesiaIESR/RUPTL2025')


hia_plants %>% 
  group_by(CFPP.name, estimate) %>% summarise(across(ends_with('per_TWh'), sum)) %>% 
  filter(estimate=='central') %>% 
  left_join(emis_long, .) -> emis_long

emis_long %>% write_csv(file.path(emissions_dir, 'RUPTL HIA by unit, with cluster, July 2026 update.csv'))

emis_long <- read_csv(file.path(emissions_dir, 'RUPTL HIA by unit, with cluster, July 2026 update.csv'))

prioritize_retirement <- function(plantdata) {
  plantdata %>% ungroup %>% distinct(CFPP.name, .keep_all=T) %>% arrange(cost_mn_currentUSD_per_TWh)
  #plantdata %>% filter(pollutant=='SOx', year==2022) %>% select(-year) %>% arrange(emissions_t_per_GWh)
}


retire_plants <- function(plantdata, traj) {
  plantdata %>% 
    group_modify(function(df, group) {
      message(group)
      
      df %>% prioritize_retirement() -> plantdata_prioritized
      
      for(yr in rev(sort(traj$year))) {
        operating <- plantdata_prioritized$COD<=yr & plantdata_prioritized$year_retire>=yr & plantdata_prioritized$eligible_for_retirement
        current_mw = plantdata_prioritized$MW[operating] %>% sum
        if(!is.null(traj$target_mw)) target_mw <- traj$target_mw[traj$year==yr]
        if(!is.null(traj$delta_mw)) target_mw <- current_mw + traj$delta_mw[traj$year==yr]
        if(!is.null(traj$delta_percent)) target_mw <- current_mw * (1 + traj$delta_percent[traj$year==yr])
        
        plantdata_prioritized$year_retire[operating][cumsum(plantdata_prioritized$MW[operating])>target_mw] <- yr
      }
      
      df %>% select(-year_retire) %>% left_join(plantdata_prioritized %>% select(CFPP.name, year_retire))
    })
}

#pull BAU and ADB retirement years from old data
read_xlsx('~/CALPUFF/creapuff/project_workflows/IndonesiaIESR/data/MASTERLIST_Indonesia Coal.xlsx', sheet='PLN & IPP',
          skip=1) %>% set_names(make.names(names(.))) %>% select(CFPP.name, GEM.ID, contains('retire')) ->
  retirement_years

emis_long %<>% left_join(retirement_years)

#add BAU retirement
emis_long %<>% mutate(earliest_retire=round(2042 + (COD-2022)/5, 0),
                      year_retire = case_when(!is.na(BAU.retire)~BAU.retire,
                                              Owner=='captive'~pmax(COD+30, earliest_retire), 
                                              T~pmax(COD+30, earliest_retire)), 
                      scenario="RUPTL 2025")

#add cancellation of new plants in RUPTL
emis_long %<>% filter(scenario=='RUPTL 2025') %>% 
  mutate(COD=case_when(In.RUPTL.2025.2034=='Yes' & COD_RUPTL.2025>2026~Inf, T~COD),
         scenario='RUPTL 2025 with new plants cancelled') %>% 
  bind_rows(emis_long)

emis_long %<>% filter(scenario=='RUPTL 2025') %>% 
  mutate(COD=case_when(Owner=='captive' & Status.July.2026 %in% c('permitted', 'pre-permit', 'announced') ~Inf, T~COD),
         scenario='RUPTL 2025 with new captive plants replaced') %>% 
  bind_rows(emis_long)

#expand to all projection years
years <- emis_long$year_retire %>% subset(is.finite(.)) %>% max %>% add(2) %>% seq(2000, ., 1)

emis_long %<>% cross_join(tibble(year=years))

read_xlsx(file.path(emissions_dir, 'projections_100GW_solar.xlsx'), sheet='final') %>% 
  pivot_longer(matches("^[0-9]{4}"), names_to='year') %>% 
  set_names(make_names(names(.))) %>% 
  mutate(across(year, as.numeric)) -> proj_100GW


proj_100GW %>% filter(unit=='MtCO2', scenario=='RUKN', category=='on-grid') %>% 
  mutate(emissions_projected_t=value*1e6) %>% 
  select(year, emissions_projected_t) -> 
  rukn_traj

emis_long %>% filter(pollutant=='CO2', COD<=year, year_retire>=year, Owner!='captive') %>% 
  group_by(scenario, year) %>% summarise(across(emissions_t, sum)) -> bottomup_traj

rukn_traj %<>% full_join(bottomup_traj) %>% 
  arrange(year) %>% group_by(scenario) %>% 
  mutate(adj=emissions_projected_t/emissions_t) %>% 
  fill(adj, .direction='updown')

rukn_traj %>% ggplot(aes(year, col=scenario)) + geom_line(aes(y=adj))
rukn_traj %>% ggplot(aes(year, col=scenario)) + geom_line(aes(y=emissions_t)) + geom_line(aes(y=emissions_projected_t), linetype='dotted')

#add adjustment for captive plants after 2050
proj_100GW %>% filter(unit=='MtCO2', scenario=='RUKN', category=='industrial captive') %>% 
  mutate(emissions_projected_t=value*1e6) %>% 
  select(year, emissions_projected_t) -> 
  rukn_captive_traj

emis_long %>% filter(pollutant=='CO2', COD<=year, year_retire>=year, Owner=='captive') %>% 
  group_by(year) %>% summarise(across(emissions_t, sum)) -> bottomup_captive_traj

rukn_captive_traj %<>% full_join(bottomup_captive_traj) %>% 
  arrange(year) %>% group_by() %>% 
  mutate(emissions_projected_t=emissions_projected_t*emissions_t[year==2050]/emissions_projected_t[year==2050],
         captive_adj=ifelse(year<2050, 1, emissions_projected_t/emissions_t)) %>% 
  fill(captive_adj, .direction='updown')

#add new plants replaced scenario
emis_long %<>% filter(scenario=='RUPTL 2025 with new plants cancelled') %>% mutate(scenario='RUPTL 2025 with new plants replaced') %>% 
  bind_rows(emis_long)

rukn_traj %<>% filter(scenario=='RUPTL 2025') %>% mutate(scenario='RUPTL 2025 with new plants replaced') %>% 
  bind_rows(rukn_traj)


emis_long %<>% 
  left_join(rukn_traj %>% select(scenario, year, adj)) %>% 
  left_join(rukn_captive_traj %>% select(year, captive_adj)) %>% 
  mutate(adj=ifelse(Owner=='captive', captive_adj, adj),
         across(c(emissions_t, GWh), ~.x*adj),
         category=ifelse(Owner=='captive', 'industrial captive', 'utility'))


emis_long %>% filter(pollutant=='CO2', COD<=year, year_retire>=year) %>% 
  group_by(scenario, category, year) %>% summarise(across(emissions_t, sum)) %>% ggplot(aes(year, col=scenario)) + geom_line(aes(y=emissions_t)) + facet_wrap(~category)

#create solar scenario by subtracting solar generation from each type of coal plant
proj_100GW %>% filter(energy_source=='solar', unit=='TWh') %>% 
  group_by(scenario) %>% mutate(category=case_when(category %in% c('on-grid', 'small scale') | all(category=='total')~'utility',
                                                   T~category)) %>% 
  filter(category!='total') %>% 
  group_by(category, year, scenario) %>% 
  summarise(across(value, sum)) %>% 
  summarise(solar_delta_TWh=value[scenario=='100GW solar']-if_null(value[scenario=='RUKN'], 0)) ->
  solar_delta

solar_delta %>% ggplot(aes(year, solar_delta_TWh, color=category)) + geom_line()

emis_long %>% filter(pollutant=='CO2', COD<=year, year_retire>=year) %>% 
  group_by(scenario, category, year) %>% 
  summarise(across(GWh, sum)) %>% 
  full_join(solar_delta) %>% 
  mutate(solar_adj=pmax(0, 1-solar_delta_TWh*1000/GWh)) %>% 
  fill(solar_adj, .direction='downup') ->
  solar_delta

solar_delta %>% ggplot(aes(year, solar_adj, color=category, linetype=scenario)) + geom_line()

emis_long %<>% 
  left_join(solar_delta %>% select(scenario, category, year, solar_adj)) %>% 
  mutate(scenario=scenario %>% gsub('RUPTL 2025', '100GW solar', .),
         across(c(emissions_t, GWh), ~.x*solar_adj)) %>% 
  bind_rows(emis_long)

emis_long %<>% filter(scenario %notin% c('RUPTL 2025 with new plants cancelled', '100GW solar with new plants replaced'))

#calculate effect of co-firing on emissions
#https://github.com/energyandcleanair/eu_ied_review/blob/master/outputs/cofiring_analysis/cofiring%20impact%20on%20emissions.csv
cofiring_effect = read_csv('~/CALPUFF/creapuff/inst/extdata/cofiring impact on emissions.csv') %>% 
  mutate(biomass_effect=pmax(biomass_effect, 1-biomass_share))


get_cofiring_effect <- function(df) {
  df %>% mutate(pollutant_ird = recode(pollutant, PM='DUST', NOx='NOX', SOx='SO2')) %>% 
    group_by(pollutant, pollutant_ird) %>% 
    group_modify(function(df, group) {
      cofiring_effect %>% filter(pollutantCode==group$pollutant_ird) -> effect_poll
      if(nrow(effect_poll)==0) cofiring_effect %>% filter(pollutantCode=='DUST') %>% mutate(biomass_effect=1-biomass_share) -> effect_poll
      df %>% mutate(cofiring_effect = approx(effect_poll$biomass_share, effect_poll$biomass_effect, df$cofiring_share)$y)
    }) %>% ungroup
}

get_ccs_effect <- function(df) {
  df %>% mutate(ccs_effect = case_when(pollutant=='PM'~0.461546678,
                                       pollutant=='NOx'~0.71,
                                       pollutant=='SOx'~0.15,
                                       pollutant=='CO2'~0.15,
                                       pollutant=='Hg'~0.5),
                ccs_effect = 1 - (1 - ccs_effect) * ccs_share)
}


emis_long %<>% mutate(cofiring_target_date = case_when(!is.na(cofiring_target_date)~cofiring_target_date,
                                                       !is.na(cofiring_future_target)~median(cofiring_target_date, na.rm=T)),
                      cofiring_share = case_when(!is.na(cofiring_future_target) & year>cofiring_target_date~cofiring_future_target,
                                                 year<=cofiring_date~0,
                                                 !is.na(cofiring_share)~cofiring_share, T~0))

#add the effect of replacing cofiring with clean energy
include_no_cofiring_scenarios=T
include_APC_scenarios=T
if(include_no_cofiring_scenarios) {
  emis_long %<>% filter(scenario=='RUPTL 2025') %>% mutate(scenario='RUPTL 2025 no cofiring', cofiring_share=0) %>% bind_rows(emis_long)
}

#assume the further increase of cofiring won't happen because of supply constraints
if(F) {
  emis_long %<>% 
    mutate(cofiring_share = case_when(Owner=='PLN' & grepl('PERPRES|JETP|RUPTL', scenario) & cofiring_share>0 ~ 
                                        pmax(approx(c(2025,2030,2035), c(0,.1,.2), year, rule=2)$y, 
                                             cofiring_share, na.rm = T),
                                      scenario=='BAU no cofiring'~0,
                                      scenario=='PERPRES 112/2022 no additional cofiring' & 
                                        !grepl('implemented', cofiring_status)~0,
                                      T~cofiring_share))
}

emis_long %<>% get_cofiring_effect
emis_long %<>% mutate(emissions_t = emissions_t * cofiring_effect) %>% ungroup


#add the effect of CCS on emissions
emis_long %<>% mutate(ccs_share=0)
emis_long %<>% get_ccs_effect %>% mutate(emissions_t = emissions_t * ccs_effect)

if(include_APC_scenarios) {
  emis_long %<>% filter(!grepl('APC', scenario)) %>% select(-any_of('mg.Nm3_retro'))
  
  #calculate effect of retrofitting on emissions
  retrofit_mg <- tibble(pollutant=c('SOx', 'NOx', 'PM'),
                        mg.Nm3_retro=c(130, 150, 10))
  
  emis_long %<>% mutate(retrofit_year=Inf)
  
  emis_long %<>% filter(scenario %in% c('100GW solar', '100GW solar with new plants cancelled')) %>% 
    left_join(retrofit_mg) %>% 
    mutate(retrofit_year = case_when(COD>=2027~2026,
                                     year_retire>2040~2035, T~Inf),
           is_retrofit = retrofit_year==2035) %>% 
    group_by(scenario, is_retrofit) %>% 
    mutate(retrofit_lead = -round(5*rank(-cost_mn_currentUSD_per_TWh)/length(cost_mn_currentUSD_per_TWh),0),
           retrofit_year = retrofit_year + ifelse(retrofit_year==2035, retrofit_lead, 0)) %>% 
    ungroup %>% select(-is_retrofit, -retrofit_lead) %>% 
    mutate(emissions_t = emissions_t * case_when(pollutant=='CO2' | year<=retrofit_year~1,
                                                 pollutant=='Hg' & year>retrofit_year ~ (1-.75) / (1 - hg_capture),
                                                 T~pmin(1, mg.Nm3_retro/mg.Nm3)),
           scenario=paste(scenario, '/w APC')) %>% 
    bind_rows(emis_long)
  
  #calculate newbuild and retrofit APC capacity
  emis_long %>% filter(grepl('APC', scenario), is.finite(retrofit_year), is.finite(COD), year_retire>2035) %>% 
    distinct(CFPP.name, scenario, .keep_all=T) %>% 
    group_by(scenario, is_retrofit = retrofit_year!=2026, retrofit_year) %>% 
    summarise(across(c(MW_fitted_w_APC=MW), sum)) %>% 
    mutate(cost_mn_USD = MW_fitted_w_APC * ifelse(is_retrofit, 204, 152)) %>% 
    summarise(across(where(is.numeric), sum))
  
  emis_long %>% filter(scenario=='100GW solar', year %in% c(2012,2022,2030), 
                       COD<=year, year_retire>=year) %>% 
    group_by(pollutant, year) %>% summarise(across(emissions_t, sum)) %>% 
    mutate(change=emissions_t/lag(emissions_t)-1)
}

emis_long %>% 
  filter(COD<=year, year_retire>=year,
         !grepl('APC|cofiring|captive', scenario),
         pollutant=='CO2') %>% 
  group_by(scenario, pollutant, Owner=ifelse(Owner=='captive', 'captive', 'on-grid'), year) %>% 
  summarise(across(emissions_t, sum)) %>% 
  ggplot(aes(year, emissions_t, color=scenario)) + geom_line() + facet_wrap(~Owner, scales='free_y')

#avoided emissions from 100GW solar
emis_long %>% 
  filter(COD<=year, year_retire>year) %>% 
  group_by(pollutant, year, scenario) %>% 
  summarise(across(emissions_t, sum)) %>% 
  mutate(delta=emissions_t[scenario=='RUPTL 2025'] - emissions_t) %>% 
  ggplot(aes(year, delta, col=scenario)) + geom_line() + facet_wrap(~pollutant, scales='free_y')


#export base year emissions for modeling
#source('project_workflows/indonesia_iesr_export_CALPUFF_inputs.R')

emis_long %<>% filter(COD<=year, year_retire>=year)


#export emissions pathways
emis_long %<>% filter(Status=='operating' | year>2025,
                      scenario %in% c('100GW solar with new plants cancelled',
                                      '100GW solar with new plants cancelled /w APC',
                                      'RUPTL 2025', 'RUPTL 2025 with new plants replaced')) %>%
  mutate(scenario=recode(scenario,
                         '100GW solar with new plants cancelled'='100GW solar',
                         '100GW solar with new plants cancelled /w APC'='100GW solar /w APC',
                         'RUPTL 2025 with new plants replaced'='RUPTL 2025 without new plants'))
  

emis_long %>% 
  select(pollutant, scenario, Owner, grid, region, province, Latitude, Longitude, CFPP.name, MW, GWh, GEM.ID,
         cluster, Status, In.RUPTL.2025.2034, COD, year_retire, year, emissions_t) %>% 
  saveRDS(file.path(emissions_dir, 'indonesia_RUPTL_emission_pathways_October_2026.RDS'))


emis_long %>% distinct(CFPP.name, GEM.ID, province, grid, scenario, COD, year_retire, MW) %>% 
  write_csv(file.path(emissions_dir, 'plant_retirement_October_2026.csv'))

#capacity pathways
require(rcrea)
require(ggrepel)

emis_long %>% ungroup %>% distinct(scenario, year, CFPP.name, .keep_all=T) %>% 
  filter(!grepl('APC|cofiring', scenario), 
         !grepl('1\\.5', scenario) | Owner=='captive' | year<=2040,
         !grepl('1\\.5 degrees$', scenario) |  year<=2040) %>% 
  group_by(scenario, year) %>% summarise(across(c(MW, GWh), sum)) %>% ungroup %>% 
  pivot_longer(c(MW, GWh)) %>% complete(scenario, year=2000:2060, name, fill=list(value=0)) %>% 
  #filter(year %in% seq(2010,2060, 5)) %>% 
  group_by(scenario, name) %>% arrange(year) %>% 
  mutate(value=zoo::rollapply(value, 3, FUN=function(x) ifelse(x[2]==0, 0, mean(x)), align='center', fill=0),
         name=ifelse(name=='MW', 'GW', 'TWh')) %>% 
  filter(year>=2010, name=='GW') -> cap_plot

label_df <- cap_plot %>% group_by(scenario) %>% filter(year==min(2046, max(year[value>0])))

cap_plot %>% write_csv(file.path(output_dir, 'Operating coal-fired capacity by scenario.csv')) %>% 
  ggplot(aes(year, value/1e3, col=scenario)) + 
  geom_line(linewidth=1) +
  #facet_wrap(~name, scales='free_y') +
  theme_crea_new() + 
  scale_color_crea_d('dramatic', guide='none') +
  geom_label_repel(data=label_df, aes(label=str_wrap(scenario, 15)), xlim=c(2056, 2060), ylim=c(12,NA), hjust=0, size=rel(2)) +
  x_at_zero() +
  scale_x_continuous(expand=expansion(mult=c(0,.1))) +
  labs(title='Operating coal-fired capacity by scenario',
       y='GW', x='')
quicksave(file.path(output_dir, 'Operating coal-fired capacity by scenario.png'), scale=1)

cap_plot %>% filter(year==2022 | value==max(value)) %>% arrange(scenario)

#calculate total emissions in future pathways
emis_long %>% 
  #mutate(scenario=gsub('JET-P', 'Proposed JET-P target', scenario)) %>% 
  group_by(pollutant, scenario, year) %>% summarise(across(emissions_t, sum)) %>% ungroup %>% 
  complete(pollutant, scenario, year, fill=list(emissions_t=0)) -> emis_byyear

plot_lines <- function(df, title='', guide='none', height=10) {
  df %<>% filter(year>=2010) %>% mutate(scenario = scenario %>% gsub('Investment ', '', .) %>% gsub('excluding', 'excl.', .) %>% 
                                          gsub(' degrees', '°C', .),
                                        emissions_t = emissions_t * case_when(pollutant=='CO2'~1e-6,
                                                                              pollutant=='Hg'~1e3,
                                                                              T~1e-3),
                                        pollutant = paste0(pollutant, ', ', 
                                                           case_when(pollutant=='CO2'~'Mt/year',
                                                                     pollutant=='Hg'~'kg/year',
                                                                     T~'kt/year')))
  
  label_df <- df %>% filter(year==2046)
  ggplot(mapping=aes(year, emissions_t, col=scenario, label=str_wrap(scenario, 20))) + 
    geom_line(data=df, linewidth=1) + facet_wrap(~pollutant, ncol=1, scales='free_y') +
    theme_crea_new() + 
    x_at_zero() +
    scale_color_crea_d(guide=guide) +
    labs(title=title, y='kt/year', x='') +
    scale_x_continuous(expand=expansion(mult=c(0,.15))) -> p
  
  if(guide=='none') p <- p + geom_text_repel(data=label_df, xlim=c(2057, 2060))
  
  quicksave(file.path(output_dir, paste0(title, '.png')), plot=p, scale=1.33, height=height)
  df %>% write_csv(file.path(output_dir, paste0(title, '.csv')))
}

emis_byyear %>% filter(!grepl('cofiring', scenario)) %>% plot_lines('Emissions by scenario')


plot_cols <- function(df, title='') {
  df %>% filter(year>2025) %>% group_by(pollutant, scenario) %>% summarise(across(emissions_t, sum, na.rm=T)) %>% 
    write_csv(file.path(output_dir, paste0(title, '.csv'))) %>% 
    ggplot(aes(scenario, emissions_t/1e6, fill=scenario)) + geom_col() + 
    facet_wrap(~pollutant, scales='free_y') +
    theme_crea_new() + 
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    scale_fill_crea_d(guide='none') + x_at_zero() +
    labs(title=title, subtitle='2026 until end-of-life of all plants', y='Mt', x='') -> p
  quicksave(file.path(output_dir, paste0(title, '.png')), plot=p, scale=1.33)
}

emis_byyear %>% filter(!grepl('cofiring', scenario)) %>% plot_cols('Cumulative emissions by scenario')
#emis_byyear %>% filter(grepl('1\\.5 degrees|2022$|APC', scenario)) %>% plot_cols('Effect of APC on emissions, cumulative')
#emis_byyear %>% filter(grepl('BAU$|2022$|cofiring', scenario)) %>% plot_cols('Effect of co-firing on emissions, cumulative')

require(sf)
require(ggspatial)
require(ggmap)
readLines('~/google_api_key.txt') %>% register_google()

map_bb <- emis_long %>% to_spdf() %>% extent %>% multiply_by(1.1) %>% as.matrix() %>% as.vector
basemap <- ggmap::get_map(map_bb, zoom=4)
emis_long %>% filter(is.finite(COD)) %>% 
  mutate(Status = case_when(Status %in% c('operating', 'construction')~Status, T~'planned')) %>% 
  group_by(Longitude, Latitude, pollutant, Status) %>% summarise(across(emissions_t, sum)) %>% 
  st_as_sf(coords=c('Longitude', 'Latitude')) %>% st_set_crs(4326) -> emis_sf

ggmap(basemap) + 
  layer_spatial(emis_sf, aes(size=emissions_t, col=Status), alpha=.5) + facet_wrap(~pollutant, ncol=1) +
  coord_sf(xlim=map_bb[c(1,3)], ylim=map_bb[c(2,4)])

emis_long %>% distinct(CFPP.name, .keep_all = T) %>% 
  select(Tracker.ID, CFPP.name, Longitude, Latitude, Owner, Status, COD) %>% 
  st_as_sf(coords=c('Longitude', 'Latitude')) %>% st_set_crs(4326) %>% 
  st_write(file.path(output_dir, 'all plants.kml'))
