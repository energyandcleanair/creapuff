require(terra)
require(sf)
require(raster)
require(tidyverse)
require(magrittr)
require(lubridate)
require(readxl)
require(creapuff)
require(creahelpers)
require(rcrea)

emissions_dir='project_workflows/IndonesiaIESR/data'
emissions_file=file.path(emissions_dir, 'MASTERLIST_Indonesia CFPPs - PLN & IPP and captive.xlsx')
gcpt_file=file.path(emissions_dir, 'Global-Coal-Plant-Tracker-January-2025.xlsx')

project_dir='G:/Shared drives/CREA-HIA/Projects/Indonesia_JETP'
output_dir=project_dir

gis_dir = '~/GIS'

source('project_workflows/Indonesia_RUPTL2025/indonesia_RUPTL_read_plantdata.R')

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
emis_long %<>% filter(Status %notin% c('shelved', 'cancelled', 'retired') | !is.na(COD_RUPTL.2025)) %>% 
  mutate(COD = case_when(Status %in% c('pre-permit', 'planned')~pmax(COD, year(today())+5),
                         Status=='permitted'~pmax(COD, year(today())+4),
                         Status=='announced'~pmax(COD, year(today())+6),
                         Status=='construction'~pmax(COD, year(today())),
                         T~COD))

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
#emissions_dir <- "G:/IndonesiaIESR/emissions"
project_dir <- "G:/Shared drives/CREA-HIA/Projects/Indonesia_JETP"
emissions_dir <- "G:/Shared drives/CREA-HIA/Projects/Indonesia_JETP/emissions"

#read clusters
clusters <- read_csv(file.path(emissions_dir, 'emissions, clustered, with missing sources.csv')) %>% to_sf_points() %>% 
  mutate(cluster=tolower(emission_names))

#add clustering
emis_long %>% ungroup %>% distinct(CFPP.name, Latitude, Longitude) %>% to_sf_points() %>% 
  mutate(cluster=clusters$cluster[st_nearest_feature(., clusters)],
         distance_to_cluster=st_distance(., clusters) %>% apply(1, min)) ->
  clustering

emis_long %<>% left_join(clustering)
emis_long %>% write_csv(file.path(emissions_dir, 'RUPTL emissions by unit, with cluster.csv'))

if(!exists('hia_plants')) {
  source('../creahia/project_workflows/Indonesia_RUPTL/do_hia_function.R')
  hia_plants <- emis_long %>% mutate(year=2024, utilization=base_utilization, scenario='base_year') %>% 
    do_hia(project_dir=project_dir)
}

hia_plants %>% 
  group_by(CFPP.name, estimate) %>% summarise(across(ends_with('per_TWh'), sum)) %>% 
  filter(estimate=='central') %>% 
  left_join(emis_long, .) -> emis_long

#TODO: scenarios: RUPTL capacity additions, captive additions, RUPTL vs JET-P and 1p5 generation
#pull BAU and ADB retirement years from old data
read_xlsx('project_workflows/IndonesiaIESR/data/MASTERLIST_Indonesia Coal.xlsx', sheet='PLN & IPP',
          skip=1) %>% set_names(make.names(names(.))) %>% select(CFPP.name, GEM.ID, contains('retire')) ->
  retirement_years

emis_long %<>% left_join(retirement_years)

#TODO: add gas emissions
#G:/Shared drives/CREA-data/Global Coal Plant Tracker/non_coal/Global-Oil-and-Gas-Plant-Tracker-GOGPT-August-2023.xlsx

#add BAU retirement
emis_long %<>% mutate(earliest_retire=round(2042 + (COD-2022)/5, 0),
                      year_retire = case_when(!is.na(BAU.retire)~BAU.retire,
                                              Owner=='captive'~pmax(COD+30, earliest_retire), 
                                              T~pmax(COD+30, earliest_retire)), 
                      scenario="RUPTL 2025-2034 - RE Base")

#expand to all projection years
years <- emis_long$year_retire %>% subset(is.finite(.)) %>% max %>% add(2) %>% seq(2000, ., 1)

emis_long %<>% cross_join(tibble(year=years))

#add ACT scenario
#emis_long %<>% mutate(year_retire = case_when(!is.na(ADB_retire)~ADB_retire, T~year_retire), scenario='ACT Investment Plan') %>% 
#  bind_rows(emis_long)

perpres_traj <- tibble(year=2040:2051) %>% mutate(delta_percent=-approx(c(2040,2051), c(0,1), year)$y^2)
perpres_captive_traj <- tibble(year=2055:2059) %>% mutate(delta_percent=-approx(c(2055,2060), c(0,1), year)$y^2)

emis_long %<>% mutate(status_category = case_when(grepl('oper|constr|permitted|existing', Status)~'existing',
                                                  T~'new'))

#add PERPRES scenario
emis_long %<>% filter(grepl('RUPTL 2025', scenario)) %>% 
  mutate(eligible_for_retirement = Owner != 'captive') %>% 
  group_by(grid) %>% 
  retire_plants(traj=perpres_traj) %>% 
  mutate(eligible_for_retirement = Owner == 'captive' & status_category=='existing') %>% 
  retire_plants(traj=perpres_captive_traj) %>% 
  mutate(year_retire = case_when(Owner=='captive' & status_category=='new' & year_retire > 2050 ~ 2050, T~year_retire)) %>% 
  mutate(scenario='PERPRES 112/2022') %>% 
  bind_rows(emis_long)


#add JETP capacity scenario
emis_long %<>% mutate(new_in_ruptl=!is.na(In.RUPTL.2025.2034) & In.RUPTL.2025.2034=='Yes' & COD>2024 & Status!='operating')

emis_long %<>% filter(scenario=='PERPRES 112/2022') %>% 
  mutate(COD=ifelse(new_in_ruptl, Inf, COD),
         scenario='JETP CIPP') %>% 
  bind_rows(emis_long)


#add 1.5 degree scenario
traj_1.5 <- coal_gen %>% ungroup %>% 
  filter(scenario=='1p5', year>=2025) %>% 
  mutate(delta_percent=GW/GW[year==2025]-1) %>% 
  full_join(tibble(year=2025:2070), .) %>% 
  mutate(delta_percent = approx(year, delta_percent, year, rule=1)$y) %>% 
  fill(delta_percent)

emis_long %<>% filter(scenario=='JETP CIPP') %>% 
  mutate(eligible_for_retirement = Owner != 'captive',
         COD = ifelse(Status %in% c('operating', 'construction') | !eligible_for_retirement, COD, Inf)) %>% 
  group_by(grid) %>% 
  retire_plants(traj=traj_1.5) %>% 
  mutate(eligible_for_retirement = Owner == 'captive' & status_category == 'existing') %>% 
  retire_plants(traj=jetp_captive_traj) %>% 
  mutate(scenario='1.5 degrees excluding captive') %>% 
  bind_rows(emis_long)

#add 1.5 degree with captive scenario
emis_long %<>% filter(scenario=='JETP CIPP') %>% 
  mutate(eligible_for_retirement = T,
         COD = ifelse(Status %in% c('operating', 'construction') | !eligible_for_retirement, COD, Inf)) %>% 
  group_by(grid) %>% 
  retire_plants(traj=traj_1.5) %>% 
  mutate(scenario='1.5 degrees') %>% 
  bind_rows(emis_long)

#calculate utilization adjustment based on pathway
coal_gen %<>% mutate(scenario=recode(scenario, "1p5"="1.5 degrees"))
emis_long %<>% mutate(source='Coal')

coal_gen %>% 
  filter(year>=2024, year<=case_when(grepl('2034', scenario)~2034, grepl('JETP', scenario)~2040, T~Inf)) %>% 
  group_by(scenario, source) %>% mutate(generation_change=EJ/EJ[year==2024]) %>%
  select(source, year, generation_change) ->
  coal_gen_changes

emis_long %>% filter(pollutant=='SOx', year_retire>year, COD<year, Owner!='captive') %>% 
  group_by(scenario, source, year) %>% summarise(across(GWh, sum)) %>% 
  mutate(generation_change_bottom_up=GWh/GWh[year==2024]) %>% 
  inner_join(coal_gen_changes) %>% 
  mutate(generation_change=generation_change %>% zoo::na.approx(na.rm=F, rule=1),
         utilization_adjustment=(generation_change/generation_change_bottom_up)) %>% 
  fill(utilization_adjustment, .direction='down') %>% 
  select(scenario, source, year, utilization_adjustment) ->
  utilization_adjustment

utilization_adjustment %<>% filter(grepl('RUPTL 2025', scenario)) %>% 
  mutate(scenario='PERPRES 112/2022') %>% 
  bind_rows(utilization_adjustment)

emis_long %<>% left_join(utilization_adjustment) %>% 
  mutate(utilization=base_utilization * case_when(Owner=='captive' | is.na(utilization_adjustment)~1, T~utilization_adjustment),
         utilization=pmin(utilization, .9))

emis_long %<>% left_join(coal_gen %>% select(scenario, source, year, ccs_share)) %>% 
  group_by(scenario, source) %>% 
  mutate(ccs_share=ccs_share %>% zoo::na.approx(rule=2, na.rm=F)) %>% 
  replace_na(list(ccs_share=0))

emis_long %<>% mutate(across(c(emissions_t, GWh), ~.x * utilization / base_utilization))

#add cancellation of new plants in RUPTL
emis_long %<>% filter(grepl('RUPTL 2025', scenario)) %>% 
  mutate(COD=ifelse(new_in_ruptl, Inf, COD),
         scenario='without new plants included in 2025 RUPTL') %>% 
  bind_rows(emis_long)

#calculate effect of co-firing on emissions
cofiring_effect = read_csv('../eu_ied_review/outputs/cofiring_analysis/cofiring impact on emissions.csv') %>% 
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
                                                 year<=year(cofiring_date)~0,
                                                 !is.na(cofiring_share)~cofiring_share, T~0))

#add the effect of replacing cofiring with clean energy
include_no_cofiring_scenarios=F
include_APC_scenarios=F
if(include_no_cofiring_scenarios) {
  emis_long %<>% filter(scenario=='BAU') %>% mutate(scenario='BAU no cofiring') %>% bind_rows(emis_long)
  emis_long %<>% filter(scenario=='PERPRES 112/2022') %>% 
    mutate(scenario='PERPRES 112/2022 no additional cofiring') %>% bind_rows(emis_long)
}

emis_long %<>% 
  mutate(cofiring_share = case_when(Owner=='PLN' & grepl('PERPRES|JETP|RUPTL', scenario) & cofiring_share>0 ~ 
                                      pmax(approx(c(2025,2030,2035), c(0,.1,.2), year, rule=2)$y, 
                                           cofiring_share, na.rm = T),
                                    scenario=='BAU no cofiring'~0,
                                    scenario=='PERPRES 112/2022 no additional cofiring' & 
                                      !grepl('implemented', cofiring_status)~0,
                                    T~cofiring_share))

if(include_no_cofiring_scenarios) {
  emis_long %<>% filter(scenario=='PERPRES 112/2022') %>% 
  mutate(scenario='PERPRES 112/2022 no cofiring',
         cofiring_share=0) %>% 
  bind_rows(emis_long)
}

emis_long %<>% get_cofiring_effect

if(include_no_cofiring_scenarios) {
  emis_long %<>% 
    group_by(CFPP.name, pollutant, year) %>% 
    mutate(cofiring_effect = cofiring_effect * 
             case_when(scenario=='PERPRES 112/2022 no additional cofiring' & 
                         cofiring_share < 1 ~ (1-cofiring_share[scenario=='PERPRES 112/2022'])/(1-cofiring_share),
                       scenario=='BAU no cofiring' ~ (1-cofiring_share[scenario=='BAU']),
                       T~1))
}



emis_long %<>% mutate(emissions_t = emissions_t * cofiring_effect) %>% ungroup

#add the effect of CCS on emissions
emis_long %<>% get_ccs_effect %>% mutate(emissions_t = emissions_t * ccs_effect)

if(include_APC_scenarios) {
  #calculate effect of retrofitting on emissions
  retrofit_mg <- tibble(pollutant=c('SOx', 'NOx', 'PM'),
                        mg.Nm3_retro=c(130, 150, 10))
  
  emis_long %<>% mutate(retrofit_year=Inf)
  
  emis_long %<>% filter(scenario %in% c('PERPRES 112/2022', '1.5 degrees', '1.5 degrees excluding captive')) %>% 
    left_join(retrofit_mg) %>% 
    mutate(retrofit_year = case_when(COD>=2027~2026,
                                     year_retire>2035~2030, T~Inf),
           is_retrofit = retrofit_year==2030) %>% 
    group_by(scenario, is_retrofit) %>% 
    mutate(retrofit_lead = -round(3*rank(-cost_mn_currentUSD_per_GWh)/length(cost_mn_currentUSD_per_GWh),0),
           retrofit_year = retrofit_year + ifelse(retrofit_year==2030, retrofit_lead, 0)) %>% 
    ungroup %>% select(-is_retrofit, -retrofit_lead) %>% 
    mutate(emissions_t = emissions_t * case_when(pollutant=='CO2' | year<=retrofit_year~1,
                                                 pollutant=='Hg' & year>retrofit_year ~ (1-.75) / (1 - hg_capture),
                                                 T~pmin(1, mg.Nm3_retro/mg.Nm3)),
           scenario=paste(scenario, '/w APC')) %>% 
    bind_rows(emis_long)
  
  #calculate newbuild and retrofit APC capacity
  emis_long %>% filter(grepl('APC', scenario), is.finite(retrofit_year), is.finite(COD), year_retire>2035) %>% 
    distinct(CFPP.name, scenario, .keep_all=T) %>% 
    group_by(scenario, is_retrofit = retrofit_year!=2026) %>% 
    summarise(across(c(MW_fitted_w_APC=MW), sum)) %>% 
    mutate(cost_mn_USD = MW_fitted_w_APC * ifelse(is_retrofit, 204, 152)) %>% 
    summarise(across(where(is.numeric), sum))
  
  emis_long %>% filter(scenario=='PERPRES 112/2022', year %in% c(2012,2022,2030), 
                       COD<=year, year_retire>=year) %>% 
    group_by(pollutant, year) %>% summarise(across(emissions_t, sum)) %>% 
    mutate(change=emissions_t/lag(emissions_t)-1)
}


#export base year emissions for modeling
#source('project_workflows/indonesia_iesr_export_CALPUFF_inputs.R')

emis_long %<>% filter(COD<=year, year_retire>=year)

if(include_APC_scenarios) {
  #compare effect of co-firing and APC on emissions
  emis_long %>% group_by(scenario, year, pollutant) %>% 
    summarise(across(emissions_t, sum)) %>% 
    group_by(pollutant) %>% 
    summarise(cofiring_reduction = emissions_t[scenario=='PERPRES 112/2022' & year==2035] / 
                emissions_t[scenario=='PERPRES 112/2022 no cofiring' & year==2035] - 1,
              apc_reduction = emissions_t[scenario=='PERPRES 112/2022 /w APC' & year==2036] / 
                emissions_t[scenario=='PERPRES 112/2022' & year==2036] - 1)
}

#export emissions pathways
emis_long %>% select(pollutant, scenario, Owner, grid, region, province, Latitude, Longitude, CFPP.name, MW, GEM.ID,
                     Status, new_in_ruptl, COD, year_retire, year, utilization, emissions_t) %>% 
  saveRDS(file.path(emissions_dir, 'indonesia_RUPTL_emission_pathways.RDS'))


emis_long %>% distinct(CFPP.name, GEM.ID, province, grid, scenario, COD, year_retire, MW) %>% 
  write_csv('G:/Shared drives/CREA-HIA/Projects/Indonesia_RUPTL2025/emissions/plant_retirement.csv')

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
  theme_crea() + 
  scale_color_crea_d('dramatic', guide='none') +
  geom_label_repel(data=label_df, aes(label=str_wrap(scenario, 15)), xlim=c(2056, 2060), ylim=c(12,NA), hjust=0) +
  x_at_zero() +
  scale_x_continuous(expand=expansion(mult=c(0,.1))) +
  labs(title='Operating coal-fired capacity by scenario',
       y='GW', x='') -> plt
quicksave(file.path(output_dir, 'Operating coal-fired capacity by scenario.png'), plot=plt, scale=1, logo_scale=1.5, footer_height=.03)

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
    theme_crea() + 
    x_at_zero() +
    scale_color_crea_d(guide=guide) +
    labs(title=title, y='kt/year', x='') +
    scale_x_continuous(expand=expansion(mult=c(0,.15))) -> p
  
  if(guide=='none') p <- p + geom_text_repel(data=label_df, xlim=c(2057, 2060))
  
  quicksave(file.path(output_dir, paste0(title, '.png')), plot=p, scale=1.33, height=height)
}

emis_byyear %>% filter(!grepl('cofiring|APC', scenario)) %>% plot_lines('Emissions by scenario')

emis_byyear %>% filter(grepl('1\\.5 degrees|2022$|APC', scenario), pollutant != 'CO2', !grepl('excl', scenario)) %>% 
  plot_lines('Effect of APC on emissions')

emis_byyear %>% filter(grepl('2022$|2022.*APC', scenario), pollutant != 'CO2') %>% 
  plot_lines('Effect of APC on emissions in the PERPRES 112/2022 scenario', guide='legend', height=8)

emis_byyear %>% filter(grepl('1\\.5 degrees($| /w APC)', scenario), pollutant != 'CO2') %>% 
  plot_lines('Effect of APC on emissions in the 1.5 degree scenario', guide='legend', height=8)

emis_byyear %>% filter(grepl('BAU$|2022$|cofiring', scenario)) %>% plot_lines('Effect of co-firing on emissions')

plot_cols <- function(df, title='') {
  df %>% filter(year>2023) %>% group_by(pollutant, scenario) %>% summarise(across(emissions_t, sum, na.rm=T)) %>% 
    ggplot(aes(scenario, emissions_t/1e6, fill=scenario)) + geom_col() + 
    facet_wrap(~pollutant, scales='free_y') +
    theme_crea(axis.text.x=element_text(angle=45, hjust=1)) + 
    scale_fill_crea_d(guide='none') + x_at_zero() +
    labs(title=title, subtitle='2024 until end-of-life of all plants', y='Mt', x='') -> p
  quicksave(file.path(output_dir, paste0(title, '.png')), plot=p, scale=1.33)
}

emis_byyear %>% filter(!grepl('cofiring|APC', scenario)) %>% plot_cols('Cumulative emissions by scenario')
emis_byyear %>% filter(grepl('1\\.5 degrees|2022$|APC', scenario)) %>% plot_cols('Effect of APC on emissions, cumulative')
emis_byyear %>% filter(grepl('BAU$|2022$|cofiring', scenario)) %>% plot_cols('Effect of co-firing on emissions, cumulative')

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
