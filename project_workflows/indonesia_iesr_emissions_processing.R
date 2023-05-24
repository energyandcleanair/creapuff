require(raster)
require(tidyverse)
require(magrittr)
require(lubridate)
require(readxl)
require(creapuff)
require(creahelpers)
require(rcrea)

emissions_dir='~/../Downloads/'
emissions_file=file.path(emissions_dir, 'MASTERLIST_Indonesia Coal (15).xlsx')

project_dir=tapmpath('2017cases/Indonesia_JETP')
output_dir=project_dir

source('project_workflows/indonesia_iesr_read_emissions.R')

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
  mutate(across(CFPP.name, make.unique, sep=' Unit '), Hg..t=NA, CO2..t=NA) %>% 
  pivot_longer(matches('SOx$|NOx$|PM$|\\.\\.t', ignore.case=F)) %>% 
  mutate(pollutant = gsub('\\..*', '', name),
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
    base_utilization = case_when(Owner=='captive'~.8, T~base_utilization_gcam*utilization_uplift),
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
emis_long %<>% filter(Status %notin% c('shelved', 'cancelled')) %>% 
  mutate(COD = case_when(Status %in% c('pre-permit', 'planned')~pmax(COD, 2027),
                         Status=='permitted'~pmax(COD, 2026),
                         Status=='announced'~pmax(COD, 2028),
                         Status=='construction'~pmax(COD, 2023),
                         T~COD))

prioritize_retirement <- function(plantdata) {
  plantdata %>% ungroup %>% distinct(CFPP.name, .keep_all=T) %>% arrange(cost_mn_currentUSD_per_GWh)
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



#add grids for retirement
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
hia_plants_total <- read_csv('G:/Shared drives/CREA-HIA/Projects/Indonesia_JETP/hia_plants_total.csv')
hia_plants_total %>% filter(estimate=='central') %>% select(CFPP.name, cost_mn_currentUSD_per_GWh) %>% 
  left_join(emis_long, .) -> emis_long


#add BAU retirement
emis_long %<>% mutate(earliest_retire=2042 - round((2022-COD)/3, 0),
                      year_retire = case_when(!is.na(BAU.retire)~BAU.retire,
                                              Owner=='captive'~pmax(COD+30, earliest_retire), 
                                              T~pmax(COD+30, earliest_retire)), 
                      scenario='BAU')

#expand to all projection years
years <- emis_long$year_retire %>% subset(is.finite(.)) %>% max %>% add(2) %>% seq(2000, ., 1)

emis_long %<>% full_join(tibble(year=years), by=character())

#add ACT scenario
emis_long %<>% mutate(year_retire = case_when(!is.na(ADB_retire)~ADB_retire, T~year_retire), scenario='ACT Investment Plan') %>% 
  bind_rows(emis_long)

jetp_traj <- tibble(year=2036:2051) %>% mutate(delta_percent=-approx(c(2036,2051), c(0,1), year)$y^2)
jetp_captive_traj <- tibble(year=2055:2059) %>% mutate(delta_percent=-approx(c(2055,2060), c(0,1), year)$y^2)

emis_long %<>% mutate(status_category = case_when(grepl('oper|constr|permitted', Status)~'existing',
                                                  T~'new'))

#add PERPRES scenario
emis_long %<>% filter(scenario=='ACT Investment Plan') %>% 
  mutate(eligible_for_retirement = Owner != 'captive') %>% 
  group_by(grid) %>% 
  retire_plants(traj=jetp_traj) %>% 
  mutate(eligible_for_retirement = Owner == 'captive' & status_category=='existing') %>% 
  retire_plants(traj=jetp_captive_traj) %>% 
  mutate(year_retire = case_when(Owner=='captive' & status_category=='new' & year_retire > 2050 ~ 2050, T~year_retire)) %>% 
  mutate(scenario='PERPRES 112/2022') %>% 
  bind_rows(emis_long)

#add 1.5 degree scenario
traj_1.5 <- coal_gen %>% ungroup %>% 
  filter(Scenario=='1p5', year>=2025) %>% 
  mutate(delta_percent=GW/GW[year==2025]-1) %>% 
  full_join(tibble(year=2025:2070), .) %>% 
  mutate(delta_percent = approx(year, delta_percent, year, rule=1)$y) %>% 
  fill(delta_percent)

emis_long %<>% filter(scenario=='PERPRES 112/2022') %>% 
  mutate(eligible_for_retirement = Owner != 'captive',
         COD = ifelse(Status %in% c('operating', 'construction') | !eligible_for_retirement, COD, Inf)) %>% 
  group_by(grid) %>% 
  retire_plants(traj=traj_1.5) %>% 
  mutate(eligible_for_retirement = Owner == 'captive' & status_category == 'existing') %>% 
  retire_plants(traj=jetp_captive_traj) %>% 
  mutate(scenario='1.5 degrees excluding captive') %>% 
  bind_rows(emis_long)

#add 1.5 degree with captive scenario
emis_long %<>% filter(scenario=='PERPRES 112/2022') %>% 
  mutate(eligible_for_retirement = T,
         COD = ifelse(Status %in% c('operating', 'construction') | !eligible_for_retirement, COD, Inf)) %>% 
  group_by(grid) %>% 
  retire_plants(traj=traj_1.5) %>% 
  mutate(scenario='1.5 degrees') %>% 
  bind_rows(emis_long)


#calculate utilization adjustment based on pathway
emis_long %<>% group_by(scenario, Owner) %>% 
  group_modify(function(df, group) {
    basis_scen = ifelse(grepl('1\\.5', group$scenario), '1p5', 'cpol')
    coal_gen %>% filter(Scenario==basis_scen) -> gen_scen
    df %<>% mutate(utilization = case_when(group$Owner=='captive'~base_utilization, 
                                           T~approx(gen_scen$year, gen_scen$load_factor, df$year, rule=2)$y * utilization_uplift),
                   ccs_share = approx(gen_scen$year, gen_scen$ccs_share, df$year, rule=2)$y)
    
    return(df)
  })

emis_long %<>% mutate(across(c(emissions_t, GWh), ~.x * utilization / base_utilization))

#calculate effect of co-firing on emissions
cofiring_effect = read_csv('../eu_ied_review/outputs/cofiring_analysis/cofiring impact on emissions.csv') %>% 
  mutate(biomass_effect=pmax(biomass_effect, 1-biomass_share))

cofiring_effect %>% 
  ggplot(aes(biomass_share, biomass_effect)) + 
  geom_line(linewidth=1, color=crea_palettes$dramatic[1]) + 
  facet_wrap(~pollutantCode) +
  labs(title='Effect of biomass co-firing on air pollutant emissions',
       x='biomass share', y='emissions without co-firing = 100%') +
  theme_crea() +
  scale_x_continuous(expand=expansion(mult=c(0,0)), labels = scales::percent) +
  scale_y_continuous(expand=expansion(mult=c(0,0.05)), labels = scales::percent) +
  expand_limits(y=0) +
  theme(panel.spacing = unit(2, 'lines'), plot.margin = margin(1, 1, .25, .25, unit='lines')) -> plt
quicksave(file.path(output_dir, 'Effect of biomass co-firing on air pollutant emissions.png'), plot=plt, scale=.9, footer_height=.03)

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
if(F) {
  emis_long %<>% filter(scenario=='BAU') %>% mutate(scenario='BAU no cofiring') %>% bind_rows(emis_long)
  emis_long %<>% filter(scenario=='PERPRES 112/2022') %>% 
    mutate(scenario='PERPRES 112/2022 no additional cofiring') %>% bind_rows(emis_long)
}

emis_long %<>% 
  mutate(cofiring_share = case_when(Owner=='PLN' & scenario=='PERPRES 112/2022' & cofiring_share>0 ~ 
                                      pmax(approx(c(2025,2030,2035), c(0,.1,.2), year, rule=2)$y, 
                                           cofiring_share, na.rm = T),
                                    scenario=='BAU no cofiring'~0,
                                    scenario=='PERPRES 112/2022 no additional cofiring' & 
                                      !grepl('implemented', cofiring_status)~0,
                                    T~cofiring_share))

emis_long %<>% filter(scenario=='PERPRES 112/2022') %>% 
  mutate(scenario='PERPRES 112/2022 no cofiring',
         cofiring_share=0) %>% 
  bind_rows(emis_long)

emis_long %<>% 
  get_cofiring_effect %>% 
  group_by(CFPP.name, pollutant, year) %>% 
  mutate(cofiring_effect = cofiring_effect * 
           case_when(scenario=='PERPRES 112/2022 no additional cofiring' & 
                       cofiring_share < 1 ~ (1-cofiring_share[scenario=='PERPRES 112/2022'])/(1-cofiring_share),
                     scenario=='BAU no cofiring' ~ (1-cofiring_share[scenario=='BAU']),
                     T~1)) 



emis_long %<>% mutate(emissions_t = emissions_t * cofiring_effect) %>% ungroup

#add the effect of CCS on emissions
emis_long %<>% get_ccs_effect %>% mutate(emissions_t = emissions_t * ccs_effect)

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

#export base year emissions for modeling
#source('project_workflows/indonesia_iesr_export_CALPUFF_inputs.R')

emis_long %<>% filter(COD<=year, year_retire>=year)


#compare effect of co-firing and APC on emissions
emis_long %>% group_by(scenario, year, pollutant) %>% 
  summarise(across(emissions_t, sum)) %>% 
  group_by(pollutant) %>% 
  summarise(cofiring_reduction = emissions_t[scenario=='PERPRES 112/2022' & year==2035] / 
              emissions_t[scenario=='PERPRES 112/2022 no cofiring' & year==2035] - 1,
            apc_reduction = emissions_t[scenario=='PERPRES 112/2022 /w APC' & year==2036] / 
              emissions_t[scenario=='PERPRES 112/2022' & year==2036] - 1)


#export emissions pathways
emis_long %>% select(pollutant, scenario, Owner, grid, region, province, Latitude, Longitude, CFPP.name, MW, GEM.ID,
                     Status, COD, year_retire, year, utilization, emissions_t) %>% 
  saveRDS(file.path(output_dir, 'indonesia_iesr_emission_pathways v2.RDS'))


#calculate total emissions in future pathways
emis_long %>% 
  #mutate(scenario=gsub('JET-P', 'Proposed JET-P target', scenario)) %>% 
  group_by(pollutant, scenario, year) %>% summarise(across(emissions_t, sum)) %>% ungroup %>% 
  complete(pollutant, scenario, year, fill=list(emissions_t=0)) -> emis_byyear

require(rcrea)
require(ggrepel)
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

map_bb <- emis_long %>% spdf() %>% extent %>% multiply_by(1.1) %>% as.matrix() %>% as.vector
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
