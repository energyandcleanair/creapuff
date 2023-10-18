require(terra)
require(raster)
require(sf)
require(tidyverse)
require(magrittr)
require(lubridate)
library(readxl)
#library(creapuff) 
require(rcrea)
require(creahelpers)

#case_name = 'Sumatra_plants'
#PLTU Teluk Sepang, PLTU Nagan Raya, and PLTU Pangkalan Susu
#Bengkulu, Nagan Raya, Pangkalan Susu
#plant_names = c('Bengkulu', 'Nagan Raya', 'Pangkalan Susu')

case_name = 'Cirebon'
plant_names = c('Cirebon')

project_dir="G:/IndonesiaIESR"       # calpuff_external_data-2 persistent disk (project data)
emissions_dir <- file.path(project_dir,"emissions") # Directory where emission files are stored

readRDS(file.path(project_dir, 'HIA', "hia_scenarios.RDS")) %>% ungroup -> hia_scen

gis_dir <- "H:/GIS"                         # The folder where we store general GIS data
source('project_workflows/read_iesr_emissions.R')



hia_scen %>% filter(grepl(paste(plant_names, collapse='|'), CFPP.name)) -> hia_sel

hia_sel %>%   
  mutate(plant_name = CFPP.name %>% gsub(' Unit.*| Phase.*| Expansion', '', .),
                    unit_name = CFPP.name %>% gsub(' NA| Andalas.*', '', .)) %>% 
  filter(plant_name %in% plant_names) %>% 
  mutate(plant_name_in_request = case_when(plant_name=='Bengkulu'~'PLTU Teluk Sepang', T~paste('PLTU', plant_name))) -> case_hia

hia_sel %>% mutate(plant_name=CFPP.name, unit_name=CFPP.name, plant_name_in_request=CFPP.name) -> case_hia

#add total economic costs
case_hia %<>% filter(!double_counted) %>% 
  group_by(CFPP.name, plant_name, unit_name, plant_name_in_request, estimate, double_counted, year, scenario) %>% 
  summarise(across(cost_mn_currentUSD, sum)) %>% 
  mutate(Outcome='economic costs, total', Unit='mln USD') %>% 
  rename(number=cost_mn_currentUSD) %>% 
  bind_rows(case_hia) #%>% select(-cost_mn_currentUSD)

#add total deaths
case_hia %<>% filter(!double_counted, grepl('Deaths', Outcome)) %>% 
  group_by(CFPP.name, plant_name, unit_name, plant_name_in_request, estimate, double_counted, Unit, year, scenario) %>% 
  summarise(across(number, sum)) %>% 
  mutate(Outcome='Deaths, total', Pollutant='All', Cause='All') %>% 
  bind_rows(case_hia)

make_table <- function(df) {
  df %>% ungroup %>% pivot_longer(c(number, cost_mn_currentUSD)) %>% 
    unite(name, name, estimate) %>% 
    spread(name, value) %>% 
    creahia::add_long_names() %>% select(-Outcome, -Cause) %>% 
    rename(Outcome=Outcome_long, Cause=Cause_long) %>% 
    relocate(Outcome, Cause, .before=Pollutant) %>% 
    relocate(number_central, number_low, number_high,
             cost_mn_currentUSD_central, cost_mn_currentUSD_low, cost_mn_currentUSD_high,
             .after=everything())
}

#current impacts
case_hia %>% group_by(CFPP.name) %>% mutate(COD=min(year)) %>% 
  filter(year==pmax(2022, COD), scenario=='BAU', !grepl('Prev|LBW|YLLs', Outcome)) %>% ungroup %>% 
  select(plant_name, unit_name, plant_name_in_request, Outcome, Cause, Pollutant, Unit, year, estimate, number, cost_mn_currentUSD) %>% 
  make_table %>% 
  write_csv(file.path(project_dir, 'HIA', paste0(case_name, '_results_2022.csv')))

emis %>% filter(CFPP.name=='Cirebon Unit 1', grepl('BAU$', scenario), pollutant=='SOx') %>% 
  mutate(scaling = emissions_t[year==2022]/emissions_t) %>% select(year, scaling) -> scaling

case_hia %<>% left_join(scaling) %>% mutate(across(c(number, cost_mn_currentUSD), ~.x*scaling))

#cumulative impacts
case_hia %>% filter(year>2023, !grepl('Prev|LBW|YLLs', Outcome), scenario=='BAU') %>% 
  group_by(plant_name, unit_name, plant_name_in_request, Outcome, Cause, Pollutant, Unit, estimate, double_counted) %>% 
  summarise(across(c(number, cost_mn_currentUSD), ~sum(.x, na.rm=T))) %>% 
  ungroup %>% 
  arrange(plant_name, unit_name, Outcome, Cause) %>% 
  make_table %>% 
  write_csv(file.path(project_dir, 'HIA',paste0(case_name, '_cumulative.csv')))

#avoided impacts through early retirement and APC
case_hia %>% filter(year>2023, !grepl('Prev|LBW|YLLs', Outcome), grepl('PERPRES.*(2022$|APC)|BAU$', scenario)) %>% 
  group_by(plant_name, unit_name, plant_name_in_request, Outcome, Cause, Pollutant, Unit, estimate, double_counted, scenario) %>% 
  summarise(across(c(number, cost_mn_currentUSD), ~sum(.x, na.rm=T))) %>% 
  mutate(across(number, ~.x[scenario=='BAU'] - .x)) %>% 
  ungroup %>% filter(scenario!='BAU') %>% 
  arrange(unit_name, scenario, Outcome, Cause) %>% 
  make_table %>% 
  write_csv(file.path(project_dir, 'HIA',paste0(case_name, '_avoided.csv')))



