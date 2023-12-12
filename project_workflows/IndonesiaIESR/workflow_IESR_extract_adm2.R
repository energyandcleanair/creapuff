# Set up the environment
# remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
# remotes::install_github("energyandcleanair/creahelpers")
# remotes::install_github("energyandcleanair/rcrea")
# devtools::reload(pkgload::inst("creapuff"))
require(raster)
require(sf)
require(tidyverse)
require(magrittr)
require(lubridate)
library(readxl)
library(creapuff) 
#list.files(path='R', full.names=T) %>% sapply(source)
require(rcrea)
require(creahelpers)


# Parameters ###################################################################
# ============================= Project specific ===============================
project_dir="G:/IndonesiaIESR"       # calpuff_external_data-2 persistent disk (project data)

load(file.path(project_dir, 'CALPUFF_setup.RData'))

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"plots") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

#load grid parameters
calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

# List generated csv files
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)

grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=5)

#make tifs
#calpuff_files %>% filter(period=='annual', species %in% c('so2', 'no2', 'ppm25', 'so4', 'no3')) %>% make_tifs(grids = grids, overwrite = F)
#calpuff_files %>% filter(period=='annual', species %in% c('so2', 'no2', 'ppm25', 'so4', 'no3')) %>% split(1:8) -> tif_batches
calpuff_files %>% filter(species %in% c('so2', 'no2', 'ppm25', 'so4', 'no3'), !is.na(threshold) | period=='daily', period!='annual') %>% split(1:4) -> tif_batches

require(doFuture)
require(foreach)
registerDoFuture()
future::plan("multisession", workers = 4)

foreach (i = seq_along(tif_batches)) %dopar% ({
  tif_batches[[i]] %>% make_tifs(grids = grids, overwrite = F)
})

#scale and sum up concentrations based on a scenario
calpuff_files_orig <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>% 
  mutate(emitted_species=case_when(species %in% c('so2', 'so4')~'SOx',
                                   species %in% c('no2','no3')~'NOx',
                                   species=='ppm25'~'PM'),
         cluster=scenario)

source('project_workflows/IndonesiaIESR/read_IESR_emissions.R')
emis %<>% rename(emitted_species=pollutant)

get_scaled_raster <- function(cluster_emissions, species_out, hr_out) {
  if(species_out=='pm25') species_out=c('so4', 'no3', 'ppm25')
  
  cluster_emissions %<>% group_by(cluster, emitted_species) %>% summarise(across(emissions_t, sum)) %>% left_join(modeled_emissions)
  
  calpuff_files_orig %>% filter(species %in% species_out, hr==hr_out) %>% 
    inner_join(cluster_emissions) %>% mutate(scaling = emissions_t / modeled_emissions) ->
    scaling
  
  r <- scaling$path[1] %>% raster() %>% raster()
  r[] <- 0
  
  for(i in seq_along(scaling$path)) {
    message(scaling$name[i])
    scaling$path[i] %>% raster %>% multiply_by(scaling$scaling[i]) %>% add(r) -> r 
  }
    
  
  return(r)
}

emis %>% ungroup %>% 
  filter(grepl("1\\.5 degrees(.*captive)?$|PERPRES.*2022$", scenario), 
         #grepl("1\\.5|PERPRES.*2022$|APC", scenario), 
         year %in% c(2010, 2022, 2030, 2040, 2050),
         year>2022 | grepl('PERPRES', scenario)) %>% #, , 
  distinct(emission_scenario=scenario, year) %>% 
  full_join(expand_grid(species = c('pm25', 'so2', 'no2'), hr=c(1,24,8760)), by=character()) %>% 
  filter(hr!=1 | species != 'pm25') %>% 
  mutate(scenario=paste0('s', as.numeric(as.factor(emission_scenario)))) ->
  scens

overwrite=T
scens %>% #filter(species=='no2', emission_scenario=='1.5 degrees', hr==8760, year==2030) %>% 
  group_by(across(everything())) %>% 
  group_walk(function(df, group) {
    message(group)
    outfile <- file.path(output_dir, paste0('rank(0)_', group$species, '_',group$hr,'hr_conc_', group$scenario, 'y', group$year, '.tif'))
    if(!file.exists(outfile) | overwrite) {
      emis %>% filter(scenario==group$emission_scenario, year==group$year) %>% arrange(cluster) %>% 
        get_scaled_raster(group$species, group$hr) %>% 
        writeRaster(outfile, overwrite=overwrite)
      message(outfile, 'written')
    }
  })

# Select tif data 
calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=output_dir, hg_scaling=1e-3) %>% 
  separate(scenario, c('scenario_number', 'year'), sep='y', remove=F) %>% 
  left_join(scens %>% distinct(scenario_number=scenario, emission_scenario)) %>% 
  filter(year>2023 | emission_scenario == "PERPRES 112/2022") %>% 
  mutate(subtitle=paste0(ifelse(year<=2023, 'in ', paste0(emission_scenario, ' scenario, ')), year),
         scenario_description='all coal power plants in Indonesia') %>% 
  mutate(title=make_titletxt(., line_break = F),
         subtitle_facets=paste0('by scenario'))





# ==============================================================================
#get WDPA protected areas
grids_wdpa <- grids
grids_wdpa$gridR %<>% (function(x) {crop(x, extent(x)*.33)})
get_wdpa_for_grid(grids_wdpa) -> wdpa_areas
saveRDS(file.path(output_dir, 'WDPA areas.RDS'))

#output deposition results
calpuff_files_all %>% get_deposition_results(dir=output_dir, wdpa_areas=wdpa_areas) -> depo2

depo$by_landuse %>% filter(broad.cat != 'ocean') %>% group_by(pollutant, scenario, unit) %>% summarise(across(deposition, sum))
depo$into_protected_areas %>% 
  rename(wdpa_area=name) %>% 
  pivot_longer(-wdpa_area) %>% 
  mutate(species = name %>% gsub('_.*', '', .),
         variable = name %>% gsub('.*_', '', .),
         scenario = name %>% gsub('^[a-z]*_', '', .) %>% gsub('_.*', '', .)) ->
  wdpa_depo


wdpa_depo %>% filter(scenario=='mnpp') %>% group_by(variable) %>% arrange(desc(value)) %>% slice_max(value, n=5)

#extract concentrations by adm2
gis_dir<-'~/GIS/'
adm2 <- get_adm(2, 'low') %>% subset(NAME_0=='Indonesia')
adm2$region_index <- seq_along(adm2$NAME_2)

calpuff_files_all %>% filter(grepl('BAU$|1\\.5 degrees$|captive$|2022$', emission_scenario)) %>% 
  mutate(raster_index=paste0('X', seq_along(path))) -> to_extract
to_extract$path %>% stack -> r
names(r) <- to_extract$raster_index

adm2 %>% spTransform(crs(r)) %>% rasterize(r, 'region_index') -> adm2_r

raster::zonal(r, adm2_r, max, na.rm=T) -> conc_adm2

conc_adm2 %>% as_tibble %>% 
  pivot_longer(starts_with('X'), names_to='raster_index') %>% 
  rename(region_index=zone) %>% 
  right_join(to_extract %>% select(raster_index, title, emission_source=scenario_description, emission_scenario, speciesName, period, year, unit),.) %>% 
  right_join(adm2@data %>% select(region_index, GID_1, GID_2, starts_with('NAME_')),.) %>% 
  select(-contains('index')) ->
  conc_adm2_out

conc_adm2_out %>% saveRDS('~/air pollutant concentrations attributed to coal plants by city.RDS')

conc_adm2_out %>% write.csv(file.path(output_dir, 'air pollutant concentrations attributed to coal plants by city.csv'), row.names=F)
conc_adm2_out %>% write_excel_csv(file.path(output_dir, 'air pollutant concentrations attributed to coal plants by city.csv'))

