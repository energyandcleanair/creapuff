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
#library(creapuff) 
#list.files(path='R', full.names=T) %>% sapply(source)
require(rcrea)
require(creahelpers)

# Parameters ###################################################################
# ============================= Project specific ===============================
#project_dir="I:/SouthAfrica"       # calpuff_external_data-2 persistent disk (project data)
#project_dir="C:/Users/lauri/Desktop/My Drive/air pollution/TAPM/2017cases/SouthAfrica2022"
project_dir="G:/My Drive/air pollution/TAPM/2017cases/SouthAfrica2022"
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"plots_MES") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

#load grid parameters
calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

# List generated csv files
input_dir <- file.path(project_dir,"calpuff_suite/MES_aggregated") # Where to read all CALPUFF generated files
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ppb', dir=input_dir, hg_scaling=1e-3) %>% 
  mutate(year=name %>% gsub('.*_', '', .) %>% force_numeric %>% add(2000))

grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)

#make tifs
calpuff_files %>% filter(!is.na(threshold), species != 'tpm10', year==2022, scenario=='eskom') %>% 
  #filter(!is.na(threshold), species != 'tpm10', year==2031) %>% 
  make_tifs(grids = grids, overwrite=F)

# Select tif data 
calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>% 
  mutate(year=name %>% gsub('.*_', '', .) %>% force_numeric %>% add(2000),
         scenario=paste0(scenario, '_', year-2000),
         scenario_description=case_when(grepl('eskom', scenario)~'Eskom plan',
                                       grepl('bat', scenario)~'BAT',
                                       grepl('compl', scenario)~'MES compliance',
                                       grepl('delay', scenario)~'delayed compliance',
                                       grepl('noimp', scenario)~'no improvements'),
         title=make_titletxt(.) %>% gsub(' ?\n?from ', '', .),
         subtitle=paste("In the", scenario_description, "scenario in", year),
         subtitle_facets=paste0('in ', year, ', by scenario'))


gis_dir = '~/GIS'
get_adm(2, 'low') -> adm2

calpuff_files_all %>% filter(scenario=='eskom_22', species %in% c('pm25', 'so2', 'hg')) -> files_to_extract

files_to_extract %>% use_series(path) %>% stack -> concs
paste0(files_to_extract$species, '_', files_to_extract$period) -> names(concs)
adm2 %<>% cropProj(concs)
extract(concs, adm2, fun=mean) %>% data.frame -> adm2_concs
adm2@data %>% select(starts_with('NAME_')) %>% bind_cols(adm2_concs) -> adm2_concs

adm2_concs %>% write_csv(file.path(output_dir, 'concs by adm 2 region, Eskom 2022 scenario.csv'))
