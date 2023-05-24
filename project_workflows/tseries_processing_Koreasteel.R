library(creahia)
library(creapuff)

library(magrittr)
library(sf)
library(raster)
library(readxl)
library(zoo)

sf_use_s2(F)


project_dir="I:/koreasteel"       # calpuff_external_data-2 persistent disk (project data)
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"HIA") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files

gis_dir <- "F:/gis"                    # The folder where we store general GIS data

# creahia::set_env('gis_dir',"~/GIS/")
# Sys.setenv(gis_dir="~/GIS/")
#
# setwd(get_gis_dir())
# system("gsutil rsync -r gs://crea-data/gis .")


scenarios_to_process='krsteel'
cities_to_process='.'


# Load CALMET parameters
get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>% head(1) %>% use_series(path) %>% 
  raster %>% raster -> target_raster

targetcrs = crs(target_raster)
adm <- readRDS('F:/gis/boundaries/gadm36_1_low.RDS') %>% subset(NAME_0=='South Korea') %>% 
  spTransform(targetcrs) %>% st_as_sf() %>% mutate(region_id=GID_1)

list.files(path=input_dir, pattern='tser.*1hr.*\\.dat', full.names = T) -> infiles
infiles %<>% grep(paste0('_', scenarios_to_process, '\\.'), ., value=T)

#process CALPUFF daily timeseries .dat files into long format .RDS files
for(inF in infiles) {
  alldata <- read.table(inF, skip=14)
  recnames <- paste0('R', 1:(ncol(alldata)-3))
  names(alldata) <- c('Y', 'J', 'HM', recnames)
  alldata %<>% 
    mutate(HM=HM %>% gsub('00', '', .)) %>% unite(dt, Y, J, HM, sep=' ') %>% 
    mutate(dt=as.POSIXct(dt, format='%Y %j %H'))
  
  alldata %>% pivot_longer(starts_with('R'), names_to='receptor') ->
    alldata_df
  

  rec <- read.table(inF, skip=6, nrows=5)
  rec_df <- rec[,-1] %>% t %>% data.frame
  names(rec_df) <- c('Type', 'ix', 'iy', 'Xkm', 'Ykm')
  rec_df$receptor <- recnames
  rec_df %<>% mutate(across(c(Xkm, Ykm), as.numeric))
  
  
  inF %>% basename() %>% gsub('\\.dat', '', .) %>% strsplit('_') %>% unlist -> filedata
  
  outF <- inF %>% gsub('\\.dat', '.RDS', .)
  saveRDS(list(receptors=rec_df,
               run_info=tibble(poll=filedata[2],
                               scenario=filedata[5]),
               concentrations=alldata_df), outF)
  message(outF, ' written')
}


list.files(pattern='tser.*1hr.*\\.RDS', path=input_dir, full.names = T) -> infiles

for(inF in infiles) {
  indata <- readRDS(inF)
  receptors <- indata$receptors %>% to_spdf(crs=targetcrs) %>% st_as_sf
  
  st_contains(adm, receptors, sparse=FALSE) -> overlaps
  overlaps %>% apply(2, function(x) {
    x.out <- adm$region_id[x][1]
    if(length(x.out)==0) x.out<-NA
    x.out
  }) %>% unlist ->
    receptors$region_id
  
  indata$concentrations %<>% 
    left_join(receptors %>% st_drop_geometry %>% dplyr::select(receptor, region_id)) %>% 
    filter(!is.na(region_id)) %>% 
    left_join(adm %>% st_drop_geometry() %>% dplyr::select(region_id, matches('^NAME_|GID_'))) %>% 
    bind_cols(indata$run_info)
  
  
  indata$concentrations %>% 
    group_by(across(c(region_id, matches('^NAME_|GID_'), scenario, poll, dt))) %>% 
    summarise(across(value, max)) ->
    adm_concs
  
  outF <- inF %>% gsub('\\.RDS', '.csv', .)
  outF_monthly <- inF %>% gsub('\\.RDS', '.csv', .) %>% gsub('tseries_', 'monthly_by_admin_', .)
  
  adm_concs %>% write_csv(outF)
  
  indata$concentrations %>% 
    mutate(date=lubridate::date(dt)) %>% 
    group_by(across(c(matches('^NAME_|GID_'), date, poll, scenario, receptor))) %>% 
    summarise(concentration_1hr_max=max(value),
              concentration_mean=mean(value)) %>% 
    mutate(month=lubridate::month(date)) %>% 
    group_by(across(c(matches('^NAME_|GID_'), month, poll, scenario, receptor))) %>% 
    summarise(concentration_1hr_max=max(concentration_1hr_max),
              concentration_24hr_max=max(concentration_mean),
              concentration_mean=mean(concentration_mean)) %>% 
    group_by(across(c(matches('^NAME_|GID_'), month, poll, scenario))) %>% 
    summarise(across(starts_with('concentration'), max)) %>% 
    write_excel_csv(outF_monthly)
}

list.files(input_dir, 'monthly_by_admin_', full.names=T) %>% read_csv() %>% 
  bind_rows() %>% 
  mutate(across(starts_with('concentration'), 
                function(x) {
                  case_when(poll=='so2'~1/2.62,
                            poll=='no2'~1/1.88,
                            poll=='pm25'~1) * x
                }),
         unit=case_when(poll %in% c('no2', 'so2')~'ppb',T~'µg/m3')) -> alldata

alldata %>% write_excel_csv(file.path(input_dir, 'monthly_by_admin_all.csv'))

alldata %>% ggplot(aes(month, concentration_mean, col=poll)) + geom_line() + 
  facet_wrap(~NAME_1, scales='free_y')


infiles %>% gsub('\\.RDS', '.csv', .)

require(ggspatial)
ggplot()+layer_spatial(adm)+layer_spatial(receptors)
