require(terra)
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
gis_dir <- "H:/gis"                    # The folder where we store general GIS data

#load(file.path(project_dir, 'CALPUFF_setup.RData'))

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"video") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

select <- dplyr::select
source('project_workflows/read_IESR_emissions.R')
emissions_data %>% to_spdf %>% st_as_sf -> point_sources_to_plot


#load grid parameters
calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

sf_use_s2(F)



# creahia::set_env('gis_dir',"~/GIS/")
# Sys.setenv(gis_dir="~/GIS/")
#
# setwd(get_gis_dir())
# system("gsutil rsync -r gs://crea-data/gis .")


calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=5)

plot_bb = point_sources_to_plot %>% st_transform(crs=crs(grids$gridR)) %>% extent() %>% add(800)
grids$gridR %<>% crop(plot_bb)

scenarios_to_process=calpuff_files$scenario %>% 
  (function(x) paste0('v', substr(x,2,8))) %>% unique

emissions_data %<>% mutate(video_name = paste0('v', substr(tolower(emission_names),2,8)))

targetcrs = crs(grids$gridR)

list.files(path=input_dir, pattern='tser.*1hr.*\\.dat', full.names = T) %>% 
  subset(grepl('pm25_1hr', .)) -> infiles

infiles %<>% grep(paste(paste0('_', scenarios_to_process, '\\.'), collapse='|'), ., value=T)
queue <- infiles %>% gsub('\\.dat', '.RDS', .) %>% file.exists %>% not

#process CALPUFF daily timeseries .dat files into long format .RDS files
for(inF in infiles[queue]) {
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


list.files(pattern='tser.*1hr.*\\.RDS', path=input_dir, full.names = T) %>% 
  grep(paste(paste0('_', scenarios_to_process, '\\.'), collapse='|'), ., value=T) -> infiles

r <- as(grids$gridR, 'SpatRaster')
times <- infiles[1] %>% readRDS %>% '[['('concentrations') %>% 
  filter(month(dt)==4, hour(dt) %in% ((0:7)*3)) %>% 
  use_series(dt) %>% unique

output_dir <- file.path(project_dir, 'animation'); dir.create(output_dir)

infiles <- infiles[order(as.numeric(str_match(infiles, "[0-9]+\\.")))]
overwrite=F

for(inF in infiles) {
  outfiles <- inF %>% basename() %>% gsub('\\.RDS', '', .) %>% 
    paste0('_', format(times, "%Y-%m-%dZ%H%M"), '.grd') %>% 
    file.path(output_dir, .)
  
  queue <- !file.exists(outfiles) | overwrite
  
  if(any(queue)) {
    message('processing ', inF)
    indata <- readRDS(inF)
    receptors <- indata$receptors %>% to_spdf(crs=targetcrs) %>% vect
    
    indata$concentrations %>% filter(dt %in% times) %>% 
      merge(receptors[,'receptor'], .) ->
      point_data
    
    point_data %<>% terra::crop(r)
    
    for(i in seq_along(times)[queue]) {
      message('writing ', outfiles[i])
      if(!file.exists(outfiles[i]) | overwrite)
        point_data %>% subset(point_data$dt==times[i]) %>% 
        interpNear(r, ., 'value', radius=75, interpolate=T) %>% 
        'names<-'('layer') %>% 
        writeRaster(outfiles[i], overwrite=overwrite)
    }
  }
}

case_name <- 'allsources'

outfiles <- infiles[1] %>% basename() %>% gsub('_[a-z0-9]+\\.RDS', '', .) %>% 
  paste0('_', case_name, '_', format(times, "%Y-%m-%dZ%H%M"), '.grd') %>% 
  file.path(output_dir, .)

queue <- outfiles %>% file.exists %>% not %>% or(overwrite)
#queue <- 1:8

for(i in seq_along(times)[queue]) {
  sourcefiles <- infiles %>% basename() %>% gsub('\\.RDS', '', .) %>% 
    paste0('_', format(times[i], "%Y-%m-%dZ%H%M"), '.grd') %>% 
    file.path(output_dir, .)
  
  sourcefiles %>% rast %>% sum(na.rm=T) %>% 
    'names<-'('layer') %>% 
    (function(r){r[is.na(r)]<-0;r}) %>% 
    writeRaster(outfiles[i], overwrite=overwrite)
  message(outfiles[i], ' written')
}

# Plots ########################################################################
target_crs <- crs(grids$gridR)

#plots
require(ggspatial); require(ggmap); require(ggrepel)
register_google(readLines("~/google_api_key.txt"))

source('project_workflows/read_iesr_emissions.R')

emis %>% distinct(cluster, .keep_all=T) %>% to_spdf %>% st_as_sf -> point_sources_to_plot

plot_bb = point_sources_to_plot %>% st_transform(crs=4326) %>% extent() %>% add(2)
basemap <- get_basemap(plot_bb, zoom=4)

plot_bb %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> plot_bb_polygon
ggmap(basemap) + layer_spatial(plot_bb_polygon, fill=NA, linewidth=2, color='red')

color_scale_basis_scenario <- NULL #calpuff_files_all %>% filter(year==2022) %>% distinct(scenario) %>% unlist

calpuff_files_all <- list.files(pattern='\\.grd$', path=output_dir, full.names = T) %>% 
  tibble(path=.) %>% separate(path, c('filetype', 'species', 'hr', 'type', 'scenario', 'datetime'), remove=F, sep='_') %>% 
  filter(scenario=='allsources') %>% 
  mutate(name=basename(path),
         datetime=datetime %>% gsub('\\..*', '', .) %>% ymd_hm(),
         speciesName=case_when(species=='pm25'~'PM2.5', species=='tpm10'~'PM10',T~toupper(species)),
         title=paste(speciesName, 'concentration from coal power plants in Indonesia'),
         subtitle=format(datetime, "%b %d, %H:%M"))

calpuff_files_all$path %>% sapply(function(x) x %>% raster %>% values %>% max(na.rm=T)) -> maxvals

maxvals %>% (function(x) which(x==max(x))) -> break_basis

calpuff_files_all$path[break_basis] %>% raster %>% (function(r){r[is.na(r)]<-0;r}) %>% 
  make_contour_breaks(probs=c(0,.8,.99)) %>% 
  (function(x) c(x, pretty(c(last(x), quantile(maxvals, .4)))) %>% unique %>% 
     subset(.>0) %>% sort) -> brks

calpuff_files_all %>% group_by(datetime) %>% 
  mutate(plotscale=1, threshold=NA, threshold.plotunit=NA, plotunit="µg/m3", plot_filename=name) %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='filled',
                contour_breaks = brks,
                point_sources=point_sources_to_plot,
                basemap=basemap,
                label_contours=F,
                include_threshold_as_break=F,
                color_scale=c(crea_palettes$change[4:7], 'black'),
                fill_alpha_function = (function(x) x^.7*1),
                #source_marker_linewidth=.25, source_marker_size=.7, source_marker_alpha=.8,
                label_sources=F,
                output_dir=output_dir,
                quicksave_options=list(width=10, height=6, scale=1.33, logo=T),
                ggplot_theme=theme(legend.position='bottom', legend.direction = "horizontal"),
                contour_guide=guide_legend(nrow=1))

