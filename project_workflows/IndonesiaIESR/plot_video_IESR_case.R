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


#case_name='cilacap'
#case_description='PLTU Cilacap'

case_name='cirebon'
case_description='PLTU Cirebon'

# Parameters ###################################################################
# ============================= Project specific ===============================
project_dir="G:/IndonesiaIESR"       # calpuff_external_data-2 persistent disk (project data)
gis_dir <- "H:/gis"                    # The folder where we store general GIS data

#load(file.path(project_dir, 'CALPUFF_setup.RData'))

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir, 'animation'); dir.create(output_dir)
emissions_dir <- file.path(project_dir,"emissions")

select <- dplyr::select
source('project_workflows/read_IESR_emissions.R')
emis %>% 
  filter(grepl('Cirebon', CFPP.name)) %>% 
  distinct(cluster, .keep_all=T) %>% to_spdf %>% st_as_sf -> point_sources_to_plot


#load grid parameters
calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

sf_use_s2(F)

calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=2)

plot_bb = point_sources_to_plot %>% st_transform(crs=crs(grids$gridR)) %>% extent() %>% add(500)
grids$gridR %<>% crop(plot_bb)

scenarios_to_process=calpuff_files %>% 
  filter(scenario %in% point_sources_to_plot$cluster) %>% 
  use_series(scenario) %>% 
  (function(x) paste0('v', substr(x,2,8))) %>% unique

emissions_data %<>% mutate(video_name = paste0('v', substr(tolower(emission_names),2,8)))


#list and select files to process
list.files(path=input_dir, pattern='tser.*1hr.*\\.dat', full.names = T) %>% 
  subset(grepl('pm25_1hr', .)) -> infiles

infiles %<>% grep(paste(paste0('_', scenarios_to_process, '\\.'), collapse='|'), ., value=T)

#convert tseries.dat files to RDS for faster reading and processing
tseries_to_rds(infiles) -> infiles

#select time steps to process
times <- read_times_from_rds(infiles) %>% subset(month(.)==4 & hour(.) %in% ((0:7)*3))

#interpolate concentration time series files with receptor data into gridded rasters
tseries_rds_to_raster(infiles, grids, times=times, output_case_name = "cirebon-fine")

#sum concentrations for multiple simulations for each timestep
#sum_tseries_rasters(infiles, case_name=case_name, times=times, overwrite=F)



# Plots ########################################################################
target_crs <- crs(grids$gridR)

#get basemap
require(ggspatial); require(ggmap); require(ggrepel)
register_google(readLines("~/google_api_key.txt"))

plot_bb = point_sources_to_plot %>% st_transform(crs=4326) %>% extent() %>% add(3.4)
plot_bb@ymin <- -8.1
plot_bb@ymax <- -5.7
plot_bb@xmin <- 106.6
plot_bb@xmax <- 109.85

basemap <- get_basemap(plot_bb, zoom=8)

plot_bb %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> plot_bb_polygon
grids$gridR %>% projectRaster(crs=4326) %>% extent %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> gridR_polygon
ggmap(basemap) + layer_spatial(plot_bb_polygon, fill=NA, linewidth=2, color='red') + 
  layer_spatial(gridR_polygon, fill=NA, linewidth=2, color='cyan')

#query the files to plot
calpuff_files_all <- get_calpuff_files(filetype='tseries', dir=output_dir) %>% 
  filter(scenario=='cirebon-fine') %>% 
  mutate(title=paste(speciesName, 'concentration from ', case_description),
         subtitle=format(datetime, "%b %d, %H:%M"))

#define contour breaks
calpuff_files_all$path %>% sapply(function(x) x %>% raster %>% values %>% max(na.rm=T)) -> maxvals
maxvals %>% (function(x) which(x==max(x))) -> break_basis

calpuff_files_all$path[break_basis] %>% raster %>% 
  crop(projectRaster(raster(plot_bb, crs=creapuff.env$llproj), crs=target_crs)) %>% 
  (function(r){r[is.na(r)]<-0;r}) %>% 
  make_contour_breaks(probs=c(0,.8,.99)) %>% 
  (function(x) c(x, pretty(c(last(x), quantile(maxvals, .70))))) %>% 
  signif(2) %>% 
  unique %>% 
  subset(.>0) %>% sort -> contour_breaks

#output plots
calpuff_files_all %>% group_by(datetime) %>% 
  plot_video_frames(plot_bb=plot_bb, 
                    point_sources=point_sources_to_plot,
                    basemap=basemap,
                    contour_breaks=contour_breaks,
                    output_dir=output_dir,
                    quicksave_options=list(footer_height=.05, height=6.5),
                    ggplot_theme=theme(legend.position='bottom', legend.direction = "horizontal", legend.key.size=unit(.15,'in')),
                    contour_guide=guide_legend(nrow=1)) ->
  frame_files

calpuff_files_all$path %>% gsub('\\.grd', '.grd.png', .) -> frame_files




