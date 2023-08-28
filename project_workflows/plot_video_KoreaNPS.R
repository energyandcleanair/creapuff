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
sf_use_s2(F)


case_name='korea_nps'
case_description='attributed to NPS coal power investments'


# Parameters ###################################################################
# ============================= Project specific ===============================
project_dir="H:/koreasteel" 
gis_dir <- "H:/gis"                    # The folder where we store general GIS data

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"animation") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions_coal") # Directory where emission files are stored
input_xls <- file.path(emissions_dir,"NPS_AirPollutionStudy_Updated Data_Translation.xlsx") # File where constant-emission data are specified

#load grid parameters
calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

# List generated csv files
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)

grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)


emissions_data <- read_csv(file.path(emissions_dir, 'emissions, clustered.csv'))
emissions_data %>% to_spdf %>% st_as_sf -> point_sources_to_plot

scenarios_to_process <- emissions_data$emission_names %>% unique %>% tolower

plot_bb = point_sources_to_plot %>% st_transform(crs=crs(grids$gridR)) %>% extent() %>% add(500)
grids$gridR %<>% crop(plot_bb)


#list and select files to process
list.files(path=input_dir, pattern='tser.*1hr.*\\.dat', full.names = T) %>% 
  subset(grepl('pm25_1hr', .)) -> infiles

infiles %<>% grep(paste(paste0('_', scenarios_to_process, '\\.'), collapse='|'), ., value=T)

#convert tseries.dat files to RDS for faster reading and processing
tseries_to_rds(infiles) -> infiles

#sum concentrations for multiple simulations for each timestep (this approach is faster but only works if all simulations have the same set of receptors, otherwise use sum_tseries_rasters)
emissions_data %>% filter(year==2022) %>% 
  group_by(emission_names) %>% summarise(scaling=SOx_tpa[case=='2022_NPS']/SOx_tpa[case=='2022']) %>% 
  mutate(scenario=tolower(emission_names)) -> scaling

tibble(file=infiles, scenario=infiles %>% gsub('.*_|\\.RDS$', '', .)) %>% 
  left_join(scaling) -> scaling

sum_tseries_rds(infiles, case_name, scaling=scaling) -> infiles

#select time steps to process
times <- read_times_from_rds(infiles) %>% subset(month(.)==4)

#interpolate concentration time series files with receptor data into gridded rasters
tseries_rds_to_raster(infiles, grids, times=times, output_dir=output_dir, purge_memory_freq = 10)

#sum concentrations for multiple simulations for each timestep
#sum_tseries_rasters(infiles, case_name=case_name, times=times, overwrite=F)


# Plots ########################################################################
target_crs <- crs(grids$gridR)

#get basemap
require(ggspatial); require(ggmap); require(ggrepel)
register_google(readLines("~/google_api_key.txt"))

plot_bb = point_sources_to_plot %>% st_transform(crs=4326) %>% extent() %>% add(2)
#plot_bb@ymin <- -8.25

basemap <- get_basemap(plot_bb, zoom=7)

plot_bb %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> plot_bb_polygon
grids$gridR %>% projectRaster(crs=4326) %>% extent %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> gridR_polygon
ggmap(basemap) + layer_spatial(plot_bb_polygon, fill=NA, linewidth=2, color='red') + 
  layer_spatial(gridR_polygon, fill=NA, linewidth=2, color='cyan')

#query the files to plot
case_name_old='korea_nps'
case_name='korea-nps'

list.files(output_dir, case_name_old, full.names=T) -> to_rename
file.rename(to_rename, gsub(case_name_old, case_name, to_rename))

calpuff_files_all <- get_calpuff_files(filetype='tseries', dir=output_dir) %>% 
  filter(scenario==case_name) %>% 
  mutate(title=paste(speciesName, 'concentration ', case_description),
         subtitle=format(datetime, "%b %d, %H:%M"))

#define contour breaks
calpuff_files_all$path %>% sapply(function(x) x %>% raster %>% values %>% max(na.rm=T)) -> maxvals
maxvals %>% (function(x) which(x==max(x))) -> break_basis

calpuff_files_all$path[break_basis] %>% raster %>% 
  crop(projectRaster(raster(plot_bb, crs=creapuff.env$llproj), crs=target_crs)) %>% 
  (function(r){r[is.na(r)]<-0;r}) %>% 
  make_contour_breaks(probs=c(0,.8,.99)) %>% 
  (function(x) c(x, pretty(c(last(x), quantile(maxvals, .75))))) %>% 
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
                    quicksave_options=list(footer_height=.0, height=6, logo_hjust=.94, width=6),
                    #ggplot_theme=theme(legend.position='right', legend.direction = "vertical", legend.key.size=unit(.15,'in')),
                    contour_guide=guide_legend(ncol=1)) ->
  frame_files


imgs <- list.files(output_dir, "tseries_pm25_1hr_conc_korea-coal-all.*\\.png$", full.names = T)
file.rename(imgs, gsub('\\.grd', '', imgs))
imgs <- list.files(output_dir, "tseries_pm25_1hr_conc_korea-coal-all.*\\.png$", full.names = T)
imgs_out <- gsub('korea-coal-all', 'korea-coal-all-v2', imgs)

library(grid)
rect <- rectGrob(
  x = unit(6.5, "in"),
  y = unit(1, "in"),
  width = unit(2, "in"),
  height = unit(1, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "white", col='white')
)

for(i in seq_along(imgs)) {
  file.path(imgs[i]) %>% image_read() -> img
  
  ggdraw() + draw_image(img) + draw_grob(rect) -> p
  quicksave(imgs_out[i], plot=p, width=6, logo_hjust=.94, preview=F)
  message(imgs_out[i])
}

