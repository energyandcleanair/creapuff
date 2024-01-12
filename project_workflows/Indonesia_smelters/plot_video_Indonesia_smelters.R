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
project_dir="G:/Indonesia_smelters"       # calpuff_external_data-2 persistent disk (project data)
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
met_dir <- "G:/IndonesiaIESR" # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"plots") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all generated files
emissions_dir <- file.path(project_dir,"emissions") # Directory where arbitrary-varying emission files are stored

read_csv(file.path(emissions_dir, 'emissions_with_cluster.csv')) %>% distinct(loc_cluster, .keep_all=T) %>% to_sf_points() -> point_sources_to_plot


#load grid parameters
calmet_result <- readRDS(file.path(met_dir, "calpuff_suite","calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

sf_use_s2(F)

calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=5)

plot_bb = point_sources_to_plot %>% st_transform(crs=4326) %>% extent() %>% add(c(-3,2.5,-4,1))
grids$gridLL %>% crop(plot_bb+2) %>% projectExtent2(grids$gridR) -> target_bb
grids$gridR %<>% crop(target_bb)

scenarios_to_process="all2030"

#list and select files to process
list.files(path=input_dir, pattern='tser.*1hr.*\\.dat', full.names = T) %>% 
  subset(grepl('pm25_1hr', .)) %>% subset(grepl(scenarios_to_process, .)) -> infiles

infiles %<>% grep(paste(paste0('_', scenarios_to_process, '\\.'), collapse='|'), ., value=T)

#convert tseries.dat files to RDS for faster reading and processing
tseries_to_rds(infiles) -> infiles

#select time steps to process
times <- read_times_from_rds(infiles) %>% subset(month(.) %in% 4 & hour(.) %in% seq(0,24,1))

#interpolate concentration time series files with receptor data into gridded rasters
tseries_rds_to_raster(infiles, grids, times=times, output_dir = output_dir)

# Plots ########################################################################
target_crs <- crs(grids$gridR)

#get basemap
require(ggspatial); require(ggmap); require(ggrepel)
register_google(readLines("~/google_api_key.txt"))

basemap <- get_basemap(plot_bb, zoom=6)

plot_bb %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> plot_bb_polygon
ggmap(basemap) + layer_spatial(plot_bb_polygon, fill=NA, linewidth=2, color='red') + annotation_spatial(point_sources_to_plot)

#query the files to plot
calpuff_files_all <- get_calpuff_files(filetype='tseries', dir=output_dir) %>% 
  filter(scenario %in% scenarios_to_process) %>% 
  mutate(title=paste(speciesName, 'concentration from metal smelters and associated captive power'),
         subtitle=format(datetime, "%b %d, %H:%M"))

#output plots
calpuff_files_all %>% group_by(datetime) %>% #filter(cur_group_id()==1) %>% 
  plot_video_frames(plot_bb=plot_bb, 
                    #times_to_plot=calpuff_files_all$datetime[1],
                    contour_breaks=c(.02,.05,.1,.5,1,10,25,50,100,150),
                    fill_alpha_function = (function(x) x^.99*1),
                    #contour_break_probs=c(0,.9,.95),
                    #highest_contour=150,
                    point_sources=point_sources_to_plot,
                    basemap=basemap,
                    output_dir=output_dir,
                    quicksave_options=list(width=8, height=8, scale=1.2, logo=T, footer_height=.05, logo_hjust=.95),
                    ggplot_theme=theme(legend.position='bottom', legend.direction = "horizontal"),
                    contour_guide=guide_legend(nrow=1))

output_video(paste0(calpuff_files_all$path[1:96], '.png'), output_dir = output_dir)

