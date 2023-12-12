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


case_description='Banten-Suralaya coal power plant'
case_name='Banten-Suralaya'


# Parameters ###################################################################
# ============================= Project specific ===============================
project_dir="H:/WestJava"       # calpuff_external_data-2 persistent disk (project data)
gis_dir <- "H:/gis"                    # The folder where we store general GIS data

#load(file.path(project_dir, 'CALPUFF_setup.RData'))

input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir, 'plots'); dir.create(output_dir)
emissions_dir <- file.path(project_dir,"emissions")

#select <- dplyr::select
read_xlsx(file.path(emissions_dir, 'HIA_BantenSuralaya_2023_Tcorr.xlsx'), sheet=2) %>% 
  summarise(across(c(Longitude, Latitude), mean, na.rm=T)) -> emis

emis %>% to_sf_points() -> point_sources_to_plot


#load grid parameters
calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

sf_use_s2(F)

calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)


#list and select files to process
list.files(path=input_dir, pattern='tser.*_(no2|so2|pm25)_24hr.*\\.dat', full.names = T) -> infiles

#convert tseries.dat files to RDS for faster reading and processing
tseries_to_rds(infiles) -> infiles

#select time steps to process
times <- read_times_from_rds(infiles)

#aggregate to monthly data
average_timestep(infiles) -> infiles_agg


#interpolate concentration time series files with receptor data into gridded rasters
tseries_rds_to_raster(infiles_agg, grids, output_dir=output_dir)


# Plots ########################################################################
target_crs <- crs(grids$gridR)

#get basemap
require(ggspatial); require(ggmap); require(ggrepel)
register_google(readLines("~/google_api_key.txt"))

plot_bb = point_sources_to_plot %>% st_transform(crs=4326) %>% extent() %>% add(c(-1.5, 1.5, -1.5, 1.2))

basemap <- get_basemap(plot_bb, zoom=8)

plot_bb %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> plot_bb_polygon
grids$gridR %>% projectRaster(crs=4326) %>% extent %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> gridR_polygon
ggmap(basemap) + layer_spatial(plot_bb_polygon, fill=NA, linewidth=2, color='red') + 
  layer_spatial(gridR_polygon, fill=NA, linewidth=2, color='cyan')

#query the files to plot
calpuff_files_all <- get_calpuff_files(filetype='tseries', dir=output_dir) %>% 
  mutate(title=paste(speciesName, 'concentration from', case_description), subtitle="average by month",
         month=factor(month.name[month(datetime)], levels=month.name)) %>% 
  filter(!is.na(datetime), grepl('averaged', name))

calpuff_files_all %>% #filter(speciesName=='PM2.5') %>% 
  group_by(speciesName) %>% 
  plot_contours(plot_bb,
                point_sources = point_sources_to_plot %>% mutate(source='PLTU Suralaya'),
                basemap=basemap,
                facet_by='month',
                label_contours=F,
                output_dir=output_dir,
                title_width_characters=80,
                ggplot_theme=theme(axis.text.x = element_text(angle=30, vjust=0)))



