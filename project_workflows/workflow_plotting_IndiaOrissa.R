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

# script was accidentally plotting Banten domain and I don't know why
rm(list = ls(all.names = TRUE)) 


# Parameters ###################################################################
# ============================= Project specific ===============================
project_dir="G:/indiaorissa"       # calpuff_external_data-2 persistent disk (project data)


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

#make sure tif files have been created 
calpuff_files %>% 
  filter(species %in% c('pm25', 'no2', 'so2', 'tpm10', 'pm25', 'pm'), period=='annual') %>% 
  as.data.frame() %>%
  make_tifs(grids = grids, overwrite=T)


# Select tif data 
calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)

# Scale files by 24.0 
scaling_factor = 24.0
calpuff_files_all %>% use_series(path) -> rasters_to_scale
for(input_filename in rasters_to_scale) {
  #output_filename <- gsub("allstack", "allstack_corr", input_filename)
  message('input_filename: ', input_filename)
  raster(input_filename) %>% multiply_by(scaling_factor) %>% writeRaster(input_filename, overwrite=T)
}

scaling_factor = 0.00000678
calpuff_files_all %>% filter(species=='pm') %>% use_series(path) -> rasters_to_scale
for(input_filename in rasters_to_scale) {
  message('input_filename: ', input_filename)
  output_filename <- gsub("allstack", "allstack_corr", input_filename)
  raster(input_filename) %>% multiply_by(scaling_factor) %>% writeRaster(input_filename, overwrite=T)
}



calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>% 
  mutate(scenario_description='JSW Steel Facility') %>% 
  mutate(title=make_titletxt(., line_break = F),
         subtitle=case_when(grepl('allstack', scenario)~'all stacks'),
         scenario_description=subtitle,
         subtitle_facets=paste0('by scenario'))



emissions_data = read_xlsx(file.path(emissions_dir, 'JSW_Orisha_steel_cropper_calc.xlsx'), sheet='CALPUFF input') %>% 
  mutate(across(FGD, as.logical))


# ================================ General =====================================
gis_dir <- "H:/GIS"                         # The folder where we store general GIS data

# creahia::set_env('gis_dir',"~/GIS/")
# Sys.setenv(gis_dir="~/GIS/")
#
# setwd(get_gis_dir())
# system("gsutil rsync -r gs://crea-data/gis .")


# Plots ########################################################################
target_crs <- crs(grids$gridR)

#read point_sources from somewhere
emissions_data %>% to_spdf %>% st_as_sf -> point_sources_to_plot

#getplants <- function(calpuff_files) {
#  point_sources_to_plot %>% st_drop_geometry() %>% summarise(across(c(Longitude, Latitude), mean)) %>% to_spdf %>% st_as_sf %>% 
#    mutate(source='Steel facility')
#}

require(ggspatial); require(ggmap); require(ggrepel)
register_google(readLines("~/google_api_key.txt"))
register_google("AIzaSyDACrA6NUM4iQBjZGBV_ONsYN_t0-Aat7s")

plot_bb = point_sources_to_plot %>% st_transform(crs=4326) %>% extent() %>% add(2)
plot_bb[1:2] %<>% add(.2)
plot_bb[3:4] %<>% add(-.3)
basemap <- get_basemap(plot_bb, zoom=8)

plot_bb %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> plot_bb_polygon
ggmap(basemap) + layer_spatial(plot_bb_polygon, fill=NA, linewidth=2, color='red')




calpuff_files_all %>% mutate(subtitle_facets=subtitle, subtitle='by scenario') %>% 
  group_by(period, species, type) %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='both',
                point_sources=point_sources_to_plot,
                basemap=basemap,
                facet_by='subtitle_facets',
                facet_ncol=2,
                include_threshold_as_break=T,
                label_contours = F,
                contour_break_probs=c(0, .85,.95,.995),
                label_sources=F,
                output_dir=output_dir)






