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
project_dir="H:/NewLargo" 
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"plots") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions") # Directory where emission files are stored
input_xls <- file.path(emissions_dir,"emission_NewLargo.xlsx") # File where constant-emission data are specified

#load grid parameters
#calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
calmet_result <- readRDS(file.path("H:/SouthAfrica/calpuff_suite/calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

# List generated csv files
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)





grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)

#make tifs
calpuff_files %>% filter(!is.na(threshold) | period=='annual', species %in% c('pm25')) %>% #Jamie: you had 'no2', 'so2', 'hg' included - why? They were not modeled
  make_tifs(grids = grids, overwrite=T)

# Select tif data 
calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) 


# Scale results to get D3 and H3 simulations

scaling_factor = 1.979334757
calpuff_files_all %>% filter(scenario=='d2') %>% use_series(path) -> rasters_to_scale
for(input_filename in rasters_to_scale) {
  output_filename <- gsub("_d2", "_d3", input_filename)
  message('input_filename: ', input_filename)
  message('output_filename: ', output_filename)
  raster(input_filename) %>% multiply_by(scaling_factor) %>% writeRaster(output_filename)
}

calpuff_files_all %>% filter(scenario=='h2') %>% use_series(path) -> rasters_to_scale
for(input_filename in rasters_to_scale) {
  output_filename <- gsub("_h2", "_h3", input_filename)
  message('input_filename: ', input_filename)
  message('output_filename: ', output_filename)
  raster(input_filename) %>% multiply_by(scaling_factor) %>% writeRaster(output_filename)
}



# Add results to get combined F3/D3/H3 simulations

calpuff_files_all %>% filter(scenario=='h3') %>% use_series(path) -> h3_files

for(input_filename in h3_files) {
  
  # define name for file output
  output_filename <- gsub("h3_test", "f3.d3.h3", input_filename)

  # define paths for input and convert into rasters
  h3_file <- input_filename
  h3_raster <- raster(h3_file)
  f3_file <- gsub("h3_test", "f3", h3_file)
  f3_raster <- raster(f3_file)
  d3_file <- gsub("f3", "d3", f3_file)
  d3_raster <- raster(d3_file)
  
  raster::stack(h3_file, f3_file, d3_file) %>% sum %>% writeRaster(output_filename, overwrite=TRUE)
  
  # add the three rasters together
  #combined =  f3_raster + d3_raster + h3_raster %>% writeRaster(output_filename, overwrite=TRUE)
}




# emissions_data <- read_csv(file.path(emissions_dir,'emissions inputs, with clustering.csv'))



#coord_data = read_xlsx(file.path(emissions_dir, 'emission_NewLargo.xlsx'), sheet='Coordinates', .name_repair=make.unique)
#
#coord_data %<>% pivot_longer(-Pit) %>% 
#  mutate(axis=name %>% gsub('[0-9\\.]', '', .), corner=name %>% gsub('[a-z]', '', .) %>% gsub('1\\.1', '5', .)) %>% 
#  select(-name) %>% spread(axis, value) %>% 
#  group_by(Pit) %>%
#  summarise(geomtry = st_sfc(st_polygon(list(cbind(lon, lat))), crs = 4326))


#emissions_data <- merge(emissions_data,coord_data,by="Pit")

plot_crs=3857

c('D', 'F', 'H') %>% 
  lapply(function(pit) pit %>% paste0('pit ', ., '.kml') %>% file.path(emissions_dir, .) %>% 
           st_read %>% st_zm() %>% st_transform(plot_crs) %>% mutate(source=paste('Pit', pit))) %>% 
  bind_rows -> area_sources

area_sources_raster <- area_sources %>% st_transform(target_crs) %>% raster::rasterize(grids$gridR)


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
#read_xlsx(input_xls, skip=1) %>% set_names(make.names(names(.))) -> emissions_data

#emissions_data %>% to_spdf %>% st_as_sf -> point_sources_to_plot

#getplants <- function(calpuff_files) {
#  emissions_data %>% group_by(plant) %>% summarise(across(matches('^Lat|^Lon'), mean),
#                                                              across(c(MW, ends_with('pa')), sum)) %>% 
#    to_spdf %>% st_as_sf %>% 
#    mutate(source=plant %>% capitalize_first())
#}

require(ggspatial); require(ggmap); require(ggrepel)
register_google(readLines("~/google_api_key.txt")) # did not work, but not present in other scripts


plot_bb = area_sources %>% st_transform(crs=4326) %>% extent() %>% add(1) #Jamie: the plotting area seemed way too large, with very low concs in most of if
plot_bb %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> plot_bb_polygon

basemap <- get_basemap(plot_bb*1.5) #Jamie: you had the zoom set to 7 which was too large for your plotting area, resulting in a base map that showed a cropped area of a large scale map

ggmap(basemap) + layer_spatial(plot_bb_polygon, fill=NA, linewidth=2, color='red')

calpuff_files_all %<>% mutate(scenario_description = paste0(scenario)) #Jamie: you probably know it but this is where you give the scenarios intelligible names for the plots


calpuff_files_all %>%
  filter(species %in% c('pm25'),
         !is.na(threshold)) %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='both',
                area_sources=area_sources,
                #color_scale_basis_scenario='base',
                #point_sources=getplants,
                source_label_color = 'darkorange',
                basemap=basemap,
                label_sources=T,
                output_dir=output_dir)




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
