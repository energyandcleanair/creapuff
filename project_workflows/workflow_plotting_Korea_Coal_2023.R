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
project_dir="H:/koreasteel" 
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"plots") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
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

#make tifs
calpuff_files %>% filter(!is.na(threshold) | period=='annual', species %in% c('pm25', 'no2', 'so2', 'hg')) %>% 
  filter(grepl('nps', name)) %>% 
  make_tifs(grids = grids, overwrite=T)

# Select tif data 
calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>% 
  filter(grepl('202', scenario)) %>% 
  mutate(scenario=paste0(scenario, ifelse(grepl('_nps', name), '_nps', '')),
         scenario_description=paste(case_when(grepl('nps', scenario)~"emissions linked to NPS coal investments in",
                                              T~"coal power in"), 
                                    force_numeric(scenario))) %>% 
  mutate(title=make_titletxt(., line_break = F))

emissions_data <- read_csv(file.path(emissions_dir,'emissions inputs, with clustering.csv'))


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

emissions_data %>% to_spdf %>% st_as_sf -> point_sources_to_plot

getplants <- function(calpuff_files) {
  emissions_data %>% group_by(plant) %>% summarise(across(matches('^Lat|^Lon'), mean),
                                                              across(c(MW, ends_with('pa')), sum)) %>% 
    to_spdf %>% st_as_sf %>% 
    mutate(source=plant %>% capitalize_first())
}

require(ggspatial); require(ggmap); require(ggrepel)
register_google(readLines("~/google_api_key.txt"))


plot_bb = point_sources_to_plot %>% st_transform(crs=4326) %>% extent() %>% add(2)
#plot_bb[1:2] %<>% add(.2)
#plot_bb[3:4] %<>% add(-.3)
basemap <- get_basemap(plot_bb, zoom=7)

plot_bb %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> plot_bb_polygon
ggmap(basemap) + layer_spatial(plot_bb_polygon, fill=NA, linewidth=2, color='red')

calpuff_files_all %>% filter(grepl('NPS', scenario_description)) %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='both',
                #color_scale_basis_scenario='base',
                point_sources=getplants,
                basemap=basemap,
                label_sources=F,
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
