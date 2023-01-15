# Set up the environment
# remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
# devtools::reload(pkgload::inst("creapuff"))
require(raster)
require(sf)
require(tidyverse)
require(magrittr)
require(lubridate)
library(readxl)
library(creapuff) 
list.files(path='R', full.names=T) %>% sapply(source)


# Parameters ###################################################################
# ============================= Project specific ===============================
#project_dir="I:/SouthAfrica"       # calpuff_external_data-2 persistent disk (project data)
project_dir="C:/Users/lauri/Desktop/My Drive/air pollution/TAPM/2017cases/SouthAfrica2022"
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"plots") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

#load grid parameters
calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

# List generated csv files
input_dir <- file.path(project_dir,"calpuff_suite/Lephalale") # Where to read all CALPUFF generated files
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ppb', dir=input_dir, hg_scaling=1e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)

#make tifs
calpuff_files %>% make_tifs(grids = grids)

# Select tif data 
calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ppb', dir=input_dir, hg_scaling=1e-3) %>% 
  mutate(scenario_description = paste0(case_when(grepl('mnpp', scenario)~'Lephalale mine and IPP power plant',
                                                 #grepl('pp', scenario)~'Lephalale IPP power plant',
                                                 grepl('mine|mn', scenario)~'Lephalale mine'),
                                       ifelse(grepl('.bg', scenario), ' combined with major background sources', ''))) %>% 
  filter(grepl('pp', scenario) | grepl('pm|tsp|hg', species), scenario != 'lcppipp')

calpuff_files_all %>% write_csv(file.path(input_dir, 'file_info.csv'))



# ================================ General =====================================
gis_dir <- "~/GIS"                         # The folder where we store general GIS data

# creahia::set_env('gis_dir',"~/GIS/")
# Sys.setenv(gis_dir="~/GIS/")
#
# setwd(get_gis_dir())
# system("gsutil rsync -r gs://crea-data/gis .")


# Plots ########################################################################
target_crs <- crs(grids$gridR)

source('project_workflows/emissions_processing_SA.R')

plot_crs=3857
point_sources %<>% st_transform(plot_crs) %>% filter(grepl('IPP|Medupi|Matimba', plant))


lmine <- file.path(emissions_dir, 'Lephalale infrastructure area.kml') %>% rgdal::readOGR() %>% subset(Name=='Development Area') %>% 
  st_as_sf() %>% st_transform(plot_crs)

gtmine <- file.path(emissions_dir, 'Grootgeluk.kml') %>% rgdal::readOGR() %>% 
  st_as_sf() %>% st_transform(plot_crs)

area_sources <- bind_rows(lmine %>% mutate(source='Lephalale mine'),
                          gtmine %>% mutate(source='Grootegeluk mine'))



area_sources_raster <- area_sources %>% st_transform(target_crs) %>% raster::rasterize(grids$gridR)

require(ggspatial); require(ggmap); require(ggrepel)


plot_bb = point_sources %>% st_transform(crs=4326) %>% extent() %>% add(c(-.1,.3,-.1,.35))
basemap <- get_basemap(plot_bb)

calpuff_files_all %>% 
  filter(scenario %notin% c('bg', 'bgmx'),
         species %in% c('so2', 'no2', 'pm25', 'tpm10'),
         !is.na(threshold)) %>% '['(10,T) %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='both',
                point_sources=point_sources,
                area_sources=area_sources,
                basemap=basemap,
                output_dir=output_dir)


plot_bb_hg = point_sources %>% st_transform(crs=4326) %>% extent() %>% add(.7)
basemap_hg <- get_basemap(plot_bb, zoom=9)

calpuff_files_all %>% 
  filter(scenario %notin% c('bg', 'bgmx'),
         species %in% c('hg'),
         !is.na(threshold)) %>% 
  plot_contours(plot_bb=plot_bb_hg,
                contour_type='both',
                point_sources=point_sources,
                area_sources=area_sources,
                basemap=basemap_hg,
                output_dir=output_dir)







raster('~/../Downloads/V5GL03.HybridPM25c_0p10.Global.201901-201912.nc') -> pm25
get_adm(3, 'low') %>% subset(NAME_3=='Lephalale') -> lepha_adm
point_sources %>% st_drop_geometry() %>% to_spdf %>% raster::extract(pm25, .)



# ==============================================================================
# ... Moving ...
print('... moving output files (png, exceedances, tseries,...)')
# PNG files
png_dir <- file.path(project_dir,"png")
if (!dir.exists(png_dir)) dir.create(png_dir)
file.path(output_dir,list.files(output_dir,"\\.png$" )) -> png_files
file.copy(png_files,png_dir, overwrite = TRUE)
file.remove(png_files)
# KML files
kmz_dir <- file.path(project_dir,"kmz") ; 
if (!dir.exists(kmz_dir)) dir.create(kmz_dir)
file.path(output_dir,list.files(output_dir,"\\.kmz$" )) -> kmz_files
file.copy(kmz_files,kmz_dir, overwrite = TRUE)
file.remove(kmz_files)
# Exeedances
exceedances_dir <- file.path(project_dir,"exceedances")
if (!dir.exists(exceedances_dir)) dir.create(exceedances_dir)
file.path(output_dir,list.files(output_dir,"expPop|threshold_exceedances" )) -> exceedances_files
file.copy(exceedances_files,exceedances_dir, overwrite = TRUE)
file.remove(exceedances_files)
# tseries
tseries_dir <- file.path(project_dir,"tseries")
if (!dir.exists(tseries_dir)) dir.create(tseries_dir)
file.path(output_dir,list.files(output_dir,"tseries" )) -> tseries_files
file.copy(tseries_files,tseries_dir, overwrite = TRUE)
file.remove(tseries_files)

browser()

# ==============================================================================
#get WDPA protected areas
grids_wdpa <- grids
grids_wdpa$gridR %<>% (function(x) {crop(x, extent(x)*.33)})
get_wdpa_for_grid(grids_wdpa) -> wdpa_areas
saveRDS(file.path(output_dir, 'WDPA areas.RDS'))

#output deposition results
get_deposition_results(calpuff_files, dir=output_dir, wdpa_areas=wdpa_areas) -> depo

