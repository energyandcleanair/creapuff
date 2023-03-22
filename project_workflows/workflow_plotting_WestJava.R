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
project_dir="H:/WestJava"
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

grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)

#make tifs
calpuff_files %>% filter(!is.na(threshold), species %in% c('pm25', 'no2', 'so2', 'tpm10', 'hg'),
                         scenario %in% c('base', 'bat', 'complian')) %>% 
  make_tifs(grids = grids, overwrite=T)

# Select tif data 
calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>% 
  filter(scenario %in% c('base', 'bat', 'complian')) %>% 
  mutate(scenario_description='Banten Suralaya power plant') %>% 
  mutate(title=make_titletxt(., line_break = F),
         subtitle=case_when(grepl('base', scenario)~'in the current situation',
                            grepl('bat', scenario)~'using best available technology',
                            grepl('compl', scenario)~'under compliance with Indonesian regulation'),
         scenario_description=subtitle,
         subtitle_facets=paste0('by scenario'))

emissions_data = read_xlsx(file.path(emissions_dir, 'HIA_BantenSuralaya_2023.xlsx'), sheet='CALPUFF input') %>% 
  mutate(across(FGD, as.logical))


# ================================ General =====================================
gis_dir <- "F:/GIS"                         # The folder where we store general GIS data

# creahia::set_env('gis_dir',"~/GIS/")
# Sys.setenv(gis_dir="~/GIS/")
#
# setwd(get_gis_dir())
# system("gsutil rsync -r gs://crea-data/gis .")


# Plots ########################################################################
target_crs <- crs(grids$gridR)

#read point_sources from somewhere
emissions_data %>% to_spdf %>% st_as_sf -> point_sources_to_plot

getplants <- function(calpuff_files) {
  point_sources_to_plot %>% st_drop_geometry() %>% summarise(across(c(Longitude, Latitude), mean)) %>% to_spdf %>% st_as_sf %>% 
    mutate(source='Banten Suralaya')
}

require(ggspatial); require(ggmap); require(ggrepel)
register_google(readLines("~/google_api_key.txt"))


plot_bb = point_sources_to_plot %>% st_transform(crs=4326) %>% extent() %>% add(2)
plot_bb[1:2] %<>% add(.2)
plot_bb[3:4] %<>% add(-.3)
basemap <- get_basemap(plot_bb, zoom=8)

plot_bb %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> plot_bb_polygon
ggmap(basemap) + layer_spatial(plot_bb_polygon, fill=NA, linewidth=2, color='red')

calpuff_files_all %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='both',
                color_scale_basis_scenario='base',
                point_sources=getplants,
                basemap=basemap,
                label_sources=F,
                output_dir=output_dir)


calpuff_files_all %>% mutate(subtitle_facets=subtitle, subtitle='by scenario') %>% 
  group_by(period, species, type) %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='both',
                point_sources=getplants,
                basemap=basemap,
                facet_by='subtitle_facets',
                facet_ncol=1,
                include_threshold_as_break=T,
                label_contours = F,
                contour_break_probs=c(0, .85,.95,.995),
                label_sources=F,
                output_dir=output_dir,
                plot_dpi = 200, plot_width=8, plot_height = 15)




pop <- make_pop(grids = grids)

calpuff_files_all %>% 
  filter(!is.na(threshold), year==2031 | scenario_description=='Eskom plan') %>% 
  group_by(scenario, scenario_description, year, species, period, type, threshold, unit) %>% 
  group_modify(function(calpuff_files, group) {
    message(group %>% select(scenario:period))
    calpuff_files$path %>% raster -> r
    tibble(pop=sum(pop[r>group$threshold], na.rm=T),
           area_km2=area(r)[r>group$threshold] %>% sum,
           max_value=max(r[]))
  }) %>% 
  group_by(species, period, type) %>% 
  filter(max(pop)>0) -> exceedances

exceedances


raster('~/../Downloads/V5GL03.HybridPM25c_0p10.Global.201901-201912.nc') -> pm25
get_adm(3, 'low') %>% subset(NAME_3=='Lephalale') -> lepha_adm
point_sources %>% st_drop_geometry() %>% to_spdf %>% raster::extract(pm25, .)



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
