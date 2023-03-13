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
#library(creapuff) 
#list.files(path='R', full.names=T) %>% sapply(source)
require(rcrea)
require(creahelpers)


# Parameters ###################################################################
# ============================= Project specific ===============================
#project_dir="I:/SouthAfrica"       # calpuff_external_data-2 persistent disk (project data)
project_dir="C:/Users/lauri/Desktop/My Drive/air pollution/TAPM/2017cases/SouthAfrica2022"
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"plots_MES") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

#load grid parameters
calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

# List generated csv files
input_dir <- file.path(project_dir,"calpuff_suite/MES_aggregated") # Where to read all CALPUFF generated files
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ppb', dir=input_dir, hg_scaling=1e-3) %>% 
  mutate(year=name %>% gsub('.*_', '', .) %>% force_numeric %>% add(2000))

grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)

#make tifs
calpuff_files %>% filter(!is.na(threshold), species != 'tpm10', year==2031) %>% 
  make_tifs(grids = grids, overwrite=T)

# Select tif data 
calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>% 
  mutate(year=name %>% gsub('.*_', '', .) %>% force_numeric %>% add(2000),
         scenario=paste0(scenario, '_', year-2000),
         scenario_description=case_when(grepl('eskom', scenario)~'Eskom plan',
                                       grepl('bat', scenario)~'BAT',
                                       grepl('compl', scenario)~'MES compliance',
                                       grepl('delay', scenario)~'delayed compliance',
                                       grepl('noimp', scenario)~'no improvements'),
         title=make_titletxt(.) %>% gsub(' ?\n?from ', '', .),
         subtitle=paste("In the", scenario_description, "scenario in", year),
         subtitle_facets=paste0('in ', year, ', by scenario'))

emis_byyear_byplant <- file.path(emissions_dir, 'emissions scaling for scenarios_v2.csv') %>% read_csv %>% 
  mutate(calpuff_name = plant %>% substr(1,7) %>% tolower, scaling=emissions/modeled_emissions)


# ================================ General =====================================
gis_dir <- "~/GIS"                         # The folder where we store general GIS data

# creahia::set_env('gis_dir',"~/GIS/")
# Sys.setenv(gis_dir="~/GIS/")
#
# setwd(get_gis_dir())
# system("gsutil rsync -r gs://crea-data/gis .")


# Plots ########################################################################
target_crs <- crs(grids$gridR)

source('../creapuff/project_workflows/emissions_processing_SA.R')
point_sources %>% st_as_sf -> point_sources_to_plot

emis_byyear_byplant %>% group_by(plant) %>% filter(emissions>0) %>% 
  summarise(decommissioning_end=max(year)) -> decommissioning

getplants <- function(calpuff_files) {
  point_sources_to_plot %>% left_join(decommissioning) %>% 
    filter(decommissioning_end>=unique(calpuff_files$year))
}

require(ggspatial); require(ggmap); require(ggrepel)


plot_bb = point_sources_to_plot %>% st_transform(crs=4326) %>% extent() %>% add(6)
basemap <- get_basemap(plot_bb, zoom=6)

plot_bb %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> plot_bb_polygon
ggmap(basemap) + layer_spatial(plot_bb_polygon, fill=NA, linewidth=2, color='red')

calpuff_files_all %>% 
  filter(year==2022) %>% 
  mutate(subtitle='in 2022') %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='both',
                #color_scale_basis_scenario='noimpr_31',
                point_sources=point_sources_to_plot,
                basemap=basemap,
                label_sources=F,
                output_dir=output_dir)


calpuff_files_all %>% 
  filter(year>2030, #period=='annual', species=='no2',
         !grepl('no imp|delay', scenario_description)) %>% 
  mutate(subtitle=subtitle_facets,
         scenario_description=factor(scenario_description, levels=c('Eskom plan', 'MES compliance', 'BAT'))) %>% 
  group_by(period, species, type, year) %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='both',
                point_sources=getplants,
                basemap=basemap,
                facet_by='scenario_description',
                include_threshold_as_break=F,
                label_contours = F,
                contour_break_probs=c(0, .85,.95,.995),
                label_sources=F,
                output_dir=output_dir,
                plot_dpi = 200, plot_width=10, plot_height = 5)



calpuff_files_all %>% 
  filter(year>2030) %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='both',
                point_sources=getplants,
                basemap=basemap,
                label_contours = F,
                include_threshold_as_break = F,
                label_sources=F,
                output_dir=output_dir)


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
