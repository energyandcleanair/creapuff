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

project_dir="G:/Indonesia_smelters"       # calpuff_external_data-2 persistent disk (project data)
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
met_dir <- "G:/IndonesiaIESR" # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"plots") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all generated files
emissions_dir <- file.path(project_dir,"emissions") # Directory where arbitrary-varying emission files are stored

plants <- read_csv(file.path(emissions_dir, 'emissions_with_cluster.csv'))



load(file.path(project_dir, 'CALPUFF_setup.RData'))

gis_dir <- "H:/GIS"                         # The folder where we store general GIS data


#load grid parameters
calmet_result <- readRDS(file.path(met_dir, "calpuff_suite","calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

# List generated csv files
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)

grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)
plants %>% to_spdf() %>% spTransform(crs(grids$gridR)) %>% extent %>% add(700) -> plot_bb
grids$gridR %<>% crop(plot_bb)

#make tifs
scenarios_to_plot=calpuff_files$scenario %>% unique %>% grep('^all', ., value=T)
calpuff_files %<>% filter(scenario %in% scenarios_to_plot, 
                          species %in% c('so2', 'no2', 'pm25'), !is.na(threshold) | period=='annual')

calpuff_files %>% make_tifs(grids=grids, overwrite = F)

# Select tif data 
calpuff_files_all <- get_calpuff_files(ext=".tif$", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>% 
  filter(scenario %in% scenarios_to_plot) %>% 
  mutate(scenario_description='all smelters and captive power plants') %>% 
  mutate(title=make_titletxt(., line_break = F), year=force_numeric(scenario))



# ================================ General =====================================

# creahia::set_env('gis_dir',"~/GIS/")
# Sys.setenv(gis_dir="~/GIS/")
#
# setwd(get_gis_dir())
# system("gsutil rsync -r gs://crea-data/gis .")


# Plots ########################################################################
calpuff_files_all$year %>% unique %>% 
  lapply(function(yr) {
    plants %>% filter(Year<yr) %>% mutate(year=yr)
  }) %>% bind_rows() %>% 
  group_by(year, loc_cluster, Commodity_broad) %>% 
  summarise(across(c(Longitude, Latitude), mean),
            type=case_when(all(is.na(fuel))~'smelter', T~'smelter and captive power')) %>% to_sf_points() -> 
  point_sources_to_plot

target_crs <- crs(grids$gridR)


require(ggspatial); require(ggmap); require(ggrepel)
register_google(readLines("~/google_api_key.txt"))

plot_bb = point_sources_to_plot %>% st_transform(crs=4326) %>% extent() %>% add(c(-3.0,2.2,-2.4,2.25))
basemap <- get_basemap(plot_bb, zoom=6)

plot_bb %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> plot_bb_polygon
ggmap(basemap) + layer_spatial(plot_bb_polygon, fill=NA, linewidth=2, color='red') +
  layer_spatial(point_sources_to_plot, color='orange')

color_scale_basis_scenario <- calpuff_files_all %>% #filter(year==2030) %>% 
  distinct(scenario) %>% unlist

year_to_plot=2030

calpuff_files_all %>% filter(year==year_to_plot) %>% 
  mutate(across(starts_with('threshold'), ~round(.x,0)),
         subtitle="") %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='both',
                color_scale_basis_scenario=color_scale_basis_scenario,
                point_sources=point_sources_to_plot %>% filter(year==year_to_plot),
                basemap=basemap,
                label_contours=F,
                skip_labels=0,
                label_sources=F,
                fill_alpha_function = (function(x) x^1*.8),
                output_dir=output_dir)#,
                #quicksave_options = list(scale=1.33))


calpuff_files_all %>% 
  mutate(across(starts_with('threshold'), ~round(.x,0))) %>% 
  mutate(title = make_titletxt(., include_scenario=F, line_break=F),
         subtitle_facets=year, 
         subtitle='attributed to smelters and associated captive power plants, by year') %>% 
  group_by(period, species, type) %>% 
  #filter(cur_group_id()==1) %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='both',
                point_sources=point_sources_to_plot %>% mutate(subtitle_facets=year),
                basemap=basemap,
                facet_by='subtitle_facets',
                #facet_ncol=1,
                include_threshold_as_break=T,
                label_contours = F,
                contour_break_probs=c(0, .85,.95,.995),
                fill_alpha_function = (function(x) x^.25*.4),
                label_sources=F,
                output_dir=output_dir,
                ggplot_theme = theme(legend.position = 'top'),
                contour_guide = guide_legend(nrow=1),
                quicksave_options = list(scale=1.33, logo_hjust=1, height=6))

pop <- raster(creahelpers::get_population_path("gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif"))

grids$gridR <- calpuff_files_all$path[1] %>% raster %>% raster

popC <- crop(pop,grids$gridLL)
popC[is.na(popC)] <- 0
popC %<>% aggregate(4)
popUTM <- projectRaster(popC,crs = CRS(proj4string(grids$gridR)))
popD_CP <- resample(popUTM,grids$gridR)
pop <- popD_CP  * area(popD_CP)
names(pop) <- "pop"

pop %>% writeRaster(file.path(input_dir, 'population_for_grid.grd'), overwrite=T)

calpuff_files_all %>% 
  filter(!is.na(threshold)) %>% 
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

exceedances %>% write_csv(file.path(output_dir, paste0('exceedances.csv')))



# ==============================================================================
emis %>% filter(scenario=='BAU', year==2023, emitted_species=='Hg', CFPP.name %in% unlist(cases$units))

#get WDPA protected areas
grids_wdpa <- grids
#grids_wdpa$gridR %<>% (function(x) {crop(x, extent(x)*.33)})
get_wdpa_for_grid(grids_wdpa) -> wdpa_areas
saveRDS(file.path(output_dir, 'WDPA areas.RDS'))

#output deposition results
calpuff_files_all %>% get_deposition_results(dir=output_dir, wdpa_areas=wdpa_areas) -> depo

depo$by_landuse %>% filter(broad.cat != 'ocean') %>% group_by(pollutant, scenario, unit) %>% summarise(across(deposition, sum))
depo$into_protected_areas %>% 
  rename(wdpa_area=name) %>% 
  pivot_longer(-wdpa_area) %>% 
  mutate(species = name %>% gsub('_.*', '', .),
         variable = name %>% gsub('.*_', '', .),
         scenario = name %>% gsub('^[a-z]*_', '', .) %>% gsub('_.*', '', .)) ->
  wdpa_depo


wdpa_depo %>% filter(scenario=='mnpp') %>% group_by(variable) %>% arrange(desc(value)) %>% slice_max(value, n=5)
