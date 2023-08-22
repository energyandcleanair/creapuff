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
project_dir="G:/IndonesiaIESR"       # calpuff_external_data-2 persistent disk (project data)

load(file.path(project_dir, 'CALPUFF_setup.RData'))

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

#make tifs
#calpuff_files %>% filter(period=='annual', species %in% c('so2', 'no2', 'ppm25', 'so4', 'no3')) %>% make_tifs(grids = grids, overwrite = F)
#calpuff_files %>% filter(period=='annual', species %in% c('so2', 'no2', 'ppm25', 'so4', 'no3')) %>% split(1:8) -> tif_batches
calpuff_files %>% filter(species %in% c('so2', 'no2', 'ppm25', 'so4', 'no3'), !is.na(threshold) | period=='daily', period!='annual') %>% split(1:4) -> tif_batches

require(doFuture)
require(foreach)
registerDoFuture()
future::plan("multisession", workers = 4)

foreach (i = seq_along(tif_batches)) %dopar% ({
  tif_batches[[i]] %>% make_tifs(grids = grids, overwrite = F)
})

#scale and sum up concentrations based on a scenario
calpuff_files_orig <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>% 
  mutate(emitted_species=case_when(species %in% c('so2', 'so4')~'SOx',
                                   species %in% c('no2','no3')~'NOx',
                                   species=='ppm25'~'PM'),
         cluster=scenario)

source('project_workflows/read_IESR_emissions.R')
emis %<>% rename(emitted_species=pollutant)

get_scaled_raster <- function(cluster_emissions, species_out) {
  if(species_out=='pm25') species_out=c('so4', 'no3', 'ppm25')
  
  cluster_emissions %<>% group_by(cluster, emitted_species) %>% summarise(across(emissions_t, sum)) %>% left_join(modeled_emissions)
  
  calpuff_files_orig %>% filter(species %in% species_out) %>% inner_join(cluster_emissions) %>% mutate(scaling = emissions_t / modeled_emissions) ->
    scaling
  
  r <- scaling$path[1] %>% raster() %>% raster()
  r[] <- 0
  
  for(i in seq_along(scaling$path)) {
    message(scaling$name[i])
    scaling$path[i] %>% raster %>% multiply_by(scaling$scaling[i]) %>% add(r) -> r 
  }
    
  
  return(r)
}

emis %>% ungroup %>% 
  filter(grepl("1\\.5|PERPRES.*2022$|APC", scenario), year %in% c(2022, 2035)) %>% #, 2030, 2040
  distinct(emission_scenario=scenario, year) %>% 
  full_join(expand_grid(species = c('pm25', 'so2', 'no2'), hr=c(1,24,8760)), by=character()) %>% 
  filter(hr!=1 | species != 'pm25') %>% 
  mutate(scenario=paste0('s', as.numeric(as.factor(emission_scenario)))) ->
  scens

overwrite=F
scens %>% group_by(across(everything())) %>% 
  group_walk(function(df, group) {
    message(group)
    outfile <- file.path(output_dir, paste0('rank(0)_', group$species, '_',group$hr,'hr_conc_', group$scenario, 'y', group$year, '.tif'))
    if(!file.exists(outfile) | overwrite) {
      emis %>% filter(scenario==group$emission_scenario, year==group$year) %>% arrange(cluster) %>% get_scaled_raster(group$species) %>% 
        writeRaster(outfile, overwrite=overwrite)
    }
  })

# Select tif data 
calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=output_dir, hg_scaling=1e-3) %>% 
  separate(scenario, c('scenario_number', 'year'), sep='y', remove=F) %>% 
  left_join(scens %>% distinct(scenario_number=scenario, emission_scenario)) %>% 
  filter(year>2023 | emission_scenario == "PERPRES 112/2022") %>% 
  mutate(subtitle=paste0(ifelse(year<=2023, 'in ', paste0(emission_scenario, ' scenario, ')), year),
         scenario_description='all coal power plants in Indonesia') %>% 
  mutate(title=make_titletxt(., line_break = F),
         subtitle_facets=paste0('by scenario'))



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
getplants <- function(calpuff_files) {
  emis %>% filter(scenario == unique(calpuff_files$emission_scenario),
                  COD<=unique(calpuff_files$year),
                  year_retire>=unique(calpuff_files$year)) %>% 
    distinct(CFPP.name, .keep_all = T) -> plants_out
  
  if(nrow(plants_out)==0) { plants_out <- NULL
  } else {
    plants_out %<>% 
      group_by(Latitude, Longitude) %>% summarise(across(MW, sum)) %>% 
      to_spdf %>% st_as_sf
  }
    
  return(plants_out)  
}

require(ggspatial); require(ggmap); require(ggrepel)
register_google(readLines("~/google_api_key.txt"))

emissions_data %>% to_spdf %>% st_as_sf -> point_sources_to_plot

plot_bb = point_sources_to_plot %>% st_transform(crs=4326) %>% extent() %>% add(2)
#plot_bb[1:2] %<>% add(.2)
#plot_bb[3:4] %<>% add(-.3)
basemap <- get_basemap(plot_bb, zoom=4)

plot_bb %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> plot_bb_polygon
ggmap(basemap) + layer_spatial(plot_bb_polygon, fill=NA, linewidth=2, color='red')

color_scale_basis_scenario <- calpuff_files_all %>% filter(year==2022) %>% distinct(scenario) %>% unlist

calpuff_files_all %>% #filter(scenario==color_scale_basis_scenario) %>% head(1) %>% 
  #filter(period=='daily', species=='pm25') %>% 
  filter(year==2022) %>% 
  mutate(across(starts_with('threshold'), ~round(.x,0))) %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='both',
                color_scale_basis_scenario=color_scale_basis_scenario,
                point_sources=getplants,
                basemap=basemap,
                label_contours=F,
                source_marker_linewidth=.25, source_marker_size=.7, source_marker_alpha=.8,
                label_sources=F,
                output_dir=output_dir)


calpuff_files_all %>% 
  filter(year==2035, grepl('BAU$|PERPRES.*2022$|1\\.5 degrees$|excluding captive$', emission_scenario)) %>% 
  filter(period=='daily', species=='pm25') %>% 
  mutate(across(starts_with('threshold'), ~round(.x,0))) %>% 
  mutate(subtitle_facets=emission_scenario, 
         subtitle=paste0('by scenario in ', unique(year)),
         subtitle_facets=factor(subtitle_facets, levels=sort(unique(subtitle_facets))[c(3,2,1)])) %>% 
  group_by(period, species, type) %>% 
  #filter(cur_group_id()==1) %>% 
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
                plot_dpi = 200, plot_width=10, plot_height = 10)


calpuff_files_all %>% 
  filter(year==2035, grepl('PERPRES.*2022($| /w APC)|1\\.5 degrees($| /w APC)', emission_scenario)) %>% 
  mutate(across(starts_with('threshold'), ~round(.x,0))) %>% 
  mutate(subtitle_facets=emission_scenario, 
         subtitle=paste0('by scenario in ', unique(year), ', with the effect of APC'),
         subtitle_facets=factor(subtitle_facets, levels=sort(unique(subtitle_facets))[c(3,4,1,2)])) %>% 
  group_by(period, species, type) %>% 
  #filter(cur_group_id()==1) %>% 
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
                plot_dpi = 200, plot_width=10, plot_height = 12)

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
