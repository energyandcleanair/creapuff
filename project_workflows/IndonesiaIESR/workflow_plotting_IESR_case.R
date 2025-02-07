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

case_units = emis %>% filter(grepl('Bengkulu|Nagan Raya|Pangkalan Susu', CFPP.name)) %>% use_series(CFPP.name) %>% unique
cases=tibble(units=list(case_units),
             scenario_description=c('PLTU Teluk Sepang, Nagan Raya and Pangkalan Susu'),
             case_name=c('sumatra-plants'))

u1 = "Cirebon Unit 1"
u2 = "Jawa-1 / Cirebon-2"
cases=tibble(units=list(u1,u2, c(u1, u2)),
             scenario_description=c('PLTU Cirebon 1','PLTU Cirebon 2','PLTU Cirebon 1 and 2'),
             case_name=c('cirebon1', 'cirebon2', 'cirebon12'))


cases=tibble(units=list(u1,u2, c(u1, u2)),
             scenario_description=c('PLTU Cirebon 1','PLTU Cirebon 2','PLTU Cirebon 1 and 2'),
             case_name=c('sumatra-plants'))


# Parameters ###################################################################
# ============================= Project specific ===============================
project_dir="G:/IndonesiaIESR"       # calpuff_external_data-2 persistent disk (project data)

load(file.path(project_dir, 'CALPUFF_setup.RData'))

gis_dir <- "H:/GIS"                         # The folder where we store general GIS data
source('project_workflows/read_iesr_emissions.R')
emis %<>% rename(emitted_species=pollutant)


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
#calpuff_files %>% filter(species %in% c('so2', 'no2', 'ppm25', 'so4', 'no3'), !is.na(threshold) | period=='daily', period!='annual') %>% 
#  split(1:4) -> tif_batches

emis %>% filter(CFPP.name %in% unlist(cases$units)) %>% distinct(cluster) %>% unlist -> clusters_to_process

calpuff_files %>% 
  filter(scenario %in% clusters_to_process, 
         species %in% c('so2', 'no2', 'ppm25', 'so4', 'no3', 'hg'), !is.na(threshold) | period=='annual') %>% 
  make_tifs(grids=grids, overwrite = F)

#scale and sum up concentrations based on a scenario
calpuff_files_orig <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>% 
  filter(scenario %in% clusters_to_process) %>% 
  mutate(emitted_species=case_when(species %in% c('so2', 'so4')~'SOx',
                                   species %in% c('no2','no3')~'NOx',
                                   species=='ppm25'~'PM',
                                   species=='hg'~'Hg'),
         cluster=scenario)

get_scaled_raster <- function(cluster_emissions, species_out, hr_out, type_out='concentration') {
  if(species_out=='pm25') species_out=c('so4', 'no3', 'ppm25')
  
  cluster_emissions %<>% group_by(cluster, emitted_species) %>% summarise(across(emissions_t, sum)) %>% left_join(modeled_emissions)
  
  calpuff_files_orig %>% filter(species %in% species_out, hr==hr_out, type==type_out) %>% inner_join(cluster_emissions) %>% 
    mutate(scaling = emissions_t / modeled_emissions * case_when(species=='hg'~1e3, T~1)) ->
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
  filter(scenario=='BAU', year == 2022) %>% #, 2030, 2040
  distinct(emission_scenario=scenario, year) %>% 
  full_join(expand_grid(species = c('pm25', 'so2', 'no2', 'hg'), hr=c(1,24,8760)), by=character()) %>% 
  full_join(cases, by=character()) %>% 
  filter(hr!=1 | species != 'pm25', hr==8760 | species!='hg') %>% 
  mutate(type=case_when(species=='hg'~'deposition',T~'concentration')) ->
  scens

overwrite=T
scens %>% group_by(across(everything())) %>% 
  group_walk(function(df, group) {
    message(group)
    outfile <- file.path(output_dir, paste0('rank(0)_', group$species, '_',group$hr,'hr_',
                                            case_when(group$type=='deposition'~'tflx', T~'conc'),'_', 
                                            group$case_name, 'y', group$year, '.tif'))
    
    if(!file.exists(outfile) | overwrite) {
      emis %>% 
        filter(scenario==group$emission_scenario, year==pmax(group$year, COD), CFPP.name %in% cases$units[[which(cases$case_name==group$case_name)]]) %>% 
        get_scaled_raster(group$species, group$hr, type_out = group$type) %>% 
        writeRaster(outfile, overwrite=overwrite)
    }
  })

# Select tif data 
calpuff_files_all <- get_calpuff_files(ext=".tif$", gasunit = 'ug', dir=output_dir, hg_scaling=1e-3) %>% 
  separate(scenario, c('scenario_number', 'year'), sep='y', remove=F) %>% 
  inner_join(scens %>% distinct(scenario_number=case_name, emission_scenario, scenario_description)) %>% 
  mutate(title=make_titletxt(., line_break = F))



# ================================ General =====================================

# creahia::set_env('gis_dir',"~/GIS/")
# Sys.setenv(gis_dir="~/GIS/")
#
# setwd(get_gis_dir())
# system("gsutil rsync -r gs://crea-data/gis .")


# Plots ########################################################################
target_crs <- crs(grids$gridR)

#read point_sources from somewhere
emis %>% filter(CFPP.name %in% unlist(cases$units)) %>% 
  distinct(CFPP.name, .keep_all = T) %>% 
  mutate(source = 'PLTU Cirebon') %>% 
  group_by(source) %>% summarise(across(MW, sum), across(c(Latitude, Longitude), mean)) %>% 
  to_spdf %>% st_as_sf -> plants_to_plot

require(ggspatial); require(ggmap); require(ggrepel)
register_google(readLines("~/google_api_key.txt"))

emis %>% filter(CFPP.name %in% unlist(cases$units)) %>% distinct(CFPP.name, .keep_all=T) %>% to_spdf %>% st_as_sf -> point_sources_to_plot

plot_bb = point_sources_to_plot %>% st_transform(crs=4326) %>% extent() %>% add(3.4)
#plot_bb@ymax <- -5.6
#plot_bb@ymin <- -8.1
#plot_bb[1:2] %<>% add(.2)
#plot_bb[3:4] %<>% add(-.3)
basemap <- get_basemap(plot_bb, zoom=7)

plot_bb %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> plot_bb_polygon
ggmap(basemap) + layer_spatial(plot_bb_polygon, fill=NA, linewidth=2, color='red')

color_scale_basis_scenario <- NULL #calpuff_files_all %>% filter(year==2022) %>% distinct(scenario) %>% unlist

calpuff_files_all %>% 
  mutate(across(starts_with('threshold'), ~round(.x,0))) %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='both',
                color_scale_basis_scenario=color_scale_basis_scenario,
                point_sources=plants_to_plot,
                basemap=basemap,
                label_contours=F,
                skip_labels=0,
                label_sources=T,
                fill_alpha_function = (function(x) x^1*.8),
                output_dir=output_dir,
                quicksave_options = list(scale=1.33))


calpuff_files_all %>% #filter(grepl('1-3|2-3', scenario_description)) %>% 
  mutate(across(starts_with('threshold'), ~round(.x,0))) %>% 
  mutate(title = make_titletxt(., include_scenario=F, line_break=F),
         subtitle_facets=scenario_description, 
         subtitle='before and after commissioning of Cirebon 2 and retirement of Cirebon 1') %>% 
  group_by(period, species, type) %>% 
  #filter(cur_group_id()==1) %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='both',
                point_sources=plants_to_plot,
                basemap=basemap,
                facet_by='subtitle_facets',
                facet_ncol=1,
                include_threshold_as_break=T,
                label_contours = F,
                contour_break_probs=c(0, .85,.95,.995),
                fill_alpha_function = (function(x) x^.25*.4),
                label_sources=F,
                output_dir=output_dir,
                quicksave_options = list(scale=1, logo_hjust=1, height=10))

pop <- raster(creahelpers::get_population_path("gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif"))

popC <- crop(pop,grids$gridLL)
popC[is.na(popC)] <- 0
popC %<>% aggregate(4)
popUTM <- projectRaster(popC,crs = CRS(proj4string(grids$gridR)))
popD_CP <- resample(popUTM,grids$gridR)
pop <- popD_CP  * area(popD_CP)
names(pop) <- "pop"
pop %>% writeRaster(file.path(project_dir, 'HIA', 'population_for_hia_grid.grd'))

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

exceedances %>% write_csv(file.path(project_dir, 'HIA', paste0('Cirebon_exceedances.csv')))



# ==============================================================================
emis %>% filter(scenario=='BAU', year==2023, emitted_species=='Hg', CFPP.name %in% unlist(cases$units))

#get WDPA protected areas
grids_wdpa <- grids
grids_wdpa$gridR %<>% (function(x) {crop(x, extent(x)*.33)})
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
