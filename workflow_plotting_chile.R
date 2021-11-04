# Set up the environment
# remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
# devtools::reload(pkgload::inst("creapuff"))
library(creapuff) 
require(tidyverse)
library(readxl)


# Parameters ###################################################################
# ============================= Project specific ===============================
# Reference scenario
scenario_prefix_ref<- "ScAll"

# Select macro scenario
# scenario_prefix <- "ScAll" ; scenario_description='Coal and Gas FPPs (2019)'
# scenario_prefix <- "ScB" ; scenario_description='Coal and Gas FPPs (SPS-2025)'
scenario_prefix <- "ScC" ; scenario_description='Gas FPPs (LPS-2040)'

project_dir="G:/projects/chile"        # calpuff_external_data persistent disk (project data)
# project_dir="Z:/projects/chile"        # calpuff_external_data persistent disk (project data)
output_dir <- file.path(project_dir, "calpuff_suite") # Where to write all generated files
# output_dir <- "F:/TAPM/Drax/" # Where to write all generated files
emissions_dir <- file.path(project_dir, "emissions/2019") # Directory where arbitrary-varying emission files are stored
input_xls <- file.path(emissions_dir, paste0("coordinates_",scenario_prefix,".xlsx")) # Where plant positions are reported
input_xls_ref <- file.path(emissions_dir, paste0("coordinates_",scenario_prefix_ref,".xlsx")) # Where plant positions are reported

# ================================ General =====================================
gis_dir <- "F:/gis"                         # The folder where we store general GIS data

# creahia::set_env('gis_dir',"~/GIS/")
# Sys.setenv(gis_dir="~/GIS/")
#
# setwd(get_gis_dir())
# system("gsutil rsync -r gs://crea-data/gis .")


# Plots ########################################################################
# Load CALMET parameters
calmet_result <- readRDS(file.path(output_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

# List generated tif files
calpuff_files <- get_calpuff_files(ext=paste0(tolower(scenario_prefix),".csv"), gasunit = 'ug', dir=output_dir)
# calpuff_files <- get_calpuff_files(ext=paste0(tolower(scenario_prefix),".csv"), gasunit = 'ug', dir=output_dir, hg_scaling=1e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)

# Select data and make tif
# calpuff_files %>% filter(period=='annual' | !is.na(threshold)) %>% make_tifs(grids = grids)  # Original filtering
# calpuff_files %<>% slice(grep(tolower(scenario_prefix), calpuff_files$name)) %>% make_tifs(grids = grids)
calpuff_files %>% make_tifs(grids = grids)

# Select tif data 
calpuff_files <- get_calpuff_files(ext=paste0(tolower(scenario_prefix),".tif"), gasunit = 'ug', dir=output_dir)
# calpuff_files <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=output_dir)

# Read list of modeled plants
# plants <- read_csv('F:/TAPM/Drax/stations.txt')
# plants <- read_csv(input_xls)
# plants <- read_xlsx(input_xls, sheet='CALPUFF input')
plants <- read_xlsx(input_xls_ref, sheet='CALPUFF input')

target_crs = get_utm_proj(UTMZ, UTMH, units = 'km')

# plants %<>% rename(x=easting, y=northing) %>% to_spdf(crs=get_utm_proj(UTMZ, UTMH, units = 'm')) %>% 
  # spTransform(target_crs)
plants %<>% rename(x=Long, y=Lat) %>% to_spdf(crs="+proj=longlat +datum=WGS84") %>% 
  spTransform(target_crs)

#function to select appropriate plants to include in each plot
# get_plants <- function(x, scen) {x}

#function to zip files in case system standard function doesn't work
zipping_function=function(zipfile, files_to_zip) {
  origwd=getwd()
  setwd(unique(dirname(files_to_zip)))
  zip::zip(zipfile, basename(files_to_zip)) -> return_value
  setwd(origwd)
  return(return_value)
}

plot_bb <- plants %>% extent %>% magrittr::add(400)
cities <- get_cities(plot_bb, grids)
# cities$pos[cities$name == 'Kingston upon Hull'] <- 4

# I set max values, using colorkeybasis=TRUE
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="no2" ] = 1.3 #2.0
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="pm25" ] = 0.9 #
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="tpm10" ] = 1.2 #1.25
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="so2" ] = 2.0 #3.0
calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="no2" ] = 13 #30
calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="pm25" ] = 9
calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="tpm10" ] = 15 #18
calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="so2" ] = 25 #45
calpuff_files$k[calpuff_files$period=="hourly" & calpuff_files$species=="no2" ] = 150 #200
calpuff_files$k[calpuff_files$period=="hourly" & calpuff_files$species=="pm25" ] = 30 #70
calpuff_files$k[calpuff_files$period=="hourly" & calpuff_files$species=="so2" ] = 100 #350

# calpuff_files %<>% filter(calpuff_files$species=="no2")

#output plots and exposure results
plot_results(calpuff_files,
             scenario_names = paste0(scenario_description),
             dir=output_dir, 
             plants=plants,
             cities=cities,
             colorkeybasis=TRUE,
             # plant_names='Drax',
             # plant_names=plants@data$Plants,
             zipping_function=zipping_function,
             filename_suffix=paste0("_",scenario_prefix),
             outputs=c('png','expPop'),
)

browser()

#get WDPA protected areas
grids_wdpa <- grids
grids_wdpa$gridR %<>% (function(x) {crop(x, extent(x)*.33)})
get_wdpa_for_grid(grids_wdpa) -> wdpa_areas
saveRDS(file.path(output_dir, 'WDPA areas.RDS'))

#output deposition results
get_deposition_results(calpuff_files, dir=output_dir, wdpa_areas=wdpa_areas) -> depo

