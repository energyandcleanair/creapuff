# Set up the environment
# remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
# devtools::reload(pkgload::inst("creapuff"))
library(creapuff) 
require(tidyverse)
library(magrittr)
library(readxl)


# Parameters ###################################################################
# ============================= Project specific ===============================
# Reference scenario
scenario_prefix_ref<- "ScA"

# Select macro scenario
scenario_prefix <- "ScA" ; scenario_description='Jambi1 & Jambi2, 2008 standards (SO2 upper limit)'
# scenario_prefix <- "ScB" ; scenario_description='Jambi1 & Jambi2, 2008 standards'
# scenario_prefix <- "ScC" ; scenario_description='Jambi1 & Jambi2, 2019 standards'

project_dir="H:/indonesia"      # calpuff_external_data-2 persistent disk (project data)
# project_dir="H:/cambodia"       # calpuff_external_data-2 persistent disk (project data)
# project_dir="G:/chile"        # calpuff_external_data persistent disk (project data)
# project_dir="Z:/chile"        # network disk (project data)
output_dir <- file.path(project_dir, "calpuff_suite") # Where to write all generated files
emissions_dir <- file.path(project_dir, "emissions") # Directory where arbitrary-varying emission files are stored
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
calpuff_files <- get_calpuff_files(ext=paste0(tolower(scenario_prefix),".csv"), gasunit = 'ug', dir=output_dir, hg_scaling=1)#e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)

# Select data and make tif
# calpuff_files %>% filter(period=='annual' | !is.na(threshold)) %>% make_tifs(grids = grids)  # Original filtering
# calpuff_files %<>% slice(grep(tolower(scenario_prefix), calpuff_files$name)) %>% make_tifs(grids = grids)
calpuff_files %>% make_tifs(grids = grids)

# Select tif data 
calpuff_files <- get_calpuff_files(ext=paste0(tolower(scenario_prefix),".tif"), gasunit = 'ug', dir=output_dir, hg_scaling=1)#1e-3)
# calpuff_files <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=output_dir)

# Read list of modeled plants
# plants <- read_csv('F:/TAPM/Drax/stations.txt')
# plants <- read_csv(input_xls)
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

# I set max values, to use them, put colorkeybasis=TRUE
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="pm" ]    = 0.15 # fly ash, [kg/ha/yr], deposition
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="hg" ]    = 625  # mercury, [mg/ha/yr], deposition
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="so2eq" ] = 32   # acid,    [kg/ha/yr SO2-equivalent], deposition
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="no2" ]   = 0.25 # [ug/m3], concentrations
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="pm25" ]  = 2.4  # [ug/m3]
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="tpm10" ] = 2.3  # [ug/m3]
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="so2" ]   = 1.3  # [ug/m3]
calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="no2" ]   = 1.5  # [ug/m3]
calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="pm25" ]  = 17   # [ug/m3]
calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="tpm10" ] = 18   # [ug/m3]
calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="so2" ]   = 17   # [ug/m3]
calpuff_files$k[calpuff_files$period=="hourly" & calpuff_files$species=="no2" ]   = 9    # [ug/m3]
calpuff_files$k[calpuff_files$period=="hourly" & calpuff_files$species=="pm25" ]  = 90   # [ug/m3]
calpuff_files$k[calpuff_files$period=="hourly" & calpuff_files$species=="so2" ]   = 70   # [ug/m3]

#output plots and exposure results
plot_results(calpuff_files,
             scenario_names = paste0(scenario_description),  # , ' CFPPs'),  # Figure's title
             dir=output_dir, 
             plants=plants,
             cities=cities,
             plot_km=c(800,800),
             colorkeybasis=TRUE,
             # plant_names='Drax',
             # plant_names=plants@data$Plants,
             zipping_function=zipping_function,
             filename_suffix=paste0("_",scenario_prefix),
             outputs=c('png','expPop'),
             # outputs=c("png", "kml", "expPop", "cityconcs"),
)

browser()

#get WDPA protected areas
grids_wdpa <- grids
grids_wdpa$gridR %<>% (function(x) {crop(x, extent(x)*.33)})
get_wdpa_for_grid(grids_wdpa) -> wdpa_areas
saveRDS(file.path(output_dir, 'WDPA areas.RDS'))

#output deposition results
get_deposition_results(calpuff_files, dir=output_dir, wdpa_areas=wdpa_areas) -> depo

