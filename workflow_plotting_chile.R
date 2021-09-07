# Set up the environment
# remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
# devtools::reload(pkgload::inst("creapuff"))
library(creapuff) 
require(tidyverse)
library(readxl)


# Parameters ###################################################################

# ============================= Project specific ===============================
project_dir="G:/projects/chile"        # calpuff_external_data persistent disk (project data)
output_dir <- file.path(project_dir,"calpuff_suite") # Where to write all generated files
# output_dir <- "F:/TAPM/Drax/" # Where to write all generated files
emissions_dir <- file.path(project_dir,"emissions/2019") # Directory where arbitrary-varying emission files are stored
input_xls <- file.path(emissions_dir,"coordinates.xlsx") # Where plant positions are reported

# ================================ General =====================================
gis_dir <- "F:/gis"                         # The folder where we store general GIS data
# creahia::set_env('gis_dir',"~/GIS/")
# Sys.setenv(gis_dir="~/GIS/")
#setwd(get_gis_dir())
#system("gsutil rsync -r gs://crea-data/gis .")


# Plots ########################################################################

# Load CALMET parameters
calmet_result <- readRDS(file.path(output_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

# List generated tif files
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=output_dir)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)
# calpuff_files %>% filter(period=='annual' | !is.na(threshold)) %>% make_tifs(grids = grids)  # Why this filter ??

# Select only chileall
calpuff_files %>% slice(grep('chileall', calpuff_files$name)) %>% make_tifs(grids = grids)

calpuff_files <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=output_dir)

# Read list of modeled plants
# plants <- read_csv('F:/TAPM/Drax/stations.txt')
# plants <- read_csv(input_xls)
plants <- read_xlsx(input_xls, sheet='CALPUFF input')

target_crs = get_utm_proj(UTMZ, UTMH, units = 'km')

# plants %<>% rename(x=easting, y=northing) %>% to_spdf(crs=get_utm_proj(UTMZ, UTMH, units = 'm')) %>% 
  # spTransform(target_crs)
plants %<>% rename(x=Long, y=Lat) %>% to_spdf(crs="+proj=longlat +datum=WGS84") %>% 
  spTransform(target_crs)

#function to select appropriate plants to include in each plot
get_plants <- function(x, scen) {x}

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

#output plots and exposure results
plot_results(calpuff_files,
             scenario_names = 'Chile Power Stations',
             dir=output_dir, 
             plants=plants,
             cities=cities,
             # plant_names='Drax',
             # plant_names=plants@data$Plants,
             zipping_function=zipping_function,
             #outputs=c('png','expPop'),
)

browser()

#get WDPA protected areas
grids_wdpa <- grids
grids_wdpa$gridR %<>% (function(x) {crop(x, extent(x)*.33)})
get_wdpa_for_grid(grids_wdpa) -> wdpa_areas
saveRDS(file.path(output_dir, 'WDPA areas.RDS'))

#output deposition results
get_deposition_results(calpuff_files, dir=output_dir, wdpa_areas=wdpa_areas) -> depo

