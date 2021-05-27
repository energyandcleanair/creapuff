#set up the environment
remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
library(creapuff)
require(tidyverse)

output_dir <- "F:/TAPM/Drax/" # Where to write all generated files
creahia::set_env('gis_dir',"~/GIS/")
Sys.setenv(gis_dir="~/GIS/")

#setwd(get_gis_dir())
#system("gsutil rsync -r gs://crea-data/gis .")

#list generated tif files
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=output_dir)
grids = get_grids_calpuff(calpuff_files, 30, 'N', map_res=1)
calpuff_files %>% filter(period=='annual' | !is.na(threshold)) %>% make_tifs(grids = grids)

calpuff_files <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=output_dir)

#read list of modeled plants
plants <- read_csv('F:/TAPM/Drax/stations.txt')
target_crs = get_utm_proj(30, 'N', units = 'km')
plants %<>% rename(x=easting, y=northing) %>% to_spdf(crs=get_utm_proj(30, 'N', units = 'm')) %>% 
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
cities$pos[cities$name == 'Kingston upon Hull'] <- 4

#output plots and exposure results
plot_results(calpuff_files,
             scenario_names = 'Drax Power Station',
             dir=output_dir, 
             plants=plants,
             cities=cities,
             plant_names='Drax',
             zipping_function=zipping_function,
             #outputs=c('png','expPop'),
)

#get WDPA protected areas
get_wdpa_for_grid(grids) -> wdpa_areas

#output deposition results
get_deposition_results(calpuff_files, dir=output_dir, wdpa_areas=wdpa_areas) -> depo
