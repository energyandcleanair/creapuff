#set up the environment
remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
library(creapuff)
require(tidyverse)

output_dir <- "F:/TAPM/Phils/case_results" # Where to write all generated files
creahia::set_env('gis_dir',"~/GIS/")
Sys.setenv(gis_dir="~/GIS/")

setwd(get_gis_dir())
system("gsutil rsync -r gs://crea-data/gis .")

#list generated tif files
calpuff_files <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=output_dir)

#read list of modeled plants
plants <- readxl::read_xlsx('~/CALPUFF/PH Coal plants_CEED.xlsx', sheet='Re-Runs', skip=8)
plants$scenario_short = plants$Plant %>% substr(1,3) %>% tolower
calpuff_files$scenario_short = calpuff_files$scenario %>% substr(1,3)
plants <- calpuff_files %>% distinct(scenario, scenario_short) %>% 
  right_join(plants)

plants %>% distinct(scenario_short, scenarioName=Plant) %>% right_join(calpuff_files) ->
  calpuff_files

#function to select appropriate plants to include in each plot
get_plants <- function(x, scen) {
  subset(x, scenario==scen)
}

#function to zip files in case system standard function doesn't work
zipping_function=function(zipfile, files_to_zip) {
  origwd=getwd()
  setwd(unique(dirname(files_to_zip)))
  zip::zip(zipfile, basename(files_to_zip)) -> return_value
  setwd(origwd)
  return(return_value)
}

#output plots and exposure results
calpuff_files %>% 
  plyr::d_ply(plyr::.(scenario),
              function(df) {
                plants_to_plot = get_plants(plants, df$scenario[1])
                plot_results(df, 
                             dir=output_dir, 
                             plants=plants_to_plot,
                             plant_names=plants_to_plot$Plant,
                             zipping_function=zipping_function,
                             outputs=c('png','expPop'),
                             filename_suffix = paste0('_', df$scenario[1]))
              })

#get WDPA protected areas
get_grids_calpuff(calpuff_files) -> grids
get_wdpa_for_grid(grids) -> wdpa_areas

#output deposition results
get_deposition_results(calpuff_files, dir=output_dir, wdpa_areas=wdpa_areas) -> depo
