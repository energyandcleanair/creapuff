#set up the environment
remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
library(creapuff)
require(tidyverse)

output_dir <- "F:/TAPM/Sekong/results" # Where to write all generated files
creahia::set_env('gis_dir',"~/GIS/")
Sys.setenv(gis_dir="~/GIS/")

setwd(get_gis_dir())
system("gsutil rsync -r gs://crea-data/gis .")

#list calpuff csv files
calpuff_files <- creapuff::get_calpuff_files(ext=".csv", gasunit = 'ug', dir=output_dir)

#set up plotting grid and output interpolated rasters
calpuff_files %>% get_grids_calpuff(utm_zone = 48, utm_hem = 'N') -> grids
calpuff_files %>% make_tifs(grids=grids)

#list generated tif files
calpuff_files <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=output_dir)

#output new deposition rasters as sum of existing ones
calpuff_files %>% dplyr::filter(type=='deposition', !grepl('sekong', scenario)) %>% 
  dplyr::group_by(species, period) %>% 
  dplyr::group_walk(function(df, ...) {
    df$path[1] %>% basename %>% gsub(df$scenario[1], 'sekong_a', .) %>% 
      file.path(output_dir, .) -> output_file
    message(output_file)
    df$path %>% stack %>% sum %>% writeRaster(output_file, overwrite=T)
  })

#list generated tif files
calpuff_files <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=output_dir)

#read list of modeled plants
plants <- readxl::read_xlsx('~/CALPUFF/Sekong coal plant emissions data.xlsx', skip=1, n_max=3)

#give names to scenarios
calpuff_files %<>% dplyr::mutate(scenarioName = dplyr::recode(scenario, 
                                                              chmc="CHMC Sekong Coal Power Project",
                                                              epic="EPIC Dak Cheung project",
                                                              phonesac="Phonesack Group Xekong Power Project",
                                                              sekong="All Sekong Coal Power Projects"))

#give names to plants for plotting
plants %<>% dplyr::mutate(label = dplyr::case_when(grepl("EPIC", Project)~"EPIC",
                                                   grepl("CHMC", Project)~"CHMC",
                                                   grepl("Phonesack", Project)~"Phonesack"))

#function to select appropriate plants to include in each plot
get_plants <- function(x, scen) {
  if(scen=='sekong') return(x)
  if(scen!='sekong') subset(x, grepl(scen, Project, ignore.case = T))
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
plot_results(calpuff_files, 
             dir=output_dir, 
             plants=plants,
             plant_names=plants$label,
             get_plants = get_plants,
             zipping_function=zipping_function)

#get WDPA protected areas
wdpa_areas=crea_helpers::get_wdpa(grids)

#output deposition results
get_deposition_results(calpuff_files, dir=output_dir, wdpa_areas=wdpa_areas) -> depo
