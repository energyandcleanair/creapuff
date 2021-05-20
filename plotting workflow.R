remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=F)
library(creapuff)

require(tidyverse)
output_dir <- "F:/TAPM/Sekong/results" # Where to write all generated files
creahia::set_env('gis_dir',"~/GIS/")
Sys.setenv(gis_dir="~/GIS/")

calpuff_files <- creapuff::get_calpuff_files(ext=".tif", gasunit = 'ug', dir=output_dir)
plants <- readxl::read_xlsx('~/CALPUFF/Sekong coal plant emissions data.xlsx', skip=1, n_max=3)

calpuff_files %<>% mutate(scenarioName = recode(scenario, 
                                                chmc="CHMC Sekong Coal Power Project",
                                                epic="EPIC Dak Cheung project",
                                                phonesac="Phonesack Group Xekong Power Project",
                                                sekong="All Sekong Coal Power Projects"))

require(rasterVis)
plot_results(calpuff_files, 
             dir=output_dir, 
             plants=plants,
             plant_names=plants$Project,
             queue=1)
