require(tidyverse)
list.files(path='~/CALPUFF/creapuff/R', full.names=T) %>% sapply(source)

project_dir="G:/IndonesiaIESR"       # calpuff_external_data-2 persistent disk (project data)
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files

emissions_dir <- 'G:/IndonesiaIESR/emissions'
output_dir <- file.path(emissions_dir, 'to Trivia')

emis <- read_csv(file.path(output_dir, 'emissions, COD and retirement for all units and scenarios.csv'))


calpuff_files <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3)

calpuff_files %>% filter(scenario %in% emis$cluster, period=='annual', species %in% c('ppm25', 'no2', 'so2', 'no3', 'so4')) -> tifs_to_share


require(pbapply)
tifs_to_share$path %>% pblapply(function(f) system(paste('gsutil cp', f, 'gs://indonesia-iesr-external-data/concentrations/')))
