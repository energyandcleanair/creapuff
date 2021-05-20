remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=F)
library(creapuff)

output_dir <- "F:/TAPM/Sekong/results" # Where to write all generated files
creahia::set_env('gis_dir',"~/GIS/")
Sys.setenv(gis_dir="~/GIS/")

calpuff_files <- creapuff::get_calpuff_files(ext=".csv", gasunit = 'ug', dir=output_dir)

calpuff_files %>% get_grids_calpuff(utm_zone = 48, utm_hem = 'N') -> grids

calpuff_files %>% make_tifs(grids=grids)

calpuff_files <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=output_dir)

calpuff_files %>% dplyr::filter(type=='deposition', !grepl('sekong', scenario)) %>% 
  dplyr::group_by(species, period) %>% 
  dplyr::group_walk(function(df, ...) {
    df$path[1] %>% basename %>% gsub(df$scenario[1], 'sekong_a', .) %>% 
      file.path(output_dir, .) -> output_file
    message(output_file)
    df$path %>% stack %>% sum %>% writeRaster(output_file, overwrite=T)
  })

calpuff_files <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=output_dir)

plants <- readxl::read_xlsx('~/CALPUFF/Sekong coal plant emissions data.xlsx', skip=1, n_max=3)

calpuff_files %<>% dplyr::mutate(scenarioName = dplyr::recode(scenario, 
                                                              chmc="CHMC Sekong Coal Power Project",
                                                              epic="EPIC Dak Cheung project",
                                                              phonesac="Phonesack Group Xekong Power Project",
                                                              sekong="All Sekong Coal Power Projects"))

plants %<>% dplyr::mutate(label = dplyr::case_when(grepl("EPIC", Project)~"EPIC",
                                                   grepl("CHMC", Project)~"CHMC",
                                                   grepl("Phonesack", Project)~"Phonesack"))

get_plants <- function(x, scen) {
  if(scen=='sekong') return(x)
  if(scen!='sekong') subset(x, grepl(scen, Project, ignore.case = T))
}

zipping_function=function(zipfile, files_to_zip) {
  origwd=getwd()
  setwd(unique(dirname(files_to_zip)))
  zip::zip(zipfile, basename(files_to_zip)) -> return_value
  setwd(origwd)
  return(return_value)
}

plot_results(calpuff_files, 
             dir=output_dir, 
             plants=plants,
             plant_names=plants$label,
             get_plants = get_plants,
             outputs='expPop',
             zipping_function=zipping_function)

get_deposition_results(calpuff_files, dir=output_dir, wdpa_areas=NULL) -> depo
