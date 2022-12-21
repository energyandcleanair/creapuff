# Set up the environment
# remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
# devtools::reload(pkgload::inst("creapuff"))
library(creapuff) 
require(tidyverse)
library(readxl)

# Parameters ###################################################################
# ============================= Project specific ===============================
project_dir="I:/koreasteel"       # calpuff_external_data-2 persistent disk (project data)
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"plots") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files

# Select tif data 
calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=input_dir, hg_scaling=1e-3) %>% 
  mutate(scenario_description = case_when(scenario=="gwan"~'POSCO Gwangyang steel plant',
                                          scenario=="hyundai"~'Hyundai steel plant',
                                          scenario=="krsteel"~'All integrated steel plants',
                                          scenario=="pohang"~'POSCO Pohang steel plant'))

calpuff_files_all %>% write_csv(file.path(input_dir, 'file_info.csv'))

emissions_dir <- file.path(project_dir, "emissions")  # Directory where arbitrary-varying emission files are stored
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
grids <- get_grids_calpuff(calpuff_files_all)
target_crs <- crs(grids$gridR)


plants_all <- read_csv(file.path(emissions_dir, 'emissions inputs.csv')) %>% 
  group_by(Plant) %>% summarise(across(c(Lat, Long), mean)) %>% 
  to_spdf(crs="+proj=longlat +datum=WGS84") %>% spTransform(target_crs)

#function to select appropriate plants to include in each plot
get_plants <- function(scen) {plants_all %>% subset(grepl(scen, Plant, ignore.case = T) | scen=='krsteel')}


for(run in unique(calpuff_files_all$scenario)) {
  
  
  #function to zip files in case system standard function doesn't work
  zipping_function=function(zipfile, files_to_zip) {
    origwd=getwd()
    setwd(unique(dirname(files_to_zip)))
    zip::zip(zipfile, basename(files_to_zip)) -> return_value
    setwd(origwd)
    return(return_value)
  }
  
  plants <- get_plants(run)
  
  plotting_square_length_in_km <- ifelse(nrow(plants)==1, 600, 200)
  plot_bb <- plants %>% extent %>% magrittr::add(plotting_square_length_in_km)
  cities <- get_cities(plot_bb, grids)
  
  cities$name %<>% recode(Yosu='Yeosu', Soul='Seoul', Pusan='Busan', Taejon='Daejeon', Kwangju='Gwangju', Cheju='Jeju')
  
  calpuff_files <- calpuff_files_all %>% filter(scenario==run)
  
  #output plots and exposure results
  plot_results(calpuff_files,
               scenario_names = calpuff_files$scenario_description[1],
               dir=output_dir, 
               plants=plants,
               cities=cities,
               plot_km=c(plotting_square_length_in_km,plotting_square_length_in_km),
               colorkeybasis=NULL, # TRUE,  # NULL,
               # plant_names='Pollution from Coal-fired Power Plants in 2022',
               # plant_names=plants@data$Plants,
               zipping_function=zipping_function,
               filename_suffix=paste0("_",scenario_prefix),
               outputs=c("png", "expPop", "kml"), 
  )
  
}


# ==============================================================================
# ... Moving ...
print('... moving output files (png, exceedances, tseries,...)')
# PNG files
png_dir <- file.path(project_dir,"png")
if (!dir.exists(png_dir)) dir.create(png_dir)
file.path(output_dir,list.files(output_dir,"\\.png$" )) -> png_files
file.copy(png_files,png_dir, overwrite = TRUE)
file.remove(png_files)
# KML files
kmz_dir <- file.path(project_dir,"kmz") ; 
if (!dir.exists(kmz_dir)) dir.create(kmz_dir)
file.path(output_dir,list.files(output_dir,"\\.kmz$" )) -> kmz_files
file.copy(kmz_files,kmz_dir, overwrite = TRUE)
file.remove(kmz_files)
# Exeedances
exceedances_dir <- file.path(project_dir,"exceedances")
if (!dir.exists(exceedances_dir)) dir.create(exceedances_dir)
file.path(output_dir,list.files(output_dir,"expPop|threshold_exceedances" )) -> exceedances_files
file.copy(exceedances_files,exceedances_dir, overwrite = TRUE)
file.remove(exceedances_files)
# tseries
tseries_dir <- file.path(project_dir,"tseries")
if (!dir.exists(tseries_dir)) dir.create(tseries_dir)
file.path(output_dir,list.files(output_dir,"tseries" )) -> tseries_files
file.copy(tseries_files,tseries_dir, overwrite = TRUE)
file.remove(tseries_files)

browser()

# ==============================================================================
#get WDPA protected areas
grids_wdpa <- grids
grids_wdpa$gridR %<>% (function(x) {crop(x, extent(x)*.33)})
get_wdpa_for_grid(grids_wdpa) -> wdpa_areas
saveRDS(file.path(output_dir, 'WDPA areas.RDS'))

#output deposition results
get_deposition_results(calpuff_files, dir=output_dir, wdpa_areas=wdpa_areas) -> depo

