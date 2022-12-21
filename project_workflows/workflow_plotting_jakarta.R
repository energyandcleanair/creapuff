# Set up the environment
# remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
# devtools::reload(pkgload::inst("creapuff"))
library(creapuff) 
require(tidyverse)
library(readxl)


for (i_Sc in seq(1,1)) {
# Parameters ###################################################################
# ============================= Project specific ===============================
# Reference scenario
scenario_prefix_ref<- "ScAll"

# Select macro scenarios
if (i_Sc==1) {scenario_prefix <- "ScAll"; scenario_description='Coal-fired Power Plants in 2022, from Jan to Jun'}

# project_dir="Z:/"      # network disk (project data)
project_dir="H:/jakarta"     # calpuff_external_data-3 persistent disk (project data)

output_dir <- file.path(project_dir, "calpuff_suite") # Where to write all generated files
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
# Load CALMET parameters
calmet_result <- readRDS(file.path(output_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

# List generated cvs files
calpuff_files <- get_calpuff_files(ext=paste0(tolower(scenario_prefix),".csv"), gasunit = 'ug', dir=output_dir, hg_scaling=1e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)

# Select data and make tif
# calpuff_files %>% filter(period=='annual' | !is.na(threshold)) %>% make_tifs(grids = grids)  # Original filtering
# calpuff_files %<>% slice(grep(tolower(scenario_prefix), calpuff_files$name)) %>% make_tifs(grids = grids)
# calpuff_files %>% filter(!is.na(threshold))  %>% make_tifs(grids = grids)
calpuff_files %>% make_tifs(grids = grids)

# Select tif data 
calpuff_files <- get_calpuff_files(ext=paste0(tolower(scenario_prefix),".tif"), gasunit = 'ug', dir=output_dir, hg_scaling=1e-3)
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

plotting_square_length_in_km <- 600
plot_bb <- plants %>% extent %>% magrittr::add(plotting_square_length_in_km)
cities <- get_cities(plot_bb, grids)
# cities$pos[cities$name == 'Kingston upon Hull'] <- 4

# I set max values, to use them, put colorkeybasis=TRUE instead of NULL
# calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="pm" ]    = 0.45  # fly ash, [kg/ha/yr], deposition
# calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="hg" ]    = 30    # mercury, [mg/ha/yr], deposition
# calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="so2eq" ] = 4.5    # acid,    [kg/ha/yr SO2-equivalent], deposition
# calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="no2" ]   = 0.35  # [ug/m3], concentrations
# calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="pm25" ]  = 0.5   # [ug/m3]
# calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="tpm10" ] = 0.5   # [ug/m3]
# calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="so2" ]   = 1.6  # [ug/m3]
# 
# calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="no2" ]   = 4.0   # [ug/m3]
# calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="pm25" ]  = 6    # [ug/m3]
# calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="tpm10" ] = 6    # [ug/m3]
# calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="so2" ]   = 20    # [ug/m3]
# 
# calpuff_files$k[calpuff_files$period=="hourly" & calpuff_files$species=="no2" ]   = 20    # [ug/m3]
# calpuff_files$k[calpuff_files$period=="hourly" & calpuff_files$species=="pm25" ]  = 30    # [ug/m3]
# calpuff_files$k[calpuff_files$period=="hourly" & calpuff_files$species=="so2" ]   = 100    # [ug/m3]

#output plots and exposure results
plot_results(calpuff_files,
             scenario_names = paste0(scenario_description),  # , ' CFPPs'),
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

