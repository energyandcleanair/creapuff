# Set up the environment
# remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
# devtools::reload(pkgload::inst("creapuff"))
library(creapuff) 
require(tidyverse)
library(readxl)


for (i_Sc in seq(1,16)) {
# Parameters ###################################################################
# ============================= Project specific ===============================
# Reference scenario
scenario_prefix_ref<- "ScA_all"

# Select macro scenarios
if (i_Sc==1) {scenario_prefix <- "ScA_all" ; scenario_description='Maharashtra CFPPs, all units, SO2 compliance, 85% PLF'}
if (i_Sc==2) {scenario_prefix <- "ScA_34"  ; scenario_description='Maharashtra CFPPs, units 3-4, SO2 compliance, 85% PLF'}          
if (i_Sc==3) {scenario_prefix <- "ScA_567" ; scenario_description='Maharashtra CFPPs, units 5-6-7, SO2 compliance, 85% PLF'}            
if (i_Sc==4) {scenario_prefix <- "ScA_89"  ; scenario_description='Maharashtra CFPPs, units 8-9, SO2 compliance, 85% PLF'}           

if (i_Sc==5) {scenario_prefix <- "ScB_all" ; scenario_description='Maharashtra CFPPs, all units, SO2 compliance, actual PLF'}  
if (i_Sc==6) {scenario_prefix <- "ScB_34"  ; scenario_description='Maharashtra CFPPs, units 3-4, SO2 compliance, actual PLF'}  
if (i_Sc==7) {scenario_prefix <- "ScB_567" ; scenario_description='Maharashtra CFPPs, units 5-6-7, SO2 compliance, actual PLF'}  
if (i_Sc==8) {scenario_prefix <- "ScB_89"  ; scenario_description='Maharashtra CFPPs, units 8-9, SO2 compliance, actual PLF'}  

if (i_Sc==9 ){scenario_prefix <- "ScC_all" ; scenario_description='Maharashtra CFPPs, all units, actual SO2, 85% PLF'}                
if (i_Sc==10){scenario_prefix <- "ScC_34"  ; scenario_description='Maharashtra CFPPs, units 3-4, actual SO2, 85% PLF'}                
if (i_Sc==11){scenario_prefix <- "ScC_567" ; scenario_description='Maharashtra CFPPs, units 5-6-7, actual SO2, 85% PLF'}                
if (i_Sc==12){scenario_prefix <- "ScC_89"  ; scenario_description='Maharashtra CFPPs, units 8-9, actual SO2, 85% PLF'}                

if (i_Sc==13){scenario_prefix <- "ScD_all" ; scenario_description='Maharashtra CFPPs, all units, actual SO2, actual PLF'}     
if (i_Sc==14){scenario_prefix <- "ScD_34"  ; scenario_description='Maharashtra CFPPs, units 3-4, actual SO2, actual PLF'}     
if (i_Sc==15){scenario_prefix <- "ScD_567" ; scenario_description='Maharashtra CFPPs, units 5-6-7, actual SO2, actual PLF'}     
if (i_Sc==16){scenario_prefix <- "ScD_89"  ; scenario_description='Maharashtra CFPPs, units 8-9, actual SO2, actual PLF'}     

# project_dir="Z:/"             # network disk (project data)
# project_dir="G:/chile"        # calpuff_external_data   persistent disk (project data)
# project_dir="H:/cambodia"     # calpuff_external_data-2 persistent disk (project data)
# project_dir="H:/indonesia"    # calpuff_external_data-2 persistent disk (project data)
project_dir="I:/india"          # calpuff_external_data-3 persistent disk (project data)

output_dir <- file.path(project_dir, "calpuff_suite") # Where to write all generated files
emissions_dir <- file.path(project_dir, "emissions") # Directory where arbitrary-varying emission files are stored
input_xls <- file.path(emissions_dir, paste0("coordinates_",scenario_prefix,".xlsx")) # Where plant positions are reported
input_xls_ref <- file.path(emissions_dir, paste0("coordinates_",scenario_prefix_ref,".xlsx")) # Where plant positions are reported

png_dir <- file.path(project_dir,"png") ; if (!dir.exists(png_dir)) dir.create(png_dir)
exceedances_dir <- file.path(project_dir,"exceedances") ; if (!dir.exists(exceedances_dir)) dir.create(exceedances_dir)
tseries_dir <- file.path(project_dir,"tserires") ; if (!dir.exists(tseries_dir)) dir.create(tseries_dir)

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
calpuff_files <- get_calpuff_files(ext=paste0(tolower(scenario_prefix),".csv"), gasunit = 'ug', dir=output_dir, hg_scaling=1e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)

# Select data and make tif
# calpuff_files %>% filter(period=='annual' | !is.na(threshold)) %>% make_tifs(grids = grids)  # Original filtering
# calpuff_files %<>% slice(grep(tolower(scenario_prefix), calpuff_files$name)) %>% make_tifs(grids = grids)
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

plot_bb <- plants %>% extent %>% magrittr::add(400)
cities <- get_cities(plot_bb, grids)
# cities$pos[cities$name == 'Kingston upon Hull'] <- 4

# I set max values, to use them, put colorkeybasis=TRUE
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="pm" ]    = 0.15  # fly ash, [kg/ha/yr], deposition
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="hg" ]    = 150   # mercury, [mg/ha/yr], deposition
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="so2eq" ] = 15    # acid,    [kg/ha/yr SO2-equivalent], deposition
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="no2" ]   = 0.25  # [ug/m3], concentrations
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="pm25" ]  = 1.1   # [ug/m3]
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="tpm10" ] = 1.1   # [ug/m3]
calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="so2" ]   = 0.25  # [ug/m3]

calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="no2" ]   = 1.7   # [ug/m3]
calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="pm25" ]  = 19    # [ug/m3]
calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="tpm10" ] = 20    # [ug/m3]
calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="so2" ]   = 12    # [ug/m3]

calpuff_files$k[calpuff_files$period=="hourly" & calpuff_files$species=="no2" ]   = 11    # [ug/m3]
calpuff_files$k[calpuff_files$period=="hourly" & calpuff_files$species=="pm25" ]  = 95    # [ug/m3]
calpuff_files$k[calpuff_files$period=="hourly" & calpuff_files$species=="so2" ]   = 55    # [ug/m3]

#output plots and exposure results
plot_results(calpuff_files,
             scenario_names = paste0(scenario_description),  # , ' CFPPs'),
             dir=output_dir, 
             plants=plants,
             cities=cities,
             plot_km=c(1200,1200),
             colorkeybasis=TRUE,
             # plant_names='Drax',
             # plant_names=plants@data$Plants,
             zipping_function=zipping_function,
             filename_suffix=paste0("_",scenario_prefix),
             outputs=c('png','expPop'),
)

}


# ==============================================================================
# ... Moving ...
print('... moving output files (png, exceedances, tseries,...)')
# PNG files
file.path(output_dir,list.files(output_dir,"\\.png$" )) -> png_files
file.copy(png_files,png_dir, overwrite = TRUE)
file.remove(png_files)
# Exeedances
file.path(output_dir,list.files(output_dir,"expPop|threshold_exceedances" )) -> exceedances_files
file.copy(exceedances_files,exceedances_dir, overwrite = TRUE)
file.remove(exceedances_files)
# tseries
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

