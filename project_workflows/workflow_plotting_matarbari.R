# Set up the environment
# remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
# devtools::reload(pkgload::inst("creapuff"))
library(creapuff) 
require(tidyverse)
library(readxl)


for (i_Sc in seq(1,3)) {
  # Parameters ###################################################################
  # ============================= Project specific ===============================
  # Reference scenario
  scenario_prefix_ref<- "matar1e"
  
  # Select macro scenarios
  if (i_Sc==1) {
    scenario_prefix1 <- "matar1e"
    scenario_prefix2 <- "matar2e"
    scenario_prefix <- "matar12e"
    scenario_description='Matarbari CFPP, phase 1 and 2'}
  if (i_Sc==2) {scenario_prefix <- "matar1e"; scenario_description='Matarbari CFPP, phase 1'}
  if (i_Sc==3) {scenario_prefix <- "matar2e"; scenario_description='Matarbari CFPP, phase 2'}

  
  
  # project_dir="Z:/"                # network disk (project data)
  project_dir="H:/ESIA"     # calpuff_external_data-3 persistent disk (project data)
  
  output_dir <- file.path(project_dir, "calpuff_suite") # Where to write all generated files
  emissions_dir <- file.path(project_dir, "emissions") # Directory where arbitrary-varying emission files are stored
  input_xls <- file.path(emissions_dir, paste0("coordinates_Matarbari_2.xlsx")) # Where plant positions are reported
  input_xls_ref <- input_xls
  
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
  
  
  if (i_Sc==1) {
    calpuff_files1 <- get_calpuff_files(ext=paste0(scenario_prefix1,".tif"), gasunit = 'ug', dir=output_dir, hg_scaling=1e-3)
    calpuff_files2 <- get_calpuff_files(ext=paste0(scenario_prefix2,".tif"), gasunit = 'ug', dir=output_dir, hg_scaling=1e-3)

    for (row in 1:nrow(calpuff_files1)) {
      rs = stack(calpuff_files1[row,1], calpuff_files2[row,1])
      rs1 <- calc(rs, sum)
      new_filename <- calpuff_files1[row,1] %>% gsub("\\matar1e\\b", scenario_prefix,.)
      writeRaster(rs1, new_filename, format="GTiff",overwrite=T)
      }
  } 
    
  calpuff_files <- get_calpuff_files(ext=paste0(scenario_prefix,".tif"), gasunit = 'ug', dir=output_dir, hg_scaling=1e-3)
  
  # Read list of modeled plants
  # plants <- read_csv('F:/TAPM/Drax/stations.txt')
  # plants <- read_csv(input_xls)
  plants <- read_xlsx(input_xls_ref, sheet='CALPUFF input')
  
  # target_crs = get_utm_proj(UTMZ, UTMH, units = 'km')
  
  # plants %<>% rename(x=easting, y=northing) %>% to_spdf(crs=get_utm_proj(UTMZ, UTMH, units = 'm')) %>% 
  # spTransform(target_crs)
  # plants %<>% rename(x=Long, y=Lat) %>% to_spdf(crs="+proj=longlat +datum=WGS84") %>% 
  # spTransform(target_crs)
  
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
  
  plotting_square_length_in_km <- 1400
  # plot_bb <- plants %>% extent %>% magrittr::add(plotting_square_length_in_km)
  # cities <- get_cities(plot_bb, grids)
  # cities$pos[cities$name == 'Kingston upon Hull'] <- 4
  
  # I set max values, to use them, put colorkeybasis=TRUE instead of NULL
  calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="pm" ]    = 2.  # fly ash, [kg/ha/yr], deposition
  calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="hg" ]    = 150    # mercury, [mg/ha/yr], deposition
  calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="so2eq" ] = 40    # acid,    [kg/ha/yr SO2-equivalent], deposition
  calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="no2" ]   = 0.6  # [ug/m3], concentrations
  calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="pm25" ]  = 0.25   # [ug/m3]
  calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="tpm10" ] = 0.25   # [ug/m3]
  calpuff_files$k[calpuff_files$period=="annual" & calpuff_files$species=="so2" ]   = 1.0  # [ug/m3]
  
  calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="no2" ]   = 12   # [ug/m3]
  calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="pm25" ]  = 7    # [ug/m3]
  calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="tpm10" ] = 7    # [ug/m3]
  calpuff_files$k[calpuff_files$period=="daily"  & calpuff_files$species=="so2" ]   = 20    # [ug/m3]
  
  calpuff_files$k[calpuff_files$period=="hourly" & calpuff_files$species=="no2" ]   = 70    # [ug/m3]
  calpuff_files$k[calpuff_files$period=="hourly" & calpuff_files$species=="pm25" ]  = 35    # [ug/m3]
  calpuff_files$k[calpuff_files$period=="hourly" & calpuff_files$species=="so2" ]   = 100    # [ug/m3]
  
  #output plots and exposure results
  plot_results(calpuff_files,
               scenario_names = paste0(scenario_description),  # , ' CFPPs'),
               dir=output_dir, 
               plants=plants,
               # cities=cities,
               plot_km=c(plotting_square_length_in_km,plotting_square_length_in_km),
               colorkeybasis=TRUE,  # NULL,
               plant_names='Matarbari CFPP',
               # plant_names=plants@data$Plants,
               zipping_function=zipping_function,
               filename_suffix=paste0("_",scenario_prefix),
               outputs=c("expPop","png"),
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

