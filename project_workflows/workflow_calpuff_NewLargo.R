# Set up the environment
# remotes::install_github("energyandcleanair/creapuff", dependencies=T, update=F)
# devtools::reload(pkgload::inst("creapuff"))


require(raster)
require(sf)
library(readxl)
library(writexl)
library(lubridate)
library(tidyverse)
library(magrittr)
library(pbapply)
require(creahelpers)
require(ncdf4)

library(creapuff)
library(sf)

#development
#source('R/env.R')
#source('R/helpers.R')
#source('R/01_calmet.R')
#source('R/02_calpuff_corrected.R')
#source('R/03_postprocessing.R')
#source('R/plots.R')



# Parameters ###################################################################
# ============================= Project specific ===============================
expand_grids = '*'  # All grids are expanded (for CALMET)
expand_ncells = -5  # Number of cells to expand met grid (e.g., for WRF data, exclusion of last 5 cells) in each direction (use negative values to crop).

# project_dir="Z:/"                 # network disk (wrf_data). If Z disk is not present: mount 10.58.186.210:/wrf_data Z:)
#project_dir="I:/SouthAfrica"       # calpuff_external_data-2 persistent disk (project data)
project_dir="H:/NewLargo" 

#wrf_dir <- file.path(project_dir,"calwrf") # Where calwrf data are stored 
wrf_dir <- file.path("H:/SouthAfrica/calwrf")

output_dir <- file.path(project_dir,"calpuff_suite") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all generated files

# emission_type = "varying" 
emission_type = "constant"  
emissions_dir <- file.path(project_dir,"emissions") # Directory where arbitrary-varying emission files are stored



# ================================ General =====================================

# BE CAREFUL : gis_dir/landcover/C3S-LC-L4-LCCS-Map-300m-P1Y-2018-v2.1.1.nc is CORRUPETED in the repository !! You should replace it with a good one 
gis_dir <- "H:/gis"                         # The folder where we store general GIS data

bc_dir  <- file.path(gis_dir, "background") # The folder with background atmospheric concentrations for O3, NH3, H2O2

exe_dir="C:/CALPUFF"
calmet_exe <- file.path(exe_dir,"CALMET_v6.5.0_L150223/calmet_v6.5.0.exe")
calpuff_exe <- file.path(exe_dir,"CALPUFF_v7.2.1_L150618/calpuff_v7.2.1.exe")
pu_exe <- file.path(exe_dir,"POSTUTIL_v7.0.0_L150207/postutil_v7.0.0.exe")
calpost_exe <- file.path(exe_dir,"CALPOST_v7.1.0_L141010/calpost_v7.1.0.exe")

template_dir="H:/templates"
calmet_templates <- list(noobs=file.path(template_dir,"CALMET_template.INP"), 
                         surfobs=file.path(template_dir,"CALMET_surfObs_template.inp"))

# calpuff_template <- file.path(template_dir,"CALPUFF_7.0_template.INP")       # No mercury in emission file 
calpuff_template <- file.path(template_dir,"CALPUFF_7.0_template_Hg.INP")  # Mercury (Hg) in emission file

pu_templates <- list (# repartition = file.path(template_dir, "Mintia_postutilRepartition_noHg.inp"),  # No mercury in emission file 
  repartition = file.path(template_dir, "Mintia_postutilRepartition.inp"),     # Mercury (Hg) in emission file 
  deposition = file.path(template_dir, "Mintia_postutil_depo.inp"), 
  # total_pm = file.path(template_dir, "Mintia_postutil_PM10_noHg.inp"))  # No mercury in emission file
  total_pm = file.path(template_dir, "Mintia_postutil_PM10.inp"),
  sumruns=file.path(template_dir, "AfsinFun_postutil_sumruns.inp"))     # Mercury (Hg) in emission file 

calpost_templates <- list(concentration = file.path(template_dir, "Mintia_AllOutput_calpost.inp"), 
                          deposition = file.path(template_dir, "Mintia_depo_calpost.inp"))

# CALMET #######################################################################
#
#calmet_result <- creapuff::runCalmet(
#  input_xls = input_xls,
#  wrf_dir = wrf_dir,
#  expand_grids = expand_grids,
#  expand_ncells = expand_ncells,
#  output_dir = output_dir,
#  gis_dir = gis_dir,
#  calmet_exe = calmet_exe,
#  calmet_templates = calmet_templates,
#  only_make_additional_files=F,
#  run_calmet = T
#)

#calmet_result <- readRDS(file.path(output_dir,"calmet_result.RDS" ))
calmet_result <- readRDS(file.path("H:/SouthAfrica/calpuff_suite/calmet_result.RDS" ))

# INPUT DATA ###################################################################
# ============================== Emissions =====================================
# Define target_crs
calmet_result$params %>% lapply(data.frame) %>% bind_rows(.id='grid_name') %>% mutate(run_name=calmet_result$run_name) %>%
  mutate_at(c('DGRIDKM', 'XORIGKM', 'YORIGKM', 'NX', 'NY'), as.numeric) %>%
  rename(UTMZ=IUTMZN,
         UTMH=UTMHEM,
         GridD=DGRIDKM,
         GridNX=NX,
         GridNY=NY,
         GridX=XORIGKM,
         GridY=YORIGKM) %>%
  mutate(StartDate=paste(IBYR, IBMO, IBDY) %>% ymd %>% format("%Y%m%d"),
         EndDate=paste(IEYR, IEMO, IEDY) %>% ymd %>% format("%Y%m%d"),
         TZ=ABTZ %>% gsub('UTC', '', .) %>% as.numeric %>% divide_by(100)) -> out_files
target_crs <- get_utm_proj(zone = unique(out_files$UTMZ), hem = unique(out_files$UTMH))

##################################################################
#                      Prepare emissions                         #
##################################################################
#
# key data in files includes (a) coordinates of coal pits, and 
# (b) emission date for different pits~methods
#
# Load coordinate data
coord_data = read_xlsx(file.path(emissions_dir, 'emission_NewLargo.xlsx'), sheet='Coordinates', .name_repair=make.unique)

coord_data %<>% pivot_longer(-Pit) %>% 
  mutate(axis=name %>% gsub('[0-9\\.]', '', .), corner=name %>% gsub('[a-z]', '', .) %>% gsub('1\\.1', '5', .)) %>% 
  select(-name) %>% spread(axis, value) %>% 
  group_by(Pit) %>%
  summarise(geomtry = st_sfc(st_polygon(list(cbind(lon, lat))), crs = 4326))

##################################################################
# Option 1 (not working)- create function to create sf polygon and use with mutate
# Function to convert coordinates into an sf polygon object
#
#
# Option 2 (working) - manually apply code to each set of cordinates 
#
# Polygon for Pit F
# Define matrix with coordinates
#coords <- matrix(c(
#  28.981129,-26.013258,
#  29.003874,-26.015418,	
#  29.012586,-26.003693,	
#  28.992158,-25.996751,
#  28.981129,-26.013258
#), ncol = 2, byrow = TRUE)
# Convert matrix into sf polygon object
#polygonF <- st_polygon(list(coords))

##################################################################

#
# Load emission data

emissions_data = read_xlsx(file.path(emissions_dir, 'emission_NewLargo.xlsx'), sheet='CALPUFF input')

# Create a new column that represnts the pit-method combination
emissions_data %<>% mutate(emission_names = paste0(Pit, method))

# Combine data on emissions and pit coordinates
emissions_data <- merge(emissions_data,coord_data,by="Pit")

# Create polygons of grid boundaries  
dom_pols = grids_to_domains(calmet_result$grids, target_crs)

# Exclude sources outside domain  / Not working...
# Error in h(simpleError(msg, call)) : 
#  error in evaluating the argument 'x' in selecting a method for function 'crop': 
# could not identify coordinate columns, need to start with lat, lon or x, y
emissions_data %<>% to_spdf %>% raster::crop(spTransform(dom_pols, raster::crs(.))) %>% '@'('data')
# Need to speak to Lauri about this / I have added point coords of pit to get code moving

# browser()

# ============================== Receptors =====================================
# MESHDN parameter (in CALPUFF.INP) which defines the grid spacing 
# (DGRIDKM/MECHDN) of each disk, wrt the grid spacing of the outer 
# meteo grid (DGRIDKM). Higher factor: higher density of receptors.
nesting_factors = c(1,3,5,15)  # 15km, 5km, 3km, 1km  # c(1,2,5,15) 
#nesting_factors = c(1,5,15)  # 15km, 3km, 1km 

if(!exists('receptors')) receptors = list()

#allsources = emissions_data %>% 
#  bind_rows(coords_lepha %>% rename(plant=feature)) %>% 
#  distinct(emission_names=plant, .keep_all=T)

#queue = unique(allsources$emission_names)[1]
#queue = unique(emissions_data$Plant)
queue = unique(emissions_data$emission_names)

#emissions_data.rename(geometry=lats)

out_files %<>% mutate(across(c(GEODAT, METDAT), ~gsub('I:\\\\', 'H:\\\\', .x)))

for(run in queue) {
  #emissions_data_run <- allsources %>% filter(emission_names == run) %>% head(1)
  emissions_data_run <- emissions_data %>% filter(emission_names == run) 
  #print(emissions_data_run)
  #this <- emissions_data_run %>% to_spdf
  #break()
  
  loc <- emissions_data_run %>% to_spdf %>% spTransform(target_crs)
  
  # Get discrete receptors with 400x400 dim
  
  get_recep(loc = loc, 
            run_name = calmet_result$run_name,
            nesting_factors=nesting_factors,
            files_met=out_files,
            #output_dir = output_dir,
            target_crs=target_crs) -> receptors [[run]]
  print(run)
}

receptors %>% saveRDS(file.path(project_dir, 'calpuff_suite/allreceptors.RDS'))

receptors <- readRDS(file.path(project_dir, 'calpuff_suite/allreceptors.RDS'))

# Select discrete receptors around sources
# Radius of receptor disks [km], from outer to inner disk
nesfact_range = c(125,50,25,5) # c(150,75,25,5)  # c(150,75,25,10)  # c(125,75,25,5)  
#nesfact_range = c(125,25,5)   # c(125,25,10) 
#sources <- allsources %>% to_spdf %>% spTransform(target_crs)
sources <- emissions_data %>% to_spdf %>% spTransform(target_crs)
receptors %<>% select_receptors(sources=sources,
                                run_name = calmet_result$run_name,
                                nesting_factors=nesting_factors,
                                nesfact_range=nesfact_range,
                                files_met=out_files)


# Discrete receptor background grid
receptors[receptors$Xkm %% 30 < 15 & receptors$Ykm %% 30 < 15 & receptors$nesfact==1, 'include'] <- T

# Receptor check
print(paste('Adding background grid:', calmet_result$run_name, sum(receptors$include), 'receptors'))
if(sum(receptors$include)>=10000) stop('too many receptors!')  # LC 

# Receptor plot
quickpng(file.path(output_dir, paste0(calmet_result$run_name, '_', 'receptors+background_grid.png'))  )
receptors %>% subset(include==1) %>% plot(cex=.2)
plot(sources, col='blue', add=T)
get_adm(0, 'coarse') %>% cropProj(raster(receptors)) %>% plot(add=T, border='gray')
dev.off()

# ========================== Background concentrations =========================
# sources <- emissions_data %>% to_spdf %>% spTransform(target_crs)  
bgconcs <- get_bg_concs(sources, mod_dir=bc_dir)
o3dat <- NULL # Hourly ozone data file (NULL: no ozone monitoring stations)

# Remove lat/lon, so calpuff doesnt confuse the area sources for points
#emissions_data = subset(emissions_data, select = -c(lat,lon) )


# NOx emiss (kt/year), SOx emiss (kt/year), PM emiss (kt/year), Hg emiss (kt/year) - chosen
# CALPUFF ######################################################################
if (emission_type == "constant") {
  
  queue = unique(emissions_data$emission_name)
  for(run in queue) {
    # emissions_data_run <- emissions_data %>% filter(emission_names == run) %>% rename(PPM25=PM)
    emissions_data_run <- emissions_data %>% filter(emission_names == run)
  
    run_name <- run
    print(paste0("CALPUFF run name: ", run_name))
    print (paste0("Dataframe      : ", emissions_data_run))
    calpuff_result <- runCalpuff(
      
        #emissions_data = "NULL",                           # For constant emission data
        #source_names = emissions_data_run$plant_scenario,  # Optional. If not set: read from emissions_data (if not present, set automatically)
        #FGD = "none",                                      # Optional. If not set: read from emissions_data (if not present an error occurs)
        area_sources=emissions_data_run,
        receptors = receptors,                             # Optional. If not set: full domain grid
        o3dat = o3dat,                                     # Optional. If not set: no surface data
        species_configuration = "so2_nox_pm",           # Two possible values: "so2_nox_pm" or "so2_nox_pm_hg"
        bgconcs = bgconcs,                                 # Optional. If not set: std values
        # addparams = addparams,                           # Optional. If not set: std values
        run_name = run_name,   
        output_dir = output_dir,
        params_allgrids = calmet_result$params,
        gis_dir = gis_dir,
        calpuff_exe = calpuff_exe,
        calpuff_template = calpuff_template,
    )
  }
}

save.image(file.path(project_dir, 'calpuff.RData'))


load(file.path(project_dir, 'calpuff.RData'))



# POST-PROCESSING ##############################################################
# Load all CAPUFF results, from calpuff_result_*.RDS
calpuff_results_all <- file.path(output_dir, list.files(output_dir, pattern = 'calpuff_result.*\\.RDS')) %>% lapply(readRDS)  
calpuff_results_all %>% lapply('[[', 'inpfiles_created') %>% unlist -> inpfiles_created
runs <- gsub(paste0('.*/','|_CALPUFF.*\\.inp'), '', inpfiles_created)
names(calpuff_results_all) <- runs
names(inpfiles_created) <- runs

queue <- runs %>% paste0(project_dir, '/calpuff_suite/', . , '.CON') %>% file.exists() %>% '['(names(inpfiles_created), .)
# queue <- runs %>% paste0(project_dir, '/calpuff_suite/', . , '_calpost.lst') %>% file.exists() %>% not %>% '['(names(inpfiles_created), .)
queue <- runs %>% paste0(project_dir, '/calpuff_suite/', . , '_calpost.lst') %>% file.exists() %>%  '['(names(inpfiles_created), .)

for(scen in queue) {
  # ==============================================================================
  # 1. Create "SUMRUNS" INP files for summing up all CALPUFF outputs for each station, for :
  # - concentrations (.CON), no need for nitrate reparation (MNITRATE = 0), a further run will do the repartition
  # - deposition (.DRY, .WET) (together with acid, mercury, dust species)
  # Generate PU and CP INP files
  runPostprocessing(
    calpuff_inp=inpfiles_created[scen],
    output_dir=output_dir,
    files_met = out_files,
    pm10fraction=calpuff_results_all[[scen]]$pm10fraction,
    METRUN = 0,  
    nper = NULL,
    pu_start_hour = NULL,
    cp_species = c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2', 'PPM25', 'SO4', 'NO3', 'PM10'),  # c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2'),
    cp_period_function = get_cp_period,
    run_discrete_receptors=T,
    run_gridded_receptors=F,
    run_concentrations=T,
    run_deposition=T,
    run_timeseries = F,
    run_hourly = c(), #c('PM25', 'NO2', 'SO2'),
    run_pu=F,
    run_calpost=F,
    pu_templates = pu_templates,
    calpost_templates=calpost_templates
  )
}

# combine concentrations from multiple simulations / F2, D2, H2
# but F2 and H2 are still running, so practice with F1,F3, D2

scens_to_combine <- c('F2', 'D2', 'H2')
runPostprocessing(
  calpuff_inp=inpfiles_created[scens_to_combine][1],
  run_name=scens_to_combine,
  run_name_out=paste(scens_to_combine, collapse='+'),
  output_dir=output_dir,
  files_met = out_files,
  pm10fraction=calpuff_results_all[['F1']]$pm10fraction,
  METRUN = 0,  
  nper = NULL,
  pu_start_hour = NULL,
  cp_species = c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2', 'PPM25', 'SO4', 'NO3', 'PM10'),  # c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2'),
  cp_period_function = get_cp_period,
  run_discrete_receptors=T,
  run_gridded_receptors=F,
  run_concentrations=T,
  run_deposition=T,
  run_timeseries = F,
  run_hourly = c(), #c('PM25', 'NO2', 'SO2'),
  run_pu=F,
  run_calpost=F,
  pu_templates = pu_templates,
  calpost_templates=calpost_templates
)


# scale H2 and D2 to get H3 and D3




# stop here / no scaling applied


emis_modeled <- read_xlsx(file.path(emissions_dir, 'HIA_BantenSuralaya_2023_v2.xlsx'), sheet='CALPUFF input', n_max=30)
emis_final <- read_xlsx(file.path(emissions_dir, 'HIA_BantenSuralaya_2023_v2.xlsx'), sheet='final_emissions', n_max=30)

emis_modeled %>% filter(scenario=='Base') %>% mutate(scenario='modeled') %>% bind_rows(emis_final) -> emis_final

emis_final %<>% select(Tracker.ID='Tracker ID', Plant, scenario, SO2=SOx, NOx, PM, Hg) %>% 
  pivot_longer(is.numeric, names_to='pollutant') %>%
  group_by(Tracker.ID, pollutant) %>% mutate(scaling=value/value[scenario=='modeled']) %>% 
  filter(scenario != 'modeled')

emissions_data %>% 
  filter(scenario=='Base') %>%
  select(Tracker.ID='Tracker ID', base_emission_names=emission_names) %>%
  right_join(emis_final) -> emis_final

emis_final %>% mutate(across(pollutant, tolower)) %>%
  select(base_emission_names, scenario, pollutant, scaling) %>%
  pivot_wider(names_from='pollutant', values_from='scaling') ->
  emissions_scaling

emissions_scaling %<>% 
  split(f=.$scenario) %>%
  lapply(function(df) df %>% select(is.numeric) %>% split(df$base_emission_names))

sum_queue = calpost_names$case %>% unique

runs_to_sum = inpfiles_created[names(inpfiles_created) %in% calpost_names$calpuff[calpost_names$case=='Base']]

for(run in sum_queue) {
  pm10fraction = calpuff_results_all[names(runs_to_sum)] %>% lapply('[[', 'pm10fraction') %>% unlist %>% mean
  
  runPostprocessing(
    calpuff_inp=runs_to_sum[1],
    run_name=names(runs_to_sum),
    run_name_out = run,
    cp_run_name=substr(run, 1, 8),
    emissions_scaling = emissions_scaling[[run]],
    output_dir=output_dir,
    files_met = out_files,
    pm10fraction=pm10fraction,
    METRUN = 0,  
    nper = NULL,
    pu_start_hour = NULL,
    cp_species = c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2'), #'PPM25', 'SO4', 'NO3', 'PM10'
    cp_period_function = get_cp_period,
    run_discrete_receptors=T,
    run_gridded_receptors=F,
    run_concentrations=T,
    run_deposition=T,
    run_timeseries = F,
    run_hourly = c(), #c('PM25', 'NO2', 'SO2'),
    run_pu=F,
    run_calpost=F,
    pu_templates = pu_templates,
    calpost_templates=calpost_templates
  )
}



# 3. Run all bat files, in sequence
system2(pu_sumruns_bat) -> exit_code
if(exit_code != 0) stop("errors in POSTUTIL SUMRUNS execution")
system2(pu_bat) -> exit_code
if(exit_code != 0) stop("errors in POSTUTIL execution")
system2(cp_bat) -> exit_code
if(exit_code != 0) stop("errors in CALPOST execution")


