# Set up the environment
# remotes::install_github("energyandcleanair/creapuff", dependencies=T, update=F)
# devtools::reload(pkgload::inst("creapuff"))
library(lubridate)
library(tidyverse)
library(magrittr)
library(creapuff)
library(readxl)
library(writexl)
library(pbapply)
library(parallel)

# project_dir="Z:/"            # network disk (wrf_data). If Z disk is not present: mount 10.58.186.210:/wrf_data Z:)
project_dir="G:/IndonesiaIESR"       # calpuff_external_data-2 persistent disk (project data)

load(file.path(project_dir, 'CALPUFF_setup.RData'))


# Parameters ###################################################################
# ============================= Project specific ===============================
expand_grids = '*'  # All grids are expanded (for CALMET)
expand_ncells = -5  # Number of cells to expand met grid (e.g., for WRF data, exclusion of last 5 cells) in each direction (use negative values to crop).
crop_grid = list(extent(c(xmin=-2000, xmax=6000, ymin=8000, ymax=11500)), NA, NA)

wrf_dir <- file.path(project_dir,"calwrf") # Where calwrf data are stored 

output_dir <- file.path(project_dir,"calpuff_suite") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all generated files

emissions_dir <- file.path(project_dir,"emissions") # Directory where arbitrary-varying emission files are stored
#input_xls <- file.path(emissions_dir,"emissions.xlsx") # File where constant-emission data are specified
#input_xls <- file.path(emissions_dir,"emissions_clustered")
# Emission file should contain the following fields :
# Plants	Scenario Lat[deg]	Long[deg]	Status COD[year] NOx_tpa[t/y] SO2_tpa[t/y] PM_tpa[t/y] Hg_kgpa[kg/y] 
# AQCS FGD[logical] Exit temperature[C]	Stack diameter[m]	Stack height[m]	Exit velocity[m/s]

# ================================ General =====================================
# BE CAREFUL : gis_dir/landcover/C3S-LC-L4-LCCS-Map-300m-P1Y-2018-v2.1.1.nc is CORRUPETED in the repository !! You should replace it with a good one 
gis_dir <- "F:/gis"                         # The folder where we store general GIS data

bc_dir  <- file.path(gis_dir, "background") # The folder with background atmospheric concentrations for O3, NH3, H2O2

exe_dir="C:/CALPUFF"
calmet_exe <- file.path(exe_dir,"CALMET_v6.5.0_L150223/calmet_v6.5.0.exe")
calpuff_exe <- file.path(exe_dir,"CALPUFF_v7.2.1_L150618/calpuff_v7.2.1.exe")
pu_exe <- file.path(exe_dir,"POSTUTIL_v7.0.0_L150207/postutil_v7.0.0.exe")
calpost_exe <- file.path(exe_dir,"CALPOST_v7.1.0_L141010/calpost_v7.1.0.exe")

template_dir="F:/templates"
calmet_templates <- list(noobs=file.path(template_dir,"CALMET_template.INP"), 
                         surfobs=file.path(template_dir,"CALMET_surfObs_template.inp"))


calpuff_template <- file.path(template_dir,"CALPUFF_7.0_template_Hg.INP")                       # Mercury (Hg) in emission file
pu_templates <- list (repartition = file.path(template_dir, "Mintia_postutilRepartition.inp"),  # Mercury (Hg) in emission file 
                       deposition = file.path(template_dir, "Mintia_postutil_depo.inp"), 
                       total_pm = file.path(template_dir,   "Mintia_postutil_PM10.inp"))        # Mercury (Hg) in emission file 

# calpuff_template <- file.path(template_dir,"CALPUFF_7.0_template.INP")                               # No mercury in emission file 
# pu_templates <- list (repartition = file.path(template_dir, "Mintia_postutilRepartition_noHg.inp"),  # No mercury in emission file 
#                       deposition = file.path(template_dir,  "Mintia_postutil_depo.inp"), 
#                       total_pm = file.path(template_dir,    "Mintia_postutil_PM10_noHg.inp"))        # No mercury in emission file

calpost_templates <- list(concentration = file.path(template_dir, "Mintia_AllOutput_calpost.inp"), 
                          deposition = file.path(template_dir,    "Mintia_depo_calpost.inp"))

# CALMET #######################################################################
list.files(path = wrf_dir, pattern = '\\.m3d$', recursive = F, full.names = T) %>% 
  file.rename(., gsub('nest_', 'nest', .))

calmet_result <- runCalmet(
  wrf_dir = wrf_dir,
  crop_grid = crop_grid,
  output_dir = output_dir,
  gis_dir = gis_dir,
  calmet_exe = calmet_exe,
  calmet_templates = calmet_templates,
  only_make_additional_files=T,
  run_calmet = F
)


#browser()

calmet_result <- readRDS(file.path(output_dir,"calmet_result.RDS" ))


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




# Read emission data from file 
read_csv(file.path(emissions_dir, 'emissions, clustered.csv')) -> emissions_data

if (emissions_data$emission_names %>% nchar %>% max > 8) stop("ERROR in plant-name length (too many plants with the same name?)")                        

# Create polygons of grid boundaries  
dom_pols = grids_to_domains(calmet_result$grids, target_crs)

# Exclude sources outside domain  
emissions_data %<>% to_spdf %>% crop(spTransform(dom_pols, crs(.))) %>% '@'('data')


emissions_data %<>% rename(exit.temperature=contains('Exit.Temp'), stack.height=contains('Height'))

emissions_data$exit.temperature %<>% pmax(40) %>% add(273.15)


# browser()

# ============================== Receptors =====================================
# MESHDN parameter (in CALPUFF.INP) which defines the grid spacing 
# (DGRIDKM/MECHDN) of each disk, wrt the grid spacing of the outer 
# meteo grid (DGRIDKM). Higher factor: higher density of receptors.
#
runs = unique(emissions_data$emission_names)
runs %>% file.path(output_dir, .) %>% paste0('.CON') %>% file.info() -> run_df
queue = runs[is.na(run_df$size) | run_df$size<1.5e9]


base_res <- calmet_result$params %>% sapply('[[', 'DGRIDKM') %>% as.numeric %>% max

nesting_factors = c(1,2,6,12,30)  # 60km, 30km, 10km, 5km, 2km  # c(1,2,5,15) 
#nesting_factors = c(1,5,15)  # 15km, 3km, 1km 

rec_file=file.path(output_dir, 'receptors.RDS')
if(!file.exists(rec_file)) {
  receptors = list()
  queue = unique(emissions_data$emission_names) %>% subset(. %notin% names(receptors))
  for(run in queue) {
    emissions_data_run <- emissions_data %>% filter(emission_names == run) %>% head(1)
    loc <- emissions_data_run %>% to_spdf %>% spTransform(target_crs)
    # Get discrete receptors with 400x400 dim 
    get_recep(loc = loc, 
              run_name = calmet_result$run_name,
              nesting_factors=nesting_factors,
              files_met=out_files,
              target_crs=target_crs) -> receptors[[run]]
    print(run)
  }
  
  saveRDS(receptors, rec_file)
}

receptors <- readRDS(rec_file)

# Select discrete receptors around sources
# Radius of receptor disks [km], from outer to inner disk
# 
#nesfact_range = c(125,50,25,5) # c(150,75,25,5)  # c(150,75,25,10)  # c(125,75,25,5)  
nesfact_range = c(750, 300, 100, 50, 20)   # c(125,25,10) 

# CALPUFF ######################################################################
shp=readRDS(file.path(gis_dir, 'boundaries', 'gadm36_0_low.RDS'))
shp_utm = shp %>% cropProj(calmet_result$grids[[1]])

bgconc_file <- file.path(output_dir, 'bgconcs.RDS')
if(!file.exists(bgconc_file)) {
  bgconcs <- list()
  for(run in queue) {
    message(run)
    sources <- emissions_data %>% filter(emission_names==run) %>% to_spdf %>% spTransform(target_crs)
    bgconcs[[run]] <- get_bg_concs(sources, mod_dir=bc_dir)
  }
  bgconcs %>% saveRDS(bgconc_file)
}

bgconcs <- readRDS(bgconc_file)

creapuff.env <- list()
creapuff.env$llproj <- '+proj=longlat +datum=WGS84 +no_defs'

for(run in queue) {
  message(run)
  sources <- emissions_data %>% filter(emission_names==run) %>% to_spdf %>% spTransform(target_crs)
  receptors[[run]] %>% select_receptors(sources=sources,
                                        run_name = run,
                                        nesting_factors=nesting_factors,
                                        nesfact_range=nesfact_range,
                                        files_met=out_files,
                                        plotadm=shp) ->
    receptors_run
  
  # Discrete receptor background grid
  receptors_run[receptors_run$Xkm %% (base_res*2) < base_res & receptors_run$Ykm %% (base_res*2) < base_res & receptors_run$nesfact==1, 'include'] <- T
  
  # Receptor check
  print(paste('Adding background grid:', calmet_result$run_name, sum(receptors_run$include), 'receptors'))
  if(sum(receptors$include)>=10000) stop('too many receptors!')  # LC 
  
  # Receptor plot
  quickpng(file.path(output_dir, paste0(run, '_', 'receptors+background_grid.png'))  )
  receptors_run %>% subset(include==1) %>% plot(cex=.2)
  plot(sources, col='blue', add=T)
  shp_utm %>% subset(NAME_0=='Indonesia') %>% plot(add=T, border='gray')
  dev.off()
  
  receptors_run %>% saveRDS(file.path(output_dir, paste0('receptors_', run, '.RDS')))
}

for(run in queue) {
  sources <- emissions_data %>% filter(emission_names==run) %>% to_spdf %>% spTransform(target_crs)
  receptors_run <- readRDS(file.path(output_dir, paste0('receptors_', run, '.RDS')))
  o3dat <- NULL # Hourly ozone data file (NULL: no ozone monitoring stations)
  
  emissions_data_run <- emissions_data %>% filter(emission_names == run)
  print(paste0("CALPUFF run name: ", run))
  
  calpuff_result <- runCalpuff(
    
    emissions_data = emissions_data_run,               # For constant emission data
    source_names = emissions_data_run$emission_names,  # Optional. If not set: read from emissions_data (if not present, set automatically)
    FGD = T,                      # Optional. If not set: read from emissions_data (if not present an error occurs)
    receptors = receptors_run %>% subset(include),                             # Optional. If not set: full domain grid
    o3dat = o3dat,                                     # Optional. If not set: no surface data
    # species_configuration = "so2_nox_pm",            # Two possible values: "so2_nox_pm" or "so2_nox_pm_hg"
    species_configuration = "so2_nox_pm_hg",           
    bgconcs = bgconcs[[run]],                                 # Optional. If not set: std values
    # addparams = addparams,                           # Optional. If not set: std values
    run_name = run,   
    output_dir = output_dir,
    params_allgrids = calmet_result$params,
    gis_dir = gis_dir,
    calpuff_exe = calpuff_exe,
    calpuff_template = calpuff_template,
  )
}

#write out bat files to run in batches
paste0(queue, '.CON') %>% file.exists() %>% which() -> skip
file.path(output_dir, paste0(queue[-skip], '_CALPUFF_7.0.inp')) %>% split(1:14) -> batches

#calpuff_result %>% lapply('[[', 'inpfiles_created') %>% unlist %>% split(1:6) -> batches
for(i in seq_along(batches)) {
  batches[[i]] %>% paste(calpuff_exe, .) %>% c('pause') %>% 
    writeLines(file.path(output_dir, paste0('CALPUFF_batch_', i, '.bat')))
}






# POST-PROCESSING ##############################################################

plants = emissions_clustered$emission_names %>% unique

# Load all CAPUFF results, from calpuff_result_*.RDS
calpuff_results_all <- file.path(output_dir, paste0('calpuff_result_',plants,'.RDS')) %>% lapply(readRDS)  
calpuff_results_all %>% lapply('[[', 'inpfiles_created') %>% unlist -> inpfiles_created
names(calpuff_results_all) <- gsub(paste0('.*/','|_CALPUFF.*\\.inp'), '', inpfiles_created)  # TO DO : delete calmet_result$run_name in run name !
names(inpfiles_created) <- names(calpuff_results_all)

get_cp_period <- function(params) {
  runyr = as.numeric(params$val[params$name=='ISYR']) + ifelse(params$val[params$name=='ISMO']==12, 1, 0)
  list(start = paste0(runyr, '-01-01 0') %>% ymd_h,
       end = paste0(runyr+1, '-01-01 0') %>% ymd_h)
}

for (plant in plants) {
  # POST-PROCESSING ##############################################################
  # ============================ All clusters together ============================
  # 1. Sum up all output CALPUFF concentrations (.CON), using POSTUTIL
  
  # ========================== Scenario definition ===============================
  # --- Two main scenarios : 
  scenario_prefix <- plant
  
  # ---
  calpuff_results_all[names(calpuff_results_all) == plant]  -> calpuff_results_case
  inpfiles_created[names(inpfiles_created) == plant]  -> inpfiles_created_case
  emissions_clustered %>% filter(emission_names %in% names(inpfiles_created_case))  -> emissions_data_case
  
  # ==============================================================================
  # 1. Create "SUMRUNS" INP files for summing up all CALPUFF outputs for each station, for :
  # - concentrations (.CON), no need for nitrate reparation (MNITRATE = 0), a further run will do the repartition
  # - deposition (.DRY, .WET) (together with acid, mercury, dust species)
  files_met <- out_files  # or calpuff_results_all[[1]]$out_files  # All clusters have the same meteo
  first_cluster_inp <- inpfiles_created_case[1]
  first_cluster_name <- names(inpfiles_created_case)[1]
  
  if(!is.null(calpuff_results_case[[1]][['pm10fraction']]))
    calpuff_results_case %>% lapply('[[', 'pm10fraction') %>% unlist %>% mean() -> pm10fraction 
  
  # Generate "generic" PU and CP INP files (only for the first cluster, run_pu=F, run_calpost=F)
  creapuff::runPostprocessing(
    calpuff_inp=first_cluster_inp,
    cp_run_name=names(first_cluster_inp),
    output_dir=output_dir,
    files_met = files_met,
    pm10fraction=pm10fraction,
    METRUN = 0,  
    nper = NULL,
    pu_start_hour = NULL,
    cp_species = c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2'),
    cp_period_function = get_cp_period,
    run_discrete_receptors=T,
    run_gridded_receptors=F,
    run_concentrations=T,
    run_deposition=T,
    run_timeseries = T,
    run_hourly = c('PM25', 'NO2', 'SO2'),
    run_pu=F,
    run_calpost=F,
    pu_templates = pu_templates,
    calpost_templates=calpost_templates
  )
}

#write out bat files to run in batches
plants %>% split(1:2) -> batches
for(i in seq_along(batches)) {
  paste0('pu_', batches[[i]], '.bat') %>% file.path(output_dir, .) %>% lapply(readLines) -> pu_lines
  paste0('calpost_', batches[[i]], '.bat') %>% file.path(output_dir, .) %>% lapply(readLines) -> cp_lines
  
  
  
  c(pu_lines[[1]][1],
    c(pu_lines, cp_lines) %>% unlist %>% subset(!grepl('cd |pause', .)),
    'pause') %>% 
    writeLines(file.path(output_dir, paste0('batch_', i, '.bat')))
}


#aggregated scenarios
emissions_clustered_all <- read_csv(file.path(emissions_dir, 'emissions, clustered.csv'))

emissions_clustered_all %>% group_by(cluster) %>% 
  mutate(across(ends_with("pa"), ~.x/.x[case=='2022'])) ->
  emissions_scaling


emissions_scaling %>% 
  group_by(case) %>% 
  group_map(function(df, group) {
    scaling_case <- emissions_scaling %>% ungroup %>% filter(case==group$case) %>% 
      select(emission_names, pm=PM_tpa, so2=SOx_tpa, nox=NOx_tpa, hg=Hg_tpa) %>% 
      split(f=.$emission_names) %>% 
      lapply(select, -emission_names)
    
    if(group$case=='2022') scaling_case <- NULL
    
    list(run_name_out=group$case,
         run_name=df$emission_names,
         cp_run_name = group$case,
         emissions_scaling = scaling_case)
  }) -> run_queue

runScaling <- function(x) {
  message(x$run_name_out)
  runPostprocessing(
    calpuff_inp=inpfiles_created[[1]],
    run_name = x$run_name,
    run_name_out = x$run_name_out,
    cp_run_name = x$cp_run_name,
    output_dir=output_dir,
    files_met = out_files,
    pm10fraction=calpuff_results_all[['LCPP_IPP']]$pm10fraction,
    METRUN = 0,  
    nper = NULL,
    pu_start_hour = NULL,
    cp_species = c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2'),
    cp_period_function = get_cp_period,
    run_discrete_receptors=T,
    run_gridded_receptors=F,
    run_concentrations=T,
    run_deposition=T,
    run_timeseries = F,
    run_hourly = c('SO2', 'NO2'), #c('PM25', 'NO2', 'SO2'),
    emissions_scaling = x$emissions_scaling,
    run_pu=F,
    run_calpost=F,
    pu_templates = pu_templates,
    calpost_templates=calpost_templates
  )
}   


run_queue %>% lapply(runScaling)

