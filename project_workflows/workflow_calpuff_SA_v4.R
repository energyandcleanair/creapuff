# Set up the environment
# remotes::install_github("energyandcleanair/creapuff", dependencies=T, update=F)
# devtools::reload(pkgload::inst("creapuff"))


require(raster)
library(readxl)
library(writexl)
library(lubridate)
library(tidyverse)
library(magrittr)
library(pbapply)
require(creahelpers)
require(ncdf4)

#library(creapuff)

#development
source('R/env.R')
source('R/helpers.R')
source('R/01_calmet.R')
source('R/02_calpuff_corrected.R')
source('R/03_postprocessing.R')
source('R/plots.R')




# Parameters ###################################################################
# ============================= Project specific ===============================
expand_grids = '*'  # All grids are expanded (for CALMET)
expand_ncells = -5  # Number of cells to expand met grid (e.g., for WRF data, exclusion of last 5 cells) in each direction (use negative values to crop).

# project_dir="Z:/"                 # network disk (wrf_data). If Z disk is not present: mount 10.58.186.210:/wrf_data Z:)
project_dir="I:/SouthAfrica"       # calpuff_external_data-2 persistent disk (project data)

wrf_dir <- file.path(project_dir,"calwrf") # Where calwrf data are stored 

output_dir <- file.path(project_dir,"calpuff_suite") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all generated files

# emission_type = "varying" 
emission_type = "constant"  
emissions_dir <- file.path(project_dir,"emissions") # Directory where arbitrary-varying emission files are stored



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

# calpuff_template <- file.path(template_dir,"CALPUFF_7.0_template.INP")       # No mercury in emission file 
calpuff_template <- file.path(template_dir,"CALPUFF_7.0_template_Hg.INP")  # Mercury (Hg) in emission file

pu_templates <- list (# repartition = file.path(template_dir, "Mintia_postutilRepartition_noHg.inp"),  # No mercury in emission file 
                      repartition = file.path(template_dir, "Mintia_postutilRepartition.inp"),     # Mercury (Hg) in emission file 
                      deposition = file.path(template_dir, "Mintia_postutil_depo.inp"), 
                      # total_pm = file.path(template_dir, "Mintia_postutil_PM10_noHg.inp"))  # No mercury in emission file
                      total_pm = file.path(template_dir, "Mintia_postutil_PM10.inp"))     # Mercury (Hg) in emission file 

calpost_templates <- list(concentration = file.path(template_dir, "Mintia_AllOutput_calpost.inp"), 
                          deposition = file.path(template_dir, "Mintia_depo_calpost.inp"))


# CALMET #######################################################################


calmet_result <- creapuff::runCalmet(
  input_xls = input_xls,
  wrf_dir = wrf_dir,
  expand_grids = expand_grids,
  expand_ncells = expand_ncells,
  output_dir = output_dir,
  gis_dir = gis_dir,
  calmet_exe = calmet_exe,
  calmet_templates = calmet_templates,
  only_make_additional_files=F,
  run_calmet = T
)

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

if (emission_type == "constant") {
  # Read emission data from file 
  
  source('project_workflows/emissions_processing_SA.R')

  # Produce unique ascii names with 8 characters
  emissions_data %<>% mutate(emission_names = plant %>% gsub(' ', '', .) %>% substr(1,7) %>% paste0(stack))
  if (emissions_data$emission_names %>% nchar %>% max > 8) stop("ERROR in plant-name length (too much plants with the same name?)")                        

  # Create polygons of grid boundaries  
  dom_pols = grids_to_domains(calmet_result$grids, target_crs)
  
  # Exclude sources outside domain  
  emissions_data %<>% to_spdf %>% raster::crop(spTransform(dom_pols, raster::crs(.))) %>% '@'('data')
}

if (emission_type == "varying") {
  # Find ptemarb.DAT files
  emissions_file <- file.path(emissions_dir) %>% list.files(pattern='\\.DAT$', recursive = F, full.names=T) 
  # Read plant names and positions
  read_xlsx(input_xls) -> emissions_data 

  # Emission names
  emissions_data$Plants %>% gsub(' ', '', .) %>% make_srcnam -> emissions_data$emission_names
  
  # Ensure numeric values of lat and long  
  emissions_data$Lat %<>% as.numeric()
  emissions_data$Long %<>% as.numeric()
  
  # Create polygons of grid boundaries  
  dom_pols = grids_to_domains(calmet_result$grids, target_crs)
  
  # Exclude sources outside domain  
  emissions_data %<>% to_spdf %>% crop(spTransform(dom_pols, crs(.))) %>% '@'('data')

  # Test with less data
  # emissions_data %<>% head(1)
  # emissions_data %<>% tail(1)
  # emissions_data <- rbind(emissions_data %>% head(1), emissions_data %>% tail(1))
}


# browser()

# ============================== Receptors =====================================
# MESHDN parameter (in CALPUFF.INP) which defines the grid spacing 
# (DGRIDKM/MECHDN) of each disk, wrt the grid spacing of the outer 
# meteo grid (DGRIDKM). Higher factor: higher density of receptors.
nesting_factors = c(1,3,5,15)  # 15km, 5km, 3km, 1km  # c(1,2,5,15) 
#nesting_factors = c(1,5,15)  # 15km, 3km, 1km 

if(!exists('receptors')) receptors = list()

allsources = emissions_data %>% 
  bind_rows(coords_lepha %>% rename(plant=feature)) %>% 
  distinct(emission_names=plant, .keep_all=T)
queue = unique(allsources$emission_names)
for(run in queue) {
  emissions_data_run <- allsources %>% filter(emission_names == run) %>% head(1)
  loc <- emissions_data_run %>% to_spdf %>% spTransform(target_crs)
  # Get discrete receptors with 400x400 dim 
  get_recep(loc = loc, 
            run_name = calmet_result$run_name,
            nesting_factors=nesting_factors,
            files_met=out_files,
            target_crs=target_crs) -> receptors [[run]]
    print(run)
}

receptors %>% saveRDS(file.path(project_dir, 'calpuff_suite/allreceptors.RDS'))

receptors <- readRDS(file.path(project_dir, 'calpuff_suite/allreceptors.RDS'))

# Select discrete receptors around sources
# Radius of receptor disks [km], from outer to inner disk
nesfact_range = c(125,50,25,5) # c(150,75,25,5)  # c(150,75,25,10)  # c(125,75,25,5)  
#nesfact_range = c(125,25,5)   # c(125,25,10) 
sources <- allsources %>% to_spdf %>% spTransform(target_crs)
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


# CALPUFF ######################################################################
if (emission_type == "constant") {
  
  queue = unique(emissions_data$plant)
  for(run in queue) {
    emissions_data_run <- emissions_data %>% filter(plant == run) %>% 
      rename(NOx_tpa=NOx, SO2_tpa=SO2, PM_tpa=PM, Hg_kgpa=Hg)
    
    emis %>% 
      filter(plant==run) %>% mutate(plant='all') %>% 
      group_by(apply_to=plant, pollutant) %>% 
      arrange(month) %>% mutate(across(monthscaling, signif, 6)) %>% 
      summarise(across(monthscaling, paste, collapse=', ')) ->
      monthly_scaling
    
    #use NOx for Hg
    monthly_scaling %<>% filter(pollutant=='NOx') %>% mutate(pollutant='Hg') %>% bind_rows(monthly_scaling)
    
    
    run_name <- run
    print(paste0("CALPUFF run name: ", run_name))
    
    calpuff_result <- runCalpuff(
      
        emissions_data = emissions_data_run,               # For constant emission data
        source_names = emissions_data_run$emission_names,  # Optional. If not set: read from emissions_data (if not present, set automatically)
        FGD = emissions_data_run$FGD,                      # Optional. If not set: read from emissions_data (if not present an error occurs)
        receptors = receptors,                             # Optional. If not set: full domain grid
        o3dat = o3dat,                                     # Optional. If not set: no surface data
        species_configuration = "so2_nox_pm_hg",           # Two possible values: "so2_nox_pm" or "so2_nox_pm_hg"
        bgconcs = bgconcs,                                 # Optional. If not set: std values
        # addparams = addparams,                           # Optional. If not set: std values
        monthly_scaling = monthly_scaling,
        run_name = run_name,   
        output_dir = output_dir,
        params_allgrids = calmet_result$params,
        gis_dir = gis_dir,
        calpuff_exe = calpuff_exe,
        calpuff_template = calpuff_template,
    )
  }
  
  #Lephalale IPP
  runCalpuff(
    emissions_data = emis_ipp,               # For constant emission data
    source_names = emis_ipp$emission_names,  # Optional. If not set: read from emissions_data (if not present, set automatically)
    FGD = T,                      # Optional. If not set: read from emissions_data (if not present an error occurs)
    receptors = receptors,                             # Optional. If not set: full domain grid
    o3dat = o3dat,                                     # Optional. If not set: no surface data
    species_configuration = "so2_nox_pm_hg",           # Two possible values: "so2_nox_pm" or "so2_nox_pm_hg"
    bgconcs = bgconcs,                                 # Optional. If not set: std values
    run_name = 'LCPP_IPP',   
    output_dir = output_dir,
    params_allgrids = calmet_result$params,
    gis_dir = gis_dir,
    calpuff_exe = calpuff_exe,
    calpuff_template = calpuff_template,
  )
  
  #Lephalale mine
  runCalpuff(
    #emissions_data = emis_ipp,               # For constant emission data
    #source_names = emis_ipp$emission_names,  # Optional. If not set: read from emissions_data (if not present, set automatically)
    #FGD = T,                      # Optional. If not set: read from emissions_data (if not present an error occurs)
    area_sources=emis_mine,
    receptors = receptors,                             # Optional. If not set: full domain grid
    o3dat = o3dat,                                     # Optional. If not set: no surface data
    species_configuration = "so2_nox_pm_hg",           # Two possible values: "so2_nox_pm" or "so2_nox_pm_hg"
    bgconcs = bgconcs,                                 # Optional. If not set: std values
    run_name = 'LCPP_mine',   
    output_dir = output_dir,
    params_allgrids = calmet_result$params,
    gis_dir = gis_dir,
    calpuff_exe = calpuff_exe,
    calpuff_template = calpuff_template,
  )
  
  queue %<>% c('LCPP_IPP', 'LCPP_mine') %>% unique
  
  # Execute each CALPUFF bat file
  for(run in queue) {
    emissions_data_run <- emissions_data %>% filter(emission_names == run) %>% head(1)
    run_name <- emissions_data_run$emission_names  
    bat_file <- file.path(output_dir, paste0(run_name, '_1', '.bat'))
    shell.exec(normalizePath(bat_file))
    Sys.sleep(10)
  } 
  
  # All-in-one solution: only one CALPUFF simulation for all sources
  # calpuff_result <- creapuff::runCalpuff(
  # emissions_data = emissions_data,               # For constant emission data
  # source_names = emissions_data$emission_names,  # Optional. If not set: read from emissions_data (if not present, set automatically)
  # FGD = emissions_data$FGD,                      # Optional. If not set: read from emissions_data (if not present an error occurs)
  # receptors = receptors,                         # Optional. If not set: full domain grid
  # o3dat = o3dat,                                 # Optional. If not set: no surface data
  # species_configuration = "so2_nox_pm_hg",       # Two possible values: "so2_nox_pm" or "so2_nox_pm_hg"
  # bgconcs = bgconcs,                             # Optional. If not set: std values
  # # addparams = addparams,                       # Optional. If not set: std values
  # run_name = calmet_result$run_name,
  # output_dir = output_dir,
  # params_allgrids = calmet_result$params,
  # gis_dir = gis_dir,
  # calpuff_exe = calpuff_exe,
  # calpuff_template = calpuff_template)
}

if (emission_type == "varying") {
  queue = unique(emissions_data$emission_names)
  for(run in queue) {
    emissions_data_run <- emissions_data %>% filter(emission_names == run) %>% head(1)
    # run_name <- paste(calmet_result$run_name, emissions_data_run$emission_names,sep='_')  # TO DO : delete calmet_result$run_name in run name !
    run_name <- emissions_data_run$emission_names 
    NPT2 <- emissions_data_run$N_sources  # Number of emission sources per file
    print(paste0("CALPUFF run name: ", run_name, ", n_sources: ", NPT2))
    
    calpuff_result <- creapuff::runCalpuff(
      # emissions_data = emissions_data,     # For constant emissions 
      # source_names = source_names,         # Optional. If not set: read from emissions_data (if not present, set automatically)
      # FGD = "T",                           # Optional. If not set: read from emissions_data (if not present, an error occurs)
      receptors=receptors,                   # Optional. If not set: full domain grid
      o3dat=o3dat,                           # Optional. If not set: no surface data
      bgconcs=bgconcs,                       # Optional. If not set: std values
      species_configuration = "so2_nox_pm_hg",  # Two possible values: "so2_nox_pm", "so2_nox_pm_hg"
      addparams = list(NPTDAT = 1,           # For arbitrary-varying emissions
                       PTDAT = emissions_data_run$Path,
                       NPT2 = NPT2),
      run_name=run_name,
      output_dir = output_dir,
      params_allgrids = calmet_result$params,
      gis_dir = gis_dir,
      calpuff_exe = calpuff_exe,
      calpuff_template = calpuff_template
    )
  } 
  
  # Execute each CALPUFF bat file
  for(run in queue) {
    emissions_data_run <- emissions_data %>% filter(emission_names == run) %>% head(1)
    run_name <- emissions_data_run$emission_names

    bat_file <- file.path(output_dir, paste0(run_name, '_1', '.bat'))
    # shell.exec(normalizePath(bat_file))
    # Sys.sleep(10)
  }
}


browser()

for (i_Sc in seq(1,2)) {
# POST-PROCESSING ##############################################################
# ============================ All clusters together ============================
# 1. Sum up all output CALPUFF concentrations (.CON), using POSTUTIL
# Load all CAPUFF results, from calpuff_result_*.RDS
calpuff_results_all <- file.path(output_dir, list.files(output_dir, pattern = 'calpuff_result.*\\.RDS')) %>% lapply(readRDS)  
calpuff_results_all %>% lapply('[[', 'inpfiles_created') %>% unlist -> inpfiles_created
names(calpuff_results_all) <- gsub(paste0('.*/','|_CALPUFF.*\\.inp'), '', inpfiles_created)  # TO DO : delete calmet_result$run_name in run name !
names(inpfiles_created) <- names(calpuff_results_all)

# ========================== Scenario definition ===============================
# # --- Scenario ScA : old limits (2008), from coal consumption
# scenario_prefix <- "ScA"   # Max 8 char  # included_stations = names(inpfiles_created)
# included_stations <- names(inpfiles_created)[grep("oCC", names(inpfiles_created) )]

# --- Two main scenarios : 
if (i_Sc==1) {scenario_prefix <- "ScA"; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='Next Future']}
if (i_Sc==2) {scenario_prefix <- "ScB"; included_stations <- emissions_data$emission_names}
# if (i_Sc==2) {scenario_prefix <- "ScB"; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='Next Future', emissions_data$Scenario=='Future']}
# if (i_Sc==3) {scenario_prefix <- "ScS" ; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='2008 standards, FGD, 130 degrees']}
# if (i_Sc==4) {scenario_prefix <- "ScW"; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='2008 standards, no FGD']}

# ---
calpuff_results_all[names(calpuff_results_all) %in% included_stations == TRUE]  -> calpuff_results_all
inpfiles_created[names(inpfiles_created) %in% included_stations == TRUE]  -> inpfiles_created
emissions_data %>% filter(emissions_data$emission_names %in% names(inpfiles_created)  == TRUE)  -> emissions_data_scenario
write_xlsx(list("CALPUFF input"=emissions_data_scenario), file.path(emissions_dir, paste0("coordinates_",scenario_prefix,".xlsx")))

# ==============================================================================
# 1. Create "SUMRUNS" INP files for summing up all CALPUFF outputs for each station, for :
# - concentrations (.CON), no need for nitrate reparation (MNITRATE = 0), a further run will do the repartition
# - deposition (.DRY, .WET) (together with acid, mercury, dust species)
files_met <- out_files  # or calpuff_results_all[[1]]$out_files  # All clusters have the same meteo
first_cluster_inp <- inpfiles_created[1]
first_cluster_name <- names(inpfiles_created)[1]
calpuff_results_all %>% lapply('[[', 'pm10fraction') %>% unlist %>% mean() -> pm10fraction 

# Generate "generic" PU and CP INP files (only for the first cluster, run_pu=F, run_calpost=F)
creapuff::runPostprocessing(
  calpuff_inp=first_cluster_inp,
  output_dir=output_dir,
  files_met = files_met,
  pm10fraction=pm10fraction,
  METRUN = 0,  
  nper = NULL,
  pu_start_hour = NULL,
  cp_species = c('PM25', 'TPM10', 'SO2', 'NO2'),  # c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2'),
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

# Read generic inp files
pu_template_desinence <- pu_templates %>% lapply(function(x) gsub("^[^_]*", "", x))
pu_sumruns_template_generic=paste0(first_cluster_name, pu_template_desinence$repartition)
pu_sumruns_depo_template_generic=paste0(first_cluster_name, pu_template_desinence$depo)
readLines(file.path(output_dir,pu_sumruns_template_generic)) -> inp
readLines(file.path(output_dir,pu_sumruns_depo_template_generic)) -> inp_depo

# Define current input-data list  
sumfiles = paste0(toupper(names(inpfiles_created)), '.CON')  
sumfiles_depo = append (paste0(toupper(names(inpfiles_created)), '.DRY'), paste0(toupper(names(inpfiles_created)), '.WET'))

# Fill generic inp files with current parameters and output-data names 
inp %<>% set_puff(list(NFILES = length(sumfiles),  # Function to set parameters in a CALPUFF input file
                       MNITRATE  =  0,             # Recompute the HNO3/NO3 partition for concentrations, for all sources combined
                       UTLLST = file.path(output_dir,paste0(scenario_prefix,"_POSTUTIL_SUMRUNS.LST")),  # Output LST file
                       UTLDAT = file.path(output_dir,paste0(scenario_prefix,".CON"))))                  # Output data file, for concentrations (.CON)
inp_depo %<>% set_puff(list(NFILES = length(sumfiles_depo),  # Function to set parameters in a CALPUFF input file
                       UTLLST = file.path(output_dir,paste0(scenario_prefix,"_POSTUTIL_Depo.LST")),            # Output LST file. Filename-format as ScAll_POSTUTIL_Depo.LST
                       UTLDAT = file.path(output_dir,paste0(scenario_prefix,"_Depo.FLX"))))                    # Output data file, for fluxes (.FLX). Filename-format as ScAll_Depo.FLX

# Fill the corresponding lines for with current input-data list (CALPUFF outputs)
con_line <- grep("!MODDAT=", gsub(" ", "", inp))  # Looking for input-data position index 
inp <- c(inp[1:(con_line-1)],
         paste("   ",1:length(sumfiles),"            CALPUFF.DAT        ! MODDAT =",file.path(output_dir, sumfiles),"! !END!"),  # For concentrations (.CON)
         inp[-1:-con_line])
con_line_depo <- grep("!MODDAT=", gsub(" ", "", inp_depo))  # Looking for input-data position index
inp_depo <- c(inp_depo[1:(con_line_depo-1)],
         paste("   ",1:length(sumfiles_depo),"            CALPUFF.DAT        ! MODDAT =",file.path(output_dir, sumfiles_depo),"! !END!"),  # For deposition (.DRY, .WET)
         inp_depo[-1:-con_line_depo[2]])

# Write the _POSTUTIL_SUMRUNS.INP and  _postutil_depo.inp files
pu_sumruns_template_all <- file.path(output_dir, paste0(scenario_prefix,"_POSTUTIL_SUMRUNS.INP"))
writeLines(inp, pu_sumruns_template_all) 
pu_sumruns_depo_template_all <- file.path(output_dir, paste0(scenario_prefix,"_postutil_depo.inp"))  # Filename-format for PU bat files (ex: pu_ScAll.bat) 
writeLines(inp_depo, pu_sumruns_depo_template_all) 

# Create the _SUMRUNS.bat bat file
pu_sumruns_bat <- file.path(output_dir, paste0("pu_",scenario_prefix,"_SUMRUNS.bat"))
writeLines(c(paste("cd", output_dir),
             paste0(pu_exe, " ", normalizePath(pu_sumruns_template_all)),
             "pause"), 
           pu_sumruns_bat)  # shell.exec(normalizePath(pu_sumruns_bat)) 


# 2. Define PU and CP INP and bat files, for summed-up concentrations   
name_generic=first_cluster_name  

# PU INP: change names of input and output parameters, from generic "first_cluster_name" to selected scenario (ScAll, or ScB, ...)
file.path(output_dir, list.files(output_dir, pattern=paste0('^',first_cluster_name,'_postutil'))) -> pu_files_generic
pu_files_generic <- pu_files_generic[c(grep("Repartition", pu_files_generic),grep("PM10", pu_files_generic))]  # No need for _postutil_depo.inp, already created
for(f in pu_files_generic) {
  f %>% readLines %>% gsub(name_generic, scenario_prefix, .) %>%    
    writeLines(file.path (output_dir, gsub(paste0('^',first_cluster_name), scenario_prefix, basename(f))))
}

# CP INP: change names of input and output parameters, from generic "first_cluster_name" to selected scenario (ScAll, or ScB, ...)
file.path(output_dir, list.files(output_dir, pattern=paste0('^',first_cluster_name,'_.*calpost'))) -> cp_files_generic
for(f in cp_files_generic) {
  f %>% readLines %>% gsub(name_generic, scenario_prefix, .) %>%  
    gsub(first_cluster_name, scenario_prefix, .) %>% 
    writeLines(file.path (output_dir, gsub(paste0('^',first_cluster_name), scenario_prefix, basename(f))))
}

# Bat files 
file.path(output_dir,list.files(output_dir, pattern=paste0(first_cluster_name,'\\.bat'))) -> bat_files_generic
for(f in bat_files_generic) {
  f %>% readLines %>% gsub(name_generic, scenario_prefix, .) %>%  
    gsub(first_cluster_name, scenario_prefix, .) %>% 
    writeLines(file.path (output_dir, gsub(paste0(first_cluster_name), scenario_prefix, basename(f))))
}
pu_bat <- file.path(output_dir, paste0("pu_", scenario_prefix, ".bat"))       # shell.exec(normalizePath(pu_bat)) 
cp_bat <- file.path(output_dir, paste0("calpost_", scenario_prefix, ".bat"))  # shell.exec(normalizePath(cp_bat)) 

# Remove generic INP and bat files
file.remove(pu_files_generic)
file.remove(cp_files_generic)
file.remove(bat_files_generic)


# 3. Run all bat files, in sequence
system2(pu_sumruns_bat) -> exit_code
if(exit_code != 0) stop("errors in POSTUTIL SUMRUNS execution")
system2(pu_bat) -> exit_code
if(exit_code != 0) stop("errors in POSTUTIL execution")
system2(cp_bat) -> exit_code
if(exit_code != 0) stop("errors in CALPOST execution")

}
