# Set up the environment
# remotes::install_github("energyandcleanair/creapuff", dependencies=T, update=F)
# devtools::reload(pkgload::inst("creapuff"))
library(creapuff)
library(readxl)
library(writexl)
library(lubridate)
library(tidyverse)
library(magrittr)
library(pbapply)
library(parallel)  


# Parameters ###################################################################
# ============================= Project specific ===============================
expand_grids = '*'  # All grids are expanded (for CALMET)
expand_ncells = -5  # Number of cells to expand met grid (e.g., WRF) in each direction: Use negative values to cro; use 0 to test

# project_dir="Z:/"              # network disk (wrf_data). If Z disk is not present: mount 10.58.186.210:/wrf_data Z:)
# project_dir="F:/cambodia_test" # calpuff_data            persistent disk (config  data)
# project_dir="G:/chile"         # calpuff_external_data   persistent disk (project data)
# project_dir="H:/cambodia"      # calpuff_external_data-2 persistent disk (project data)
project_dir="I:/india_kaparkheda"         # calpuff_external_data-3 persistent disk (project data)

output_dir <- file.path(project_dir,"calpuff_suite") # Where to write all generated files

# wrf_dir <- file.path(project_dir,"calwrf/d02") # Where calwrf data are stored
# wrf_dir <- file.path(project_dir,"calwrf") 
wrf_dir <- file.path("I:/india","calwrf") 

# emission_type = "varying" 
emission_type = "constant"  # For Cambodia we dispose of constant emission data

if (emission_type == "constant"){
  emissions_dir <- file.path(project_dir,"emissions") # Directory where arbitrary-varying emission files are stored
  # input_xls <- file.path(emissions_dir,"india_coal_plants_2020.xlsx") # File where constant-emission data are specified
  input_xls <- file.path(emissions_dir,"india_coal_plants_kaparkheda_2020.xlsx") # File where constant-emission data are specified
  # Emission file, required fields :
  # Plants, Lat[deg], Long[deg], Status*, COD[year]*, SO2_tpa[t/y], NOx_tpa[t/y], PM_tpa[t/y], Hg_kgpa[kg/y], Exit temperature[C], Stack diameter[m], Stack height[m], FGD[logical]
}
if (emission_type == "varying") {
  emissions_dir <- file.path(project_dir,"emissions/2019") # Directory where arbitrary-varying emission files are stored
  input_xls <- file.path(emissions_dir,"coordinates.xlsx") # Where plant positions are reported
}

# ================================ General =====================================
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
# calmet_result <- creapuff::runCalmet(
#   input_xls = input_xls,
#   wrf_dir = wrf_dir,
#   expand_grids = expand_grids,
#   expand_ncells = expand_ncells,
#   output_dir = output_dir,
#   gis_dir = gis_dir,
#   calmet_exe = calmet_exe,
#   calmet_templates = calmet_templates,
#   only_make_additional_files=F,
#   run_calmet = F
# )
# browser()

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
  read_xlsx(input_xls, sheet='CALPUFF input') -> emissions_data

  # Define which sources should be included in which scenario. COD = commercial operation date  # LC, CHECK
  # emissions_data$COD %>% substr(.,nchar(.)-3,nchar(.)) %>% as.numeric () <   
  #   calmet_result$start_dates[[1]] %>% format("%Y") %>% as.numeric() ->  
  #   emissions_data$existing
  # emissions_data$status = ifelse(emissions_data$existing,'operating', 'future')  
  # Selection of operating/future plants
  # emissions_data %<>% filter(status == "operating")

  # Produce unique ascii names with 8 characters
  emissions_data$Plants %>% gsub(' ', '', .) %>% substr(1,5) %>% stringi::stri_trans_general("Latin-ASCII") %>%  
    make.names %>% make.unique(sep='') %>% 
    paste0('_', substr(emissions_data$Status,1,1)) -> emissions_data$emission_names
  if (emissions_data$emission_names %>% nchar %>% max > 8) stop("ERROR in plant-name length (too much plants with the same name?)")                        

  # Ensure numeric values of lat and long  
  emissions_data$Lat %<>% as.numeric()
  emissions_data$Long %<>% as.numeric()
  emissions_data$FGD %<>% as.logical()
  
  # Create polygons of grid boundaries  
  dom_pols = grids_to_domains(calmet_result$grids, target_crs)
  
  # Exclude sources outside domain  
  emissions_data %<>% to_spdf %>% crop(spTransform(dom_pols, crs(.))) %>% '@'('data')
  
  # Test with less data
  # emissions_data %<>% head(1)
  # emissions_data %<>% tail(1)
  # emissions_data <- rbind(emissions_data %>% head(1), emissions_data %>% tail(1))
}

if (emission_type == "varying") {
  # Find ptemarb.DAT files
  emissions_file <- file.path(emissions_dir) %>% list.files(pattern='\\.DAT$', recursive = F, full.names=T) 
  # Read plant names and positions
  read_xlsx(input_xls) -> emissions_data 

  # Emission names
  emissions_data$Plants %>% gsub(' ', '', .) %>% substr(1,5) %>% stringi::stri_trans_general("Latin-ASCII") %>%  
    make.names %>% make.unique(sep='') -> emissions_data$emission_names
  
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


# To start from here
# save.image(file = file.path(output_dir, "save_data_1.RDS"))
# load( file.path(output_dir, "save_data_1.RDS"))

# ============================== Receptors =====================================
# MESHDN parameter (in CALPUFF.INP) which defines the grid spacing 
# (DGRIDKM/MECHDN) of each disk, wrt the grid spacing of the outer 
# meteo grid (DGRIDKM). Higher factor: higher density of receptors.
nesting_factors = c(1,2,5,15)  # c(1,5,15) # c(1,2,5,15) 

if(!exists('receptors')) receptors = list()
queue = unique(emissions_data$emission_names)
for(run in queue) {
  emissions_data_run <- emissions_data %>% filter(emission_names == run) %>% head(1)
  loc <- emissions_data_run %>% to_spdf %>% spTransform(target_crs)
  # Get discrete receptors with 400x400 dim 
  get_recep(loc = loc, 
            run_name = calmet_result$run_name,
            nesting_factors=nesting_factors,
            files_met=out_files,
            target_crs=target_crs) -> receptors [[run]]
    print(run) 
}

# Select discrete receptors around sources
nesfact_range = c(150,75,25,10) # c(125,25,5)  # c(125,25,10) # c(150,75,25,10) # Radius of receptor disks [km], from outer to inner disk
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


# To start from here
# save.image(file = file.path(output_dir, "save_data_2.RDS"))
# load( file.path(output_dir, "save_data_2.RDS"))

# CALPUFF ######################################################################
if (emission_type == "constant") {
  
  queue = unique(emissions_data$emission_names)
  for(run in queue) {
    emissions_data_run <- emissions_data %>% filter(emission_names == run) %>% head(1)
    run_name <- run  # emissions_data_run$emission_names  
    print(paste0("CALPUFF run name: ", run_name))
    
    calpuff_result <- creapuff::runCalpuff(
        emissions_data = emissions_data_run,               # Only for constant emission data
        source_names = emissions_data_run$emission_names,  # Optional. If not set: read from emissions_data (if not present, set automatically)
        FGD = emissions_data_run$FGD,                      # Optional. If not set: read from emissions_data (if not present an error occurs)
        receptors = receptors,                             # Optional. If not set: full domain grid
        o3dat = o3dat,                                     # Optional. If not set: no surface data
        species_configuration = "so2_nox_pm_hg",           # With or without Hg  : "so2_nox_pm_hg", "so2_nox_pm"
        bgconcs = bgconcs,                                 # Optional. If not set: std values
        # addparams = addparams,                           # Optional. If not set: no other (single value) parameters to specify in the INP file
        run_name = run_name,   
        output_dir = output_dir,
        params_allgrids = calmet_result$params,
        gis_dir = gis_dir,
        calpuff_exe = calpuff_exe,
        calpuff_template = calpuff_template,
    )
  }

  # CALPUFF execution
  # Option 1 : execute each CALPUFF run, using different bat files 
  for(run in queue) {
    emissions_data_run <- emissions_data %>% filter(emission_names == run) %>% head(1)
    run_name <- run  # emissions_data_run$emission_names  
    bat_file <- file.path(output_dir, paste0(run_name, '_1', '.bat'))
    shell.exec(normalizePath(bat_file))
    Sys.sleep(2)
  } 
  
  # # Option 2 : execute several CALPUFF runs using a same bat file (final number of bat files depends on available CPUs)
  # N_CPUs <- detectCores()
  # N_bat_files <- length(queue)
  # bat_file_lines <- round(N_bat_files/N_CPUs)  # CHECK : or ceiling() ?
  # for (i in seq(1, N_bat_files, by = bat_file_lines)){
  #   i_end <- (i+bat_file_lines-1)
  #   run <- queue[i:i_end] %>% subset(!is.na(.))
  #   patterns=paste0(run, '_1', '.bat')
  #   bat_files_list <- lapply(patterns, function(x){list.files(output_dir, pattern = x,  full.names=TRUE)})
  #   bat_files_text <- sapply(bat_files_list, function(x){readLines(x,n=1)}) %>% append("pause")
  #   bat_file <- file.path(output_dir, paste0("calpuff_batgroup_",i,"-",i_end,".bat"))
  #   bat_files_text %>% writeLines(bat_file)
  #   # shell.exec(normalizePath(bat_file))
  #   # Sys.sleep(2)
  # }
  
  # Option 3 : only one CALPUFF simulation (one bat file only) for all sources together (one output file only) 
  # calpuff_result <- creapuff::runCalpuff(
  # emissions_data = emissions_data,               # Only for constant emission data
  # source_names = emissions_data$emission_names,  # Optional. If not set: read from emissions_data (if not present, set automatically)
  # FGD = emissions_data$FGD,                      # Optional. If not set: read from emissions_data (if not present an error occurs)
  # receptors = receptors,                         # Optional. If not set: full domain grid
  # o3dat = o3dat,                                 # Optional. If not set: no surface data
  # species_configuration = "so2_nox_pm_hg",       # With or without hg  : "so2_nox_pm_hr", "so2_nox_pm_hg"
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
    run_name <- run  # emissions_data_run$emission_names 
    NPT2 <- emissions_data_run$N_sources  # Number of emission sources per file
    print(paste0("CALPUFF run name: ", run_name, ", n_sources: ", NPT2))
    
    calpuff_result <- creapuff::runCalpuff(
      # emissions_data = emissions_data,        # Only for constant emissions 
      # source_names = source_names,            # Optional. If not set: read from emissions_data (if not present, set automatically)
      # FGD = "T",                              # Optional. If not set: read from emissions_data (if not present, an error occurs)
      receptors=receptors,                      # Optional. If not set: full domain grid
      o3dat=o3dat,                              # Optional. If not set: no surface data
      species_configuration = "so2_nox_pm_hg",  # With or without hg: "so2_nox_pm_hr", "so2_nox_pm_hg"
      bgconcs=bgconcs,                          # Optional. If not set: std values
      addparams = list(NPTDAT = 1,              # Specify arbitrary-varying emission parameters
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
    run_name <- run  # emissions_data_run$emission_names
    bat_file <- file.path(output_dir, paste0(run_name, '_1', '.bat'))
    # shell.exec(normalizePath(bat_file)) 
    # Sys.sleep(2)
  }
}


browser()


# To start from here
# save.image(file = file.path(output_dir, "save_data_3.RDS"))
# load( file.path(output_dir, "save_data_3.RDS"))


for (i_Sc in seq(1,16)) {
# POST-PROCESSING ##############################################################
# ============================ By unit cluster  ================================
# 1. Sum up all output CALPUFF concentrations (.CON), using POSTUTIL
# Load all CAPUFF results, from calpuff_result_*.RDS
calpuff_results_all <- file.path(output_dir, list.files(output_dir, pattern = 'calpuff_result.*\\.RDS')) %>% lapply(readRDS)  
calpuff_results_all %>% lapply('[[', 'inpfiles_created') %>% unlist -> inpfiles_created
names(calpuff_results_all) <- gsub(paste0('.*/','|_CALPUFF.*\\.inp'), '', inpfiles_created)  # TO DO : delete calmet_result$run_name in run name !
names(inpfiles_created) <- names(calpuff_results_all)

# ========================== Scenario definition ===============================
# --- Scenario ScAll : reference case (all stations)
# scenario_prefix <- "ScA"   # Max 8 chars  
# included_stations <- names(inpfiles_created)
# included_stations <- names(inpfiles_created)[grep("_O", names(inpfiles_created) )]

# --- Four main scenarios : 

# --- Scenario ScA
if (i_Sc==1) {scenario_prefix <- "ScA_all" ; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='SO2 compliance, 85% utilization']}
if (i_Sc==2) {scenario_prefix <- "ScA_12"  ; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='SO2 compliance, 85% utilization'][1:2]}
if (i_Sc==3) {scenario_prefix <- "ScA_34" ; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='SO2 compliance, 85% utilization'][3:4]}
if (i_Sc==4) {scenario_prefix <- "ScA_5"  ; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='SO2 compliance, 85% utilization'][5]}

# --- Scenario B 
if (i_Sc==5) {scenario_prefix <- "ScB_all" ; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='SO2 compliance, actual utilization' & emissions_data$year =='2020']} 
if (i_Sc==6) {scenario_prefix <- "ScB_12"  ; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='SO2 compliance, actual utilization' & emissions_data$year =='2020'][1:2]}  
if (i_Sc==7) {scenario_prefix <- "ScB_34" ; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='SO2 compliance, actual utilization' & emissions_data$year =='2020'][3:4]}  
if (i_Sc==8) {scenario_prefix <- "ScB_5"  ; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='SO2 compliance, actual utilization' & emissions_data$year =='2020'][5]}  

# --- Scenario C
if (i_Sc==9) {scenario_prefix <- "ScC_all"; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='85% utilization']}
if (i_Sc==10){scenario_prefix <- "ScC_12" ; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='85% utilization'][1:2]}
if (i_Sc==11){scenario_prefix <- "ScC_34"; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='85% utilization'][3:4]}
if (i_Sc==12){scenario_prefix <- "ScC_5" ; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='85% utilization'][5]}

# --- Scenario D 
if (i_Sc==13){scenario_prefix <- "ScD_all"; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='actual utilization' & emissions_data$year =='2020']}
if (i_Sc==14){scenario_prefix <- "ScD_12" ; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='actual utilization' & emissions_data$year =='2020'][1:2]}
if (i_Sc==15){scenario_prefix <- "ScD_34"; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='actual utilization' & emissions_data$year =='2020'][3:4]}
if (i_Sc==16){scenario_prefix <- "ScD_5" ; included_stations <- emissions_data$emission_names[emissions_data$Scenario=='actual utilization' & emissions_data$year =='2020'][5]}


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
  run_timeseries=T,
  run_hourly=c('PM25', 'NO2', 'SO2'),
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


browser()

# ============================= Plant by plant =================================
scenario_prefix="plant_by_plant"

# Load all CAPUFF results, from calpuff_result_*.RDS
calpuff_results_all <- file.path(output_dir, list.files(output_dir, pattern = 'calpuff_result.*\\.RDS')) %>% lapply(readRDS)  
calpuff_results_all %>% lapply('[[', 'inpfiles_created') %>% unlist -> inpfiles_created
names(calpuff_results_all) <- gsub(paste0('.*/','|_CALPUFF.*\\.inp'), '', inpfiles_created)  # TO DO : delete calmet_result$run_name in run name !
names(inpfiles_created) <- names(calpuff_results_all)

# Select stations
# included_stations <- names(inpfiles_created)[grep("_O", names(inpfiles_created) )]  # Only for operating stations
included_stations <- names(inpfiles_created)  # All stations

calpuff_results_all[names(calpuff_results_all) %in% included_stations == TRUE]  -> calpuff_results_all
inpfiles_created[names(inpfiles_created) %in% included_stations == TRUE]  -> inpfiles_created
emissions_data %>% filter(emissions_data$emission_names %in% names(inpfiles_created)  == TRUE)  -> emissions_data_scenario
write_xlsx(list("CALPUFF input"=emissions_data_scenario), file.path(emissions_dir, paste0("coordinates_",scenario_prefix,".xlsx")))

for(i in seq(1,length(calpuff_results_all))) {
  run_name <- names(calpuff_results_all[i])
  calpuff_result <- calpuff_results_all[[i]]
  print(paste0("Cluster name : ",run_name, " [", i,"/",length(calpuff_results_all),"]"))
  
  creapuff::runPostprocessing(
    calpuff_inp=calpuff_result$inpfiles_created,
    output_dir=output_dir,
    files_met=calpuff_result$out_files,  # out_files
    run_name=run_name,
    # sources=NULL,        # calpuff_result$sources,
    pm10fraction=calpuff_result$pm10fraction, # For mercury (Hg)
    pu_exe=pu_exe,
    pu_templates=pu_templates,
    calpost_exe=calpost_exe,
    calpost_templates=calpost_templates,
    # nper=8760,    # Number of run hours for PU (CP will run over a calendar year).
    # METRUN=1,     # Set 1 in order to run CALPOST on all periods in file, for both PU and CP (good for tests).
    nper=NULL,      # Real cases : neither nper nor METRUN (PU uses all available periods, while CP one calendar year)
    METRUN=0,       # Real cases : neither nper nor METRUN (PU uses all available periods, while CP one calendar year)
    cp_period_function=get_cp_period,
    pu_start_hour=NULL,
    cp_species=c('PM25', 'TPM10', 'SO2', 'NO2'), #c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2'),
    run_discrete_receptors=T,
    run_gridded_receptors=F,
    run_concentrations=T,  # Run "repartition" and "total PM" (PU)
    run_deposition=T,      # Do NOT run "deposition" (PU) if there is NO mercury in emission file
    run_timeseries=T,
    run_hourly=c('PM25', 'NO2', 'SO2'),
    run_pu=T,
    run_calpost=T,
  )
} 


