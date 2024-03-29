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


# Parameters ###################################################################
# ============================= Project specific ===============================
expand_grids = '*'  # All grids are expanded (for CALMET)
expand_ncells = -5  # Number of cells to expand met grid (e.g., WRF) in each direction (use negative values to crop)

# project_dir="F:/projects/chile_test" # calpuff_data persistent disk (calpuff config data)
# project_dir="Z:/projects/chile"      # network disk (wrf_data)
project_dir="G:/projects/chile"        # calpuff_external_data persistent disk (project data)

output_dir <- file.path(project_dir,"calpuff_suite") # Where to write all generated files
wrf_dir <- file.path(project_dir,"calwrf") # Where calwrf data are stored (if Z disk is not present: mount 10.58.186.210:/wrf_data Z:)

emission_type = "varying"  # Real emissions

# emission_type = "constant"  # Emissions and stations are NOT real, just a test case 
if (emission_type == "constant"){
  emissions_dir <- file.path(project_dir,"emissions") # Directory where arbitrary-varying emission files are stored
  input_xls <- file.path(emissions_dir,"emissions_test.xlsx") # File where constant-emission data are specified
  # Emission file, required fields: 
  # Plants	Capacity[MW] Lat[deg]	Long[deg]	Status	COD[year]	SO2_tpa[t/y]	NOx_tpa[t/y]	PM_tpa[t/y]	Hg_kgpa[kg/y]	Exit temperature[C]	Stack diameter[m]	Stack height[m]	Flue gas velocity[m/s] FGD[logical]
}
  
if (emission_type == "varying") {
  emissions_dir <- file.path(project_dir,"emissions/2019") # Directory where arbitrary-varying emission files are stored
  # Corrected version file, for tres_puentes power plant position (erroneous location in the original PTEMARB.DAT file)
  input_xls <- file.path(emissions_dir,"coordinates_corrected.xlsx") # Where plant positions are reported
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

calpuff_template <- file.path(template_dir,"CALPUFF_7.0_template.INP")       # No mercury in emission file 
# calpuff_template <- file.path(template_dir,"CALPUFF_7.0_template_Hg.INP")  # Mercury (Hg) in emission file 

pu_templates <- list (# sumruns="AfsinFut_postutil_sumruns.inp",
                      repartition = file.path(template_dir, "Mintia_postutilRepartition_noHg.inp"),  # No mercury in emission file 
                      # repartition = file.path(template_dir, "Mintia_postutilRepartition.inp"),     # Mercury (Hg) in emission file 
                      deposition = file.path(template_dir, "Mintia_postutil_depo.inp"), 
                      total_pm = file.path(template_dir, "Mintia_postutil_PM10_noHg.inp"))  # No mercury in emission file
                      # total_pm = file.path(template_dir, "Mintia_postutil_PM10.inp"))     # Mercury (Hg) in emission file 

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
#   run_calmet = T
# )
#  
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
  emissions_data$COD %>% substr(.,nchar(.)-3,nchar(.)) %>% as.numeric () <   
    calmet_result$start_dates[[1]] %>% format("%Y") %>% as.numeric() ->  
    emissions_data$existing
  emissions_data$Status = ifelse(emissions_data$existing,'operating', 'future')  

  # Selection of operating/future plants
  # emissions_data %<>% filter(Status == "operating")

  # Produce unique ascii names with 8 characters
  emissions_data$Plants %>% gsub(' ', '', .) %>% substr(1,5) %>% stringi::stri_trans_general("Latin-ASCII") %>%  
    make.names %>% make.unique(sep='') %>% 
    paste0('_', substr(emissions_data$Status,1,1)) -> emissions_data$emission_names
  if (emissions_data$emission_names %>% nchar %>% max > 8) stop("ERROR in plant-name length (too much plants with the same name?)")                        

  # Ensure numeric values of lat and long  
  emissions_data$Lat %<>% as.numeric()
  emissions_data$Long %<>% as.numeric()

  # Create polygons of grid boundaries  
  dom_pols = grids_to_domains(calmet_result$grids, target_crs)
  
  # Exclude sources outside domain  #   
  # latlon_crs = CRS("+proj=longlat +datum=WGS84")
  emissions_data %<>% to_spdf %>% crop(spTransform(dom_pols, crs(.))) %>% '@'('data')
  
  # If not specified, define if/which emission sources have FGD  
  if(!is.null(emissions_data$FDG)) emissions_data$FDG=T

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
  # emissions_data_plus_nueva <- emissions_data
  # emissions_data %<>% filter(Plants!='nueva_tocopilla')
  # emissions_data %<>% head(1)
  # emissions_data %<>% tail(1)
  # emissions_data <- rbind(emissions_data %>% head(1), emissions_data %>% tail(1))
}

# ============================== Receptors =====================================
# MESHDN parameter (in CALPUFF.INP) which defines the grid spacing 
# (DGRIDKM/MECHDN) of each disk, wrt the grid spacing of the outer 
# meteo grid (DGRIDKM). Higher factor: higher density of receptors.
nesting_factors = c(1,5,15)  

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
nesfact_range = c(125,25,5)  # c(125,25,10) # c(150,75,25,10) # Radius of receptor disks [km], from outer to inner disk
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


# CALPUFF ######################################################################
if (emission_type == "constant") {
  calpuff_result <- creapuff::runCalpuff(
  emissions_data = emissions_data,               # For constant emission data
  source_names = emissions_data$emission_names,  # Optional. If not set: read from emissions_data (if not present, set automatically)
  FGD = emissions_data$FGD,                      # Optional. If not set: read from emissions_data (if not present an error occurs)
  receptors = receptors,                         # Optional. If not set: full domain grid 
  o3dat = o3dat,                                 # Optional. If not set: no surface data
  bgconcs = bgconcs,                             # Optional. If not set: std values
  species_configuration = "so2_nox_pm",          # Two possible values: "so2_nox_pm", "so2_nox_pm_hg"
  # addparams = addparams,                       # Optional. If not set: std values
  run_name = calmet_result$run_name,
  output_dir = output_dir,
  params_allgrids = calmet_result$params,
  gis_dir = gis_dir,
  calpuff_exe = calpuff_exe,
  calpuff_template = calpuff_template
)
}

if (emission_type == "varying") {
  queue = unique(emissions_data$emission_names)
  for(run in queue) {
    emissions_data_run <- emissions_data %>% filter(emission_names == run) %>% head(1)
    run_name <- paste(calmet_result$run_name, emissions_data_run$emission_names,sep='_')  # TO DO : delete calmet_result$run_name in run name !
    NPT2 <- emissions_data_run$N_sources  # Number of emission sources per file
    print(paste0("CALPUFF run name: ", run_name, ", n_sources: ", NPT2))
    
    calpuff_result <- creapuff::runCalpuff(
      # emissions_data = emissions_data,     # For constant emissions 
      # source_names = source_names,         # Optional. If not set: read from emissions_data (if not present, set automatically)
      # FGD = "T",                           # Optional. If not set: read from emissions_data (if not present, an error occurs)
      receptors=receptors,                   # Optional. If not set: full domain grid
      o3dat=o3dat,                           # Optional. If not set: no surface data
      bgconcs=bgconcs,                       # Optional. If not set: std values
      species_configuration = "so2_nox_pm",  # Two possible values: "so2_nox_pm", "so2_nox_pm_hg"
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
    run_name <- paste(calmet_result$run_name, emissions_data_run$emission_names,sep='_')  # TO DO : delete calmet_result$run_name in run name !

    bat_file <- file.path(output_dir, paste0(run_name, '_1', '.bat'))
    # shell.exec(normalizePath(bat_file)) 
  }
}


browser()

# POST-PROCESSING ##############################################################
# ============================ All clusters together ============================
# 1. Sum up all output CALPUFF concentrations (.CON), using POSTUTIL
# Load all CAPUFF results, from calpuff_result_*.RDS
calpuff_results_all <- file.path(output_dir, list.files(output_dir, pattern = 'calpuff_result.*\\.RDS')) %>% lapply(readRDS)  
calpuff_results_all %>% lapply('[[', 'inpfiles_created') %>% unlist -> inpfiles_created
names(calpuff_results_all) <- gsub(paste0('.*/',calmet_result$run_name,'_|_CALPUFF.*\\.inp'), '', inpfiles_created)  # TO DO : delete calmet_result$run_name in run name !
names(inpfiles_created) <- names(calpuff_results_all)

# ========================== Scenario definition ===============================
# All data, 26 elements. 

# --- Scenario ScAll : currently operating plants (2019). 13 (over 13) coal and 13 gas plants.
# scenario_prefix <- "ScAll"      # Max 8-char string
# included_stations = c("Andin",  # Coal (2040)
#                       "Guaco",  # Coal (2040)
#                       "Santa1", # Coal (2040) (Santa Maria)
#                       # "Norge",  # Coal (2040) - Composed by 2 units, already included in Tocop (6 units)
#                       
#                       "Angam",  # Coal (2025)
#                       "Bocam",  # Coal (2025)
#                       "Cochr",  # Coal (2025)
#                       "Mejil",  # Coal (2025)
#                       "Tocop",  # Coal (2025+2040) - Also includes Norge (2040)
#                       "Venta",  # Coal (2025)
#                       "Venta1", # Coal (2025)
#                       "Venta2", # Coal (2025)
#                       "Venta3", # Coal (2025)
# 
#                       "Ataca",  # Gas
#                       "Cande",  # Gas
#                       "Cordo",  # Gas
#                       "Huasc",  # Gas
#                       "Los_P",  # Gas
#                       "Los_V",  # Gas
#                       "Nehue",  # Gas
#                       "Quint",  # Gas
#                       "Renca",  # Gas
#                       "San_i",  # Gas
#                       "San_i1", # Gas
#                       "Santa",  # Gas (Santa Lidia)
#                       "Talta"   # Gas
#                       )
                     
# # --- Scenario B : short-term phase-out scenario. 4 (over 13, 9 off) coal and 13 gas plants (shutdown deadlines lass/equal to 2025 are off).
# scenario_prefix <- "ScB"
# included_stations = c(
#                       "Andin",  # Coal (2040)
#                       "Guaco",  # Coal (2040)
#                       "Santa1", # Coal (2040) (Santa Maria)
#                       "Norge",  # Coal (2040) - Composed by 2 units, already included in Tocop (6 units)
# 
#                       # "Angam",  # Coal (2025)
#                       # "Bocam",  # Coal (2025)
#                       # "Cochr",  # Coal (2025)
#                       # "Mejil",  # Coal (2025)
#                       # "Tocop",  # Coal (2025+2040)
#                       # "Venta",  # Coal (2025)
#                       # "Venta1", # Coal (2025)
#                       # "Venta2", # Coal (2025)
#                       # "Venta3", # Coal (2025)
# 
#                       "Ataca",  # Gas
#                       "Cande",  # Gas
#                       "Cordo",  # Gas
#                       "Huasc",  # Gas
#                       "Los_P",  # Gas
#                       "Los_V",  # Gas
#                       "Nehue",  # Gas
#                       "Quint",  # Gas
#                       "Renca",  # Gas
#                       "San_i",  # Gas
#                       "San_i1", # Gas
#                       "Santa",  # Gas (Santa Lidia)
#                       "Talta"   # Gas
#                       )

# --- Scenario C : long-term phase-out scenario.  0 (over 13, 13 off) coal and 13 gas plants (shutdown deadlines lass/equal to 2040 are off).
scenario_prefix <- "ScC"  
included_stations = c(
                      # "Andin",  # Coal (2040)
                      # "Guaco",  # Coal (2040)
                      # "Santa1", # Coal (2040) (Santa Maria)
                      # "Norge",  # Coal (2040) - Composed by 2 units, already included in Tocop (6 units)
                      #
                      # "Angam",  # Coal (2025)
                      # "Bocam",  # Coal (2025)
                      # "Cochr",  # Coal (2025)
                      # "Mejil",  # Coal (2025)
                      # "Tocop",  # Coal (2025+2040)
                      # "Venta",  # Coal (2025)
                      # "Venta1", # Coal (2025)
                      # "Venta2", # Coal (2025)
                      # "Venta3", # Coal (2025)

                      "Ataca",  # Gas
                      "Cande",  # Gas
                      "Cordo",  # Gas
                      "Huasc",  # Gas
                      "Los_P",  # Gas
                      "Los_V",  # Gas
                      "Nehue",  # Gas
                      "Quint",  # Gas
                      "Renca",  # Gas
                      "San_i",  # Gas
                      "San_i1", # Gas
                      "Santa",  # Gas (Santa Lidia)
                      "Talta"  # Gas
                      )

# --- Scenario D : new emission standards (?)
# scenario_prefix <- "ScD"
# included_stations = c("")

calpuff_results_all[names(calpuff_results_all) %in% included_stations == TRUE]  -> calpuff_results_all
inpfiles_created[names(inpfiles_created) %in% included_stations == TRUE]  -> inpfiles_created
emissions_data %>% filter(emissions_data$emission_names %in% names(inpfiles_created)  == TRUE)  -> emissions_data_scenario
write_xlsx(list("CALPUFF input"=emissions_data_scenario), file.path(emissions_dir, paste0("coordinates_",scenario_prefix,".xlsx")))

# ==============================================================================
# 1. Create "SUMRUNS" INP files for summing up all CALPUFF outputs for each station, for :
# - concentrations (.CON), no need for nitrate reparation (MNITRATE = 0), a further run will do the repartition
files_met <- out_files  # calpuff_results_all[[1]]$out_files  # All clusters have the same meteo
first_cluster_inp <- inpfiles_created[1]
first_cluster_name <- names(inpfiles_created)[1]

# Generate "generic" PU and CP INP files (only for the first cluster, run_pu=F, run_calpost=F)
creapuff::runPostprocessing(
  calpuff_inp=first_cluster_inp,
  output_dir=output_dir,
  files_met = files_met,
  pm10fraction=NULL,
  METRUN = 0,  
  nper = NULL,
  pu_start_hour = NULL,
  cp_species = c('PM25', 'TPM10', 'SO2', 'NO2'),  # c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2'),
  cp_period_function = get_cp_period,
  run_discrete_receptors=T,
  run_gridded_receptors=F,
  run_concentrations=T,
  run_deposition=F,
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
readLines(file.path(output_dir,pu_sumruns_template_generic)) -> inp

# Define current input-data list  
sumfiles = paste0(toupper(calmet_result$run_name),'_',toupper(names(inpfiles_created)), '.CON')  

# Fill generic inp files with current parameters and output-data names 
inp %<>% set_puff(list(NFILES = length(sumfiles),  # Function to set parameters in a CALPUFF input file
                       MNITRATE  =  0,             # Recompute the HNO3/NO3 partition for concentrations, for all sources combined
                       UTLLST = file.path(output_dir,paste0(scenario_prefix,"_POSTUTIL_SUMRUNS.LST")),  # Output LST file
                       UTLDAT = file.path(output_dir,paste0(scenario_prefix,".CON"))))                  # Output data file, for concentrations (.CON)

# Fill the corresponding lines for with current input-data list (CALPUFF outputs)
con_line <- grep("!MODDAT=", gsub(" ", "", inp))    # Looking for input-data position index 
inp <- c(inp[1:(con_line-1)],
         paste("   ",1:length(sumfiles),"            CALPUFF.DAT        ! MODDAT =",file.path(output_dir, sumfiles),"! !END!"),
         inp[-1:-con_line])

# Write the _POSTUTIL_SUMRUNS.INP 
pu_sumruns_template_all <- file.path(output_dir, paste0(scenario_prefix,"_POSTUTIL_SUMRUNS.INP"))
writeLines(inp, pu_sumruns_template_all) 

# Create the _SUMRUNS.bat bat file
pu_sumruns_bat <- file.path(output_dir, paste0("pu_",scenario_prefix,"_SUMRUNS.bat"))
writeLines(c(paste("cd", output_dir),
             paste0(pu_exe, " ", normalizePath(pu_sumruns_template_all)),
             "pause"), 
           pu_sumruns_bat)  # shell.exec(normalizePath(pu_sumruns_bat)) 


# 2. Define PU and CP INP and bat files, for summed-up concentrations   
name_generic=paste(calmet_result$run_name, first_cluster_name,sep='_')  # e.g., chile_andin  

# PU INP: change names of input and output parameters, from generic "first_cluster_name" to selected scenario (ScAll, or ScB, ...)
file.path(output_dir, list.files(output_dir, pattern=paste0('^',first_cluster_name,'_postutil'))) -> pu_files_generic
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


browser()


# ============================= Plant by plant =================================
scenario_prefix="plant_by_plant"

# Load all CAPUFF results, from calpuff_result_*.RDS
calpuff_results_all <- file.path(output_dir, list.files(output_dir, pattern = 'calpuff_result.*\\.RDS')) %>% lapply(readRDS)  
calpuff_results_all %>% lapply('[[', 'inpfiles_created') %>% unlist -> inpfiles_created
names(calpuff_results_all) <- gsub(paste0('.*/','|_CALPUFF.*\\.inp'), '', inpfiles_created)  # TO DO : delete calmet_result$run_name in run name !
names(inpfiles_created) <- names(calpuff_results_all)

# Select stations
included_stations <- ... ? ...    # Only for operating stations
# included_stations <- names(inpfiles_created)  # All stations

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
    cp_period_function = get_cp_period,
    pu_start_hour = NULL,
    cp_species=c('PM25', 'TPM10', 'SO2', 'NO2'), #c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2'),
    run_discrete_receptors=T,
    run_gridded_receptors=F,
    run_concentrations=T,  # Run "repartition" and "total PM" (PU)
    run_deposition=T,      # Do NOT run "deposition" (PU) if there is NO mercury in emission file
    run_timeseries = T,
    run_hourly = c('PM25', 'NO2', 'SO2'),
    run_pu=T,
    run_calpost=T,
  )
} 




# ============================= Cluster by cluster =============================
calpuff_results_all <- file.path(output_dir, list.files(output_dir, pattern = 'calpuff_result.*\\.RDS')) %>% lapply(readRDS)  
calpuff_results_all %>% lapply('[[', 'inpfiles_created') %>% unlist -> inpfiles_created
names(calpuff_results_all) <- gsub(paste0('.*/',calmet_result$run_name,'_|_CALPUFF.*\\.inp'), '', inpfiles_created)  # TO DO : delete calmet_result$run_name in run name !

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
    pm10fraction=NULL,     # calpuff_result$pm10fraction, # For mercury (Hg)
    pu_exe=pu_exe,
    pu_templates=pu_templates,
    calpost_exe=calpost_exe,
    calpost_templates=calpost_templates,
    # nper=8760,    # Number of run hours for PU (CP will run over a calendar year).
    # METRUN=1,     # Set 1 in order to run CALPOST on all periods in file, for both PU and CP (good for tests).
    nper=NULL,      # Real cases : neither nper nor METRUN (PU uses all available periods, while CP one calendar year)
    METRUN=0,       # Real cases : neither nper nor METRUN (PU uses all available periods, while CP one calendar year)
    cp_period_function = get_cp_period,
    pu_start_hour = NULL,
    cp_species=c('PM25', 'TPM10', 'SO2', 'NO2'), #c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2'),
    run_discrete_receptors=T,
    run_gridded_receptors=F,
    run_concentrations=T,  # Run "repartition" and "total PM" (PU)
    run_deposition=F,      # Do NOT run "deposition" (PU) if there is NO mercury in emission file
    run_timeseries = T,
    run_hourly = c('PM25', 'NO2', 'SO2'),
    run_pu=T,
    run_calpost=T,
  )
} 

# queue = unique(emissions_data$emission_names)
# for(run in queue) {
#   emissions_data_run <- emissions_data %>% filter(emission_names == run) %>% head(1)
#   run_name <- paste(calmet_result$run_name, emissions_data_run$emission_names,sep='_')  # TO DO : delete calmet_result$run_name in run name !
#   calpuff_result <- file.path(output_dir, paste0('calpuff_result_',run_name, '.RDS'))  %>% readRDS
#   post_processing_run_name <- emissions_data_run$emission_names  # Max 8-chars name, for CALPOST run
#   
#   creapuff::runPostprocessing(
#     calpuff_inp=calpuff_result$inpfiles_created,
#     output_dir=output_dir,
#     files_met=calpuff_result$out_files,  # out_files
#     # sources=NULL,      # calpuff_result$sources,
#     pm10fraction=NULL,   # calpuff_result$pm10fraction, # For mercury (Hg)
#     pu_exe=pu_exe,
#     pu_templates=pu_templates,
#     calpost_exe=calpost_exe,
#     calpost_templates=calpost_templates,
#     run_name=post_processing_run_name,
#     # nper=8760,    # Number of run hours for PU (CP will run over a calendar year).
#     # METRUN=1,     # Set 1 in order to run CALPOST on all periods in file, for both PU and CP (good for tests).
#     nper=NULL,      # Real cases : neither nper nor METRUN (PU uses all available periods, while CP one calendar year)
#     METRUN=0,       # Real cases : neither nper nor METRUN (PU uses all available periods, while CP one calendar year)
#     cp_period_function = get_cp_period,
#     pu_start_hour = NULL,
#     cp_species=c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2'),
#     run_discrete_receptors=T,
#     run_gridded_receptors=F,
#     run_concentrations=T,  # For "repartition" and "total PM" (PU)
#     run_deposition=F,      # Do NOT run "deposition" (PU) if there is NO mercury in emission file
#     run_timeseries = T,
#     run_hourly = c('PM25', 'NO2', 'SO2'),
#     run_pu=T,
#     run_calpost=T,
#   )
# }