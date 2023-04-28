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

# Parameters ###################################################################
# ============================= Project specific ===============================
expand_grids = '*'  # All grids are expanded (for CALMET)
expand_ncells = -5  # Number of cells to expand met grid (e.g., for WRF data, exclusion of last 5 cells) in each direction (use negative values to crop).
crop_grid = list(extent(c(xmin=-2000, xmax=6000, ymin=8000, ymax=11500)), NA, NA)

# project_dir="Z:/"            # network disk (wrf_data). If Z disk is not present: mount 10.58.186.210:/wrf_data Z:)
project_dir="G:/IndonesiaIESR"       # calpuff_external_data-2 persistent disk (project data)

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
  only_make_additional_files=F,
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


# browser()

# ============================== Receptors =====================================
# MESHDN parameter (in CALPUFF.INP) which defines the grid spacing 
# (DGRIDKM/MECHDN) of each disk, wrt the grid spacing of the outer 
# meteo grid (DGRIDKM). Higher factor: higher density of receptors.
#
base_res <- calmet_result$params %>% sapply('[[', 'DGRIDKM') %>% as.numeric %>% max

nesting_factors = c(1,2,6,12,30)  # 60km, 30km, 10km, 5km, 2km  # c(1,2,5,15) 
#nesting_factors = c(1,5,15)  # 15km, 3km, 1km 

if(!exists('receptors')) receptors = list()
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

saveRDS(receptors, file.path(output_dir, 'receptors.RDS'))
receptors <- readRDS(file.path(output_dir, 'receptors.RDS'))

# Select discrete receptors around sources
# Radius of receptor disks [km], from outer to inner disk
# 
#nesfact_range = c(125,50,25,5) # c(150,75,25,5)  # c(150,75,25,10)  # c(125,75,25,5)  
nesfact_range = c(750, 300, 100, 50, 20)   # c(125,25,10) 

# CALPUFF ######################################################################
queue = unique(emissions_data$emission_names)
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

for(run in queue) {
  
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
  quickpng(file.path(output_dir, paste0(calmet_result$run_name, '_', 'receptors+background_grid.png'))  )
  receptors_run %>% subset(include==1) %>% plot(cex=.2)
  plot(sources, col='blue', add=T)
  shp_utm %>% subset(NAME_0=='Indonesia') %>% plot(add=T, border='gray')
  dev.off()
  
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
# Execute CALPUFF bat files
## Option 1 : Execute each CALPUFF bat file separately
# for(run in queue) {
#   emissions_data_run <- emissions_data %>% filter(emission_names == run) %>% head(1)
#   run_name <- emissions_data_run$emission_names  
#   bat_file <- file.path(output_dir, paste0(run_name, '_1', '.bat'))
#   shell.exec(normalizePath(bat_file))
#   Sys.sleep(10)
# } 
## Option 2 : execute several CALPUFF runs using a same bat file (final number of bat files depends on available CPUs)
N_CPUs <- detectCores()
N_bat_files <- length(queue)
bat_file_lines <- round(N_bat_files/N_CPUs)  # CHECK : or ceiling() ?
for (i in seq(1, N_bat_files, by = bat_file_lines)){
  i_end <- (i+bat_file_lines-1)
  run <- queue[i:i_end] %>% subset(!is.na(.))
  patterns=paste0(run, '_1', '.bat')
  bat_files_list <- lapply(patterns, function(x){list.files(output_dir, pattern = x,  full.names=TRUE)})
  bat_files_text <- sapply(bat_files_list, function(x){readLines(x,n=1)}) %>% append("pause")
  bat_file <- file.path(output_dir, paste0("calpuff_batgroup_",i,"-",i_end,".bat"))
  bat_files_text %>% writeLines(bat_file)
  # shell.exec(normalizePath(bat_file))
  # Sys.sleep(2)
}



# browser()

for (i_Sc in seq(1,1)) {
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

# --- Scenarios : 
if (i_Sc==1) {scenario_prefix <- "ScAll"; included_stations <- emissions_data$emission_names}
# if (i_Sc==2) {scenario_prefix <- "ScB"; included_stations <- emissions_data$emission_names}
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
  nper = NULL, # 4344, # NULL,
  pu_start_hour = NULL,
  cp_species = c('PM25', 'TPM10', 'SO2', 'NO2'),  # c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2'),
  cp_period_function = get_cp_period,
  run_discrete_receptors=T,
  run_gridded_receptors=F,
  run_concentrations=T,
  run_deposition=F,    # no deposition   
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

