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
                      total_pm = file.path(template_dir, "Mintia_postutil_PM10.inp"),
                      sumruns=file.path(template_dir, "AfsinFun_postutil_sumruns.inp"))     # Mercury (Hg) in emission file 

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
  
  #aggregate Camden's four stacks into one to speed things upW
  emissions_data %<>% mutate(stack=ifelse(plant=='Camden', "1", stack),
                            emission_names=ifelse(plant=='Camden', "Camden1", emission_names)) %>% 
    group_by(plant, stack, AQCS, FGD, emission_names) %>% 
    summarise(across(c(lat, lon, stack.height:velocity), mean),
              across(Hg:SO2, sum))
  
  if (emissions_data$emission_names %>% nchar %>% max > 8) stop("ERROR in plant-name length (too much plants with the same name?)")                        

  # Create polygons of grid boundaries  
  dom_pols = grids_to_domains(calmet_result$grids, target_crs)
  
  # Exclude sources outside domain  
  emissions_data %<>% to_spdf %>% raster::crop(spTransform(dom_pols, raster::crs(.))) %>% '@'('data')
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
    area_sources=emis_mine %>% filter(!grepl('Groot', emission_names)),
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
  
  #Grootegeluk mine
  runCalpuff(
    #emissions_data = emis_ipp,               # For constant emission data
    #source_names = emis_ipp$emission_names,  # Optional. If not set: read from emissions_data (if not present, set automatically)
    #FGD = T,                      # Optional. If not set: read from emissions_data (if not present an error occurs)
    area_sources=emis_mine %>% filter(grepl('Groot', emission_names)),
    receptors = receptors,                             # Optional. If not set: full domain grid
    o3dat = o3dat,                                     # Optional. If not set: no surface data
    species_configuration = "so2_nox_pm_hg",           # Two possible values: "so2_nox_pm" or "so2_nox_pm_hg"
    bgconcs = bgconcs,                                 # Optional. If not set: std values
    run_name = 'Grootge',   
    output_dir = output_dir,
    params_allgrids = calmet_result$params,
    gis_dir = gis_dir,
    calpuff_exe = calpuff_exe,
    calpuff_template = calpuff_template,
  )
  
  queue %<>% c('LCPP_IPP', 'LCPP_mine', 'Grootge') %>% unique
  
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
queue <- runs %>% paste0(project_dir, '/calpuff_suite/', . , '_calpost.lst') %>% file.exists() %>% not %>% '['(names(inpfiles_created), .)
queue <- runs[!grepl('LCPP', runs)]

calpuff_results_all[['LCPP_mine']]$pm10fraction <- .45e-3

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


list.files(output_dir, pattern='grootvl.*\\.(csv|dat)', full.names = T) %>% 
  (function(x) file.rename(x, gsub('grootvl', 'grootvlei', x)))

#postutil scaling for Lephalale+Grootegeluk+Medupi+Matimba max emissions
emis_lepha %>% filter(grepl('IPP|Groot|Lepha', source)) %>% 
  mutate(mitigated = !grepl('unmitigated', source),
         source = source %>% gsub(' .*', '', .)) %>% 
  group_by(source) %>% summarise(across(c(pm=TSP, so2=SO2, nox=NOx_tpa, hg=Hg_kgpa), ~.x[!mitigated]/.x[mitigated])) ->
  scaling_lepha
emissions_data %>% filter(plant %in% c('Medupi', 'Matimba')) %>% group_by(plant) %>% summarise(across(Hg:SO2, sum))

scaling <- list(mine_unmitigated=scaling_lepha %>% filter(source=='Lephalale') %>% select(pm), 
                background_unmitigated=list(Grootge=scaling_lepha %>% filter(source=='Grootgeluk') %>% select(pm),
                                            Medupi=tibble(pm=250),
                                            Matimba=tibble(pm=50)))

scaling$mine_pp_unmitigated=list(LCPP_mine=scaling$mine_unmitigated,
                                 LCPP_IPP=scaling_lepha %>% filter(source=='IPP') %>% select(-source))

#mn&pp mitigated
#unmitigated
#background = medupi, matimba, grootegeluk
#with background = background + mn&pp
#bg and wbg unmitigated
run_queue <- list(list(run_name_out='mine_unmitigated',
                       run_name='LCPP_mine',
                       cp_run_name = 'mnum',
                       emissions_scaling = scaling$mine_unmitigated),
                  list(run_name_out='mine_IPP_unmitigated',
                       run_name=c('LCPP_mine','LCPP_IPP'),
                       cp_run_name = 'mnppum',
                       emissions_scaling = scaling$mine_pp_unmitigated),
                  list(run_name_out='mine_IPP',
                       run_name=c('LCPP_mine','LCPP_IPP'),
                       cp_run_name = 'mnpp',
                       emissions_scaling = NULL),
                  list(run_name_out='background',
                       run_name=c('Medupi', 'Matimba', 'Grootge'),
                       cp_run_name = 'bg',
                       emissions_scaling = NULL),
                  list(run_name_out='background_unmitigated',
                       run_name=c('Medupi', 'Matimba', 'Grootge'),
                       cp_run_name = 'bgum',
                       emissions_scaling = scaling$background_unmitigated),
                  list(run_name_out='mine_IPP_background',
                       run_name=c('LCPP_mine', 'LCPP_IPP', 'Medupi', 'Matimba', 'Grootge'),
                       cp_run_name = 'mnppbg',
                       emissions_scaling = NULL),
                  list(run_name_out='mine_background',
                       run_name=c('LCPP_mine', 'Medupi', 'Matimba', 'Grootge'),
                       cp_run_name = 'mnbg',
                       emissions_scaling = NULL),
                  list(run_name_out='mine_IPP_background_unmitigated',
                       run_name=c('LCPP_mine', 'LCPP_IPP', 'Medupi', 'Matimba', 'Grootge'),
                       cp_run_name = 'mnppumbg',
                       emissions_scaling = scaling$mine_pp_unmitigated),
                  list(run_name_out='mine_background_unmitigated',
                       run_name=c('LCPP_mine', 'Medupi', 'Matimba', 'Grootge'),
                       cp_run_name = 'mnumbg',
                       emissions_scaling = list(LCPP_mine=scaling$mine_unmitigated)))


#already modeled
# LCPP_IPP~power plant
# LCPPmine~mine

#to model
# mine unmitigated
# mine+power plant
# mine+power plant unmitigated
# background
# mine w background
# mine+power plant w background
# mine unmitigated w background
# mine+power plant unmitigated w background

                                    
run_queue %>% 
  lapply(function(x) {
    message(x$run_name_out)
    runPostprocessing(
      calpuff_inp=inpfiles_created['LCPP_mine'],
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
      run_hourly = c('SO2'), #c('PM25', 'NO2', 'SO2'),
      emissions_scaling = x$emissions_scaling,
      run_pu=F,
      run_calpost=T,
      pu_templates = pu_templates,
      calpost_templates=calpost_templates
    )
  })






#SUMRUNS for Lephalale+Grootegeluk+Medupi+Matimba, and without Lephalale
runs_to_sum=c('LCPP_mine', 'LCPP_IPP', 'Medupi', 'Matimba', 'Grootge')
run_to_create='LCPP_wbg'

runPostprocessing(
  calpuff_inp=inpfiles_created[runs_to_sum[1]],
  run_name=runs_to_sum,
  run_name_out = 'LCPP_with_background',
  cp_run_name = 'LCPPwbg',
  output_dir=output_dir,
  files_met = out_files,
  pm10fraction=calpuff_results_all[[scen]]$pm10fraction,
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
  run_hourly = c(), #c('PM25', 'NO2', 'SO2'),
  #emissions_scaling = list(pm=scaling_lepha$pm[scaling_lepha$source=='Lephalale']),
  run_pu=F,
  run_calpost=F,
  pu_templates = pu_templates,
  calpost_templates=calpost_templates
)









# 3. Run all bat files, in sequence
system2(pu_sumruns_bat) -> exit_code
if(exit_code != 0) stop("errors in POSTUTIL SUMRUNS execution")
system2(pu_bat) -> exit_code
if(exit_code != 0) stop("errors in POSTUTIL execution")
system2(cp_bat) -> exit_code
if(exit_code != 0) stop("errors in CALPOST execution")


