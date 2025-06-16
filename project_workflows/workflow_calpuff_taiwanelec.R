# Set up the environment
# remotes::install_github("energyandcleanair/creapuff", dependencies=T, update=F)
# devtools::reload(pkgload::inst("creapuff"))

remotes::install_github("energyandcleanair/creapuff", ref="fix/digits")

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
library(ggplot2)



# Parameters ###################################################################
# ============================= Project specific ===============================
expand_grids = '*'  # All grids are expanded (for CALMET)
expand_ncells = -5  # Number of cells to expand met grid (e.g., for WRF data, exclusion of last 5 cells) in each direction (use negative values to crop).

# project_dir="Z:/"                 # network disk (wrf_data). If Z disk is not present: mount 10.58.186.210:/wrf_data Z:)
project_dir="H:/taiwan" 

wrf_dir <- file.path(project_dir,"calwrf") # Where calwrf data are stored 

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
#rename grid files to remove underscores that mess up formatting
#list.files(path = wrf_dir, pattern = '\\.m3d$', recursive = F, full.names = T) -> m3d_files
#m3d_files %>% gsub('_d01', '-d01', .) %>% gsub('BrazilTernium-', 'BrazilTernium_', .) %>% file.rename(m3d_files, .)
#m3d_files %>% gsub('-d01', '_d01', .)  %>% file.rename(m3d_files, .)
#m3d_files %>% gsub('-d02', '_d02', .)  %>% file.rename(m3d_files, .)
#m3d_files %>% gsub('AusMidArm', 'ausmidarm', .)  %>% file.rename(m3d_files, .)


calmet_result <- creapuff::runCalmet(
  #input_xls = input_xls,
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

##################################################################
#                      Prepare emissions                         #
##################################################################

# Load emission file
emissions_data = read_xlsx(file.path(emissions_dir, 'ICT.xlsx'), sheet='consol_JMK')


# Filter out rows where status == 'retired' (N = 10)
emissions_data <- emissions_data %>% 
  filter(status != "retired")

emissions_data <- emissions_data %>%
  filter(data_year == 2030)

# Filter out rows where status == 'retired' (N = 10)
emissions_data <- emissions_data %>% 
  filter('GEM ID' != "G100000406190")

# Filter out rows where status == 'retired' (N = 10)
emissions_data <- emissions_data %>% 
  filter('GEM ID' != "G100000406191")


emissions_data <- emissions_data %>%
  select(all_of(c("GEM ID", "plant",
                  "Annual Total Power Generation (kWh)(2023)",
                  "Grid location", "Unit", "longitude (of the 煙囪)", "latitude (of the 煙囪)", 
                  "2023 NOx tons/yr", "2023 SO2 Tons/yr", "2023 PMx Tons/year",
                  "Height (m)",	"Diameter (m)",	"Speed (m/s)",	"Temperature (℃)")))

##################### Clean up speed data #####################

speed_values <- emissions_data[["Speed (m/s)"]]
speed_clean <- speed_values[!is.na(speed_values) & speed_values != 0]
mean_speed <- mean(speed_clean)

emissions_data[["Speed (m/s)"]][is.na(emissions_data[["Speed (m/s)"]]) | emissions_data[["Speed (m/s)"]] == 0] <- mean_speed

##################### Clean up G100000410601 #####################

reference_values <- emissions_data %>%
  filter(.data[["GEM ID"]] == "G100000410601") %>%
  select(
    .data[["Height (m)"]],
    .data[["Diameter (m)"]],
    .data[["Speed (m/s)"]],
    .data[["Temperature (℃)"]]
  )


emissions_data[emissions_data[["GEM ID"]] == "G100000410605", "Height (m)"] <- reference_values[[1]]
emissions_data[emissions_data[["GEM ID"]] == "G100000410605", "Diameter (m)"] <- reference_values[[2]]
emissions_data[emissions_data[["GEM ID"]] == "G100000410605", "Speed (m/s)"] <- reference_values[[3]]
emissions_data[emissions_data[["GEM ID"]] == "G100000410605", "Temperature (℃)"] <- reference_values[[4]]

##################### Clean up G100000406198 #####################

reference_values <- emissions_data %>%
  filter(.data[["GEM ID"]] == "G100000406197") %>%
  select(
    .data[["Height (m)"]],
    .data[["Diameter (m)"]],
    .data[["Speed (m/s)"]],
    .data[["Temperature (℃)"]]
  )

# G100000406198

emissions_data[emissions_data[["GEM ID"]] == "G100000406198", "Height (m)"] <- reference_values[[1]]
emissions_data[emissions_data[["GEM ID"]] == "G100000406198", "Diameter (m)"] <- reference_values[[2]]
emissions_data[emissions_data[["GEM ID"]] == "G100000406198", "Speed (m/s)"] <- reference_values[[3]]
emissions_data[emissions_data[["GEM ID"]] == "G100000406198", "Temperature (℃)"] <- reference_values[[4]]

# G100000406199

emissions_data[emissions_data[["GEM ID"]] == "G100000406199", "Height (m)"] <- reference_values[[1]]
emissions_data[emissions_data[["GEM ID"]] == "G100000406199", "Diameter (m)"] <- reference_values[[2]]
emissions_data[emissions_data[["GEM ID"]] == "G100000406199", "Speed (m/s)"] <- reference_values[[3]]
emissions_data[emissions_data[["GEM ID"]] == "G100000406199", "Temperature (℃)"] <- reference_values[[4]]

# G100000406200

emissions_data[emissions_data[["GEM ID"]] == "G100000406200", "Height (m)"] <- reference_values[[1]]
emissions_data[emissions_data[["GEM ID"]] == "G100000406200", "Diameter (m)"] <- reference_values[[2]]
emissions_data[emissions_data[["GEM ID"]] == "G100000406200", "Speed (m/s)"] <- reference_values[[3]]
emissions_data[emissions_data[["GEM ID"]] == "G100000406200", "Temperature (℃)"] <- reference_values[[4]]


##################### Group units together #####################


emissions_data[c(
  "Height (m)", "Diameter (m)", "Speed (m/s)", "Temperature (℃)",
  "longitude (of the 煙囪)", "latitude (of the 煙囪)",
  "Annual Total Power Generation (kWh)(2023)",
  "2023 NOx tons/yr", "2023 SO2 Tons/yr", "2023 PMx Tons/year"
)] <- lapply(emissions_data[c(
  "Height (m)", "Diameter (m)", "Speed (m/s)", "Temperature (℃)",
  "longitude (of the 煙囪)", "latitude (of the 煙囪)",
  "Annual Total Power Generation (kWh)(2023)",
  "2023 NOx tons/yr", "2023 SO2 Tons/yr", "2023 PMx Tons/year"
)], as.numeric)



emissions_data <- emissions_data %>%
  group_by(plant) %>%
  summarise(
    grid_location = first(`Grid location`), 
    plant         = first(`plant`), 
    stack_height        = mean(.data[["Height (m)"]], na.rm = TRUE),
    diameter      = mean(.data[["Diameter (m)"]], na.rm = TRUE),
    velocity         = mean(.data[["Speed (m/s)"]], na.rm = TRUE),
    temperature   = mean(.data[["Temperature (℃)"]], na.rm = TRUE),
    longitude     = mean(.data[["longitude (of the 煙囪)"]], na.rm = TRUE),
    latitude      = mean(.data[["latitude (of the 煙囪)"]], na.rm = TRUE),
    Power         = sum(.data[["Annual Total Power Generation (kWh)(2023)"]], na.rm = TRUE),
    NOx_tpa     = sum(.data[["2023 NOx tons/yr"]], na.rm = TRUE),
    SO2_tpa     = sum(.data[["2023 SO2 Tons/yr"]], na.rm = TRUE),
    PM_tpa     = sum(.data[["2023 PMx Tons/year"]], na.rm = TRUE)
  )


library(stringi)

emissions_data$emission_names <- stri_rand_strings(n = nrow(emissions_data), length = 5, pattern = "[A-Za-z0-9]")

write.csv(emissions_data, file = file.path(emissions_dir, "emissions_data.csv"), row.names = FALSE)


##################### Check grouping #####################

total_original <- emissions_data %>%
  summarise(
    total_power = sum(`Annual Total Power Generation (kWh)(2023)`, na.rm = TRUE),
    total_NOx   = sum(`2023 NOx tons/yr`, na.rm = TRUE),
    total_SO2   = sum(`2023 SO2 Tons/yr`, na.rm = TRUE),
    total_PMx   = sum(`2023 PMx Tons/year`, na.rm = TRUE)
  )

total_grouped <- emissions_summary %>%
  summarise(
    total_power = sum(total_Power, na.rm = TRUE),
    total_NOx   = sum(total_NOx, na.rm = TRUE),
    total_SO2   = sum(total_SO2, na.rm = TRUE),
    total_PMx   = sum(total_PMx, na.rm = TRUE)
  )


bind_rows(
  original = total_original,
  grouped  = total_grouped,
  .id = "source"
)

###############################################################




# Create polygons of grid boundaries  
dom_pols = grids_to_domains(calmet_result$grids, target_crs)

# Exclude sources outside domain  
emissions_data %<>% to_spdf %>% raster::crop(spTransform(dom_pols, raster::crs(.))) %>% '@'('data')


# browser()

# ============================== Receptors =====================================
# MESHDN parameter (in CALPUFF.INP) which defines the grid spacing 
# (DGRIDKM/MECHDN) of each disk, wrt the grid spacing of the outer 
# meteo grid (DGRIDKM). Higher factor: higher density of receptors.
nesting_factors = c(1,3,5,15)  # 15km, 5km, 3km, 1km  # c(1,2,5,15) 
#nesting_factors = c(1,5,15)  # 15km, 3km, 1km 

if(!exists('receptors')) receptors = list()

queue = unique(emissions_data$emission_names)


for(run in queue) {
  #emissions_data_run <- allsources %>% filter(emission_names == run) %>% head(1)
  emissions_data_run <- emissions_data %>% filter(emission_names == run) 
  
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

# NOx emiss (kt/year), SOx emiss (kt/year), PM emiss (kt/year), Hg emiss (kt/year) - chosen
# CALPUFF ######################################################################

if (emission_type == "constant") {
  
  queue = unique(emissions_data$emission_names)
  for(run in queue) {
    emissions_data_run <- emissions_data %>% filter(emission_names == run) 
  
    run_name <- run
    print(paste0("CALPUFF run name: ", run_name))
    print (paste0("Dataframe      : ", emissions_data_run))
    calpuff_result <- runCalpuff(
      
        emissions_data = emissions_data_run,               # For constant emission data
        source_names = emissions_data_run$emission_names,  # Optional. If not set: read from emissions_data (if not present, set automatically)
        FGD = 'None',                                      # Optional. If not set: read from emissions_data (if not present an error occurs)
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
queue <- runs %>% paste0(project_dir, '/calpuff_suite/', . , '_calpost.lst') %>% file.exists() %>% not %>% '['(names(inpfiles_created), .)


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

queue <- c('XfWcm', 'JWvcY', 'Fy4hz', 'RpfqZ', 'HSKAs', 'lP3MH')
# where is 39S6P? 

runPostprocessing(
  calpuff_inp=inpfiles_created[queue][1],
  run_name=queue,
  run_name_out='South',
  output_dir=output_dir,
  files_met = out_files,
  pm10fraction=calpuff_results_all[['XfWcm']]$pm10fraction,
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
  #emissions_scaling = simulation_list,
  run_pu=F,
  run_calpost=F,
  pu_templates = pu_templates,
  calpost_templates=calpost_templates
)

queue <- c('uTWEd', 'SgQeX', 'quB0d', 'Fp8kH', 'nJ5B3')

runPostprocessing(
  calpuff_inp=inpfiles_created[queue][1],
  run_name=queue,
  run_name_out='North',
  output_dir=output_dir,
  files_met = out_files,
  pm10fraction=calpuff_results_all[['uTWEd']]$pm10fraction,
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
  #emissions_scaling = simulation_list,
  run_pu=F,
  run_calpost=F,
  pu_templates = pu_templates,
  calpost_templates=calpost_templates
)

queue <- c('NESXx', 'Fsi8R', 'i2KIE', 'anXC2', 'dXrum')

runPostprocessing(
  calpuff_inp=inpfiles_created[queue][1],
  run_name=queue,
  run_name_out='Middle',
  output_dir=output_dir,
  files_met = out_files,
  pm10fraction=calpuff_results_all[['NESXx']]$pm10fraction,
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
  #emissions_scaling = simulation_list,
  run_pu=F,
  run_calpost=F,
  pu_templates = pu_templates,
  calpost_templates=calpost_templates
)









































