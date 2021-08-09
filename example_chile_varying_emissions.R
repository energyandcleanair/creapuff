# remotes::install_github("energyandcleanair/creapuff", dependencies=T, update=F)
# devtools::reload(pkgload::inst("creapuff"))
library(creapuff)

library("readxl")
library("lubridate")
library("tidyverse")
library("magrittr")
library("pbapply")

# Parameters ###################################################################

# Project specific
expand_grids = '*'  # All grids are expanded 
expand_ncells = -5  # Number of cells to expand met grid (e.g., WRF) in each direction (use negative values to crop)

# project_dir="F:/projects/chile_test"
project_dir="Z:/projects/chile" #_test"

emission_type = "varying"  # Real emissions
# emission_type = "constant"  # Emissions and stations are NOT real, just a test case 

if (emission_type == "constant") 
  input_xls <- file.path(project_dir,"/emissions_test.xlsx") # File where constant-emission data are specified

if (emission_type == "varying") {
  emissions_dir <- file.path(project_dir,"/emissions/2019") # Directory where arbitrary-varying emission files are stored
  input_xls <- file.path(emissions_dir,"coordinates.xlsx") # Where plant positions are reported
}
output_dir <- file.path(project_dir,"/calpuff_suite") # Where to write all generated files
wrf_dir <- file.path(project_dir,"/calwrf") # Where calwrf data are stored (if Z disk is not present: mount 10.58.186.210:/wrf_data Z:)

# General parameters
gis_dir <- "F:/gis/"                              # The folder where we store general GIS data
bc_dir  <- file.path(gis_dir, "background") # The folder with background atmospheric concentrations for O3, NH3, H2O2

calmet_exe <- "C:/CALPUFF/CALMET_v6.5.0_L150223/calmet_v6.5.0.exe"
calmet_templates <- list(noobs="F:/templates/CALMET_template.INP", 
                         surfobs="F:/templates/CALMET_surfObs_template.inp")

calpuff_exe <- "C:/CALPUFF/CALPUFF_v7.2.1_L150618/calpuff_v7.2.1.exe"
# calpuff_template <- file.path("F:/templates/CALPUFF_7.0_template_Hg.INP")  # If emission data include Mercury (Hg)
calpuff_template <- file.path("F:/templates/CALPUFF_7.0_template.INP")  # If emission data do NOT include Mercury (Hg)

pu_exe <- "C:/CALPUFF/POSTUTIL_v7.0.0_L150207/postutil_v7.0.0.exe"

template_dir="F:/templates/"
pu_templates <- list (repartition = file.path(template_dir, "Mintia_postutilRepartition.inp"), 
                      deposition = file.path(template_dir, "Mintia_postutil_depo.inp"), 
                      total_pm = file.path(template_dir, "Mintia_postutil_PM10.inp"))

calpost_exe <- file.path("C:/CALPUFF/CALPOST_v7.1.0_L141010/calpost_v7.1.0.exe")
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
# browser()

calmet_result = readRDS(file.path(output_dir,"calmet_result.RDS" ))


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
  emissions_data$status = ifelse(emissions_data$existing,'operating', 'future')  

  # Selection of operating/future plants
  # emissions_data %<>% filter(status == "operating")

  # Produce unique ascii names with 8 characters
  emissions_data$Plants %>% gsub(' ', '', .) %>% substr(1,5) %>% stringi::stri_trans_general("Latin-ASCII") %>%  
    make.names %>% make.unique(sep='') %>% 
    paste0('_', substr(emissions_data$status,1,1)) -> emissions_data$emission_names
  if (emissions_data$emission_names %>% nchar %>% max > 8) stop("ERROR in plant-name length (too much plants with the same name?)")                        

  # Ensure numeric values of lat and long  
  emissions_data$Lat %<>% as.numeric()
  emissions_data$Long %<>% as.numeric()

  # Create polygons of grid boundaries  
  dom_pols = grids_to_domains(calmet_result$grids, target_crs)
  
  # Exclude sources outside domain  
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
  # emissions_data %<>% head(1)
  # emissions_data %<>% tail(1)
  # emissions_data <- rbind(emissions_data %>% head(1), emissions_data %>% tail(1))
}

# ============================== Receptors =====================================
# Get all receptors for each source point
nesting_factors = c(1,5,15) #c(1,2,5,15) # MESHDN parameter (in CALPUFF.INP) which defines the grid spacing 
                              # (DGRIDKM/MECHDN) of each disk, wrt the grid spacing of the outer 
                              # meteo grid (DGRIDKM). Higher factor: higher density of receptors.
if(!exists('receptors')) receptors = list()
queue = unique(emissions_data$emission_names)
for(run in queue) {
  emission_data_run <- emissions_data %>% filter(emission_names == run) %>% head(1)
  loc <- emission_data_run %>% to_spdf %>% spTransform(target_crs)
  # Get discrete receptors with 400x400 dim 
  get_recep(loc = loc, 
            run_name = calmet_result$run_name,
            nesting_factors=nesting_factors,
            files_met=out_files,
            target_crs=target_crs) -> receptors [[run]]
    print(run)
}

# Select receptors around sources
nesfact_range = c(125,25,5) #c(125,25,10) #c(150,75,25,10) # Radius, in km, of receptor grid disks (from outer to inner disk)
                                # around each source point.
sources <- emissions_data %>% to_spdf %>% spTransform(target_crs)
receptors %<>% select_receptors(sources=sources,
                                run_name = calmet_result$run_name,
                                nesting_factors=nesting_factors,
                                nesfact_range=nesfact_range,
                                files_met=out_files)

receptors[receptors$Xkm %% 30 < 15 & receptors$Ykm %% 30 < 15 & receptors$nesfact==1, 'include'] <- T

# Receptor check
print(paste('Adding background grid: ', calmet_result$run_name, sum(receptors$include), 'receptors'))
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
    emission_data_run <- emissions_data %>% filter(emission_names == run) %>% head(1)
    run_name <- paste(calmet_result$run_name, emission_data_run$emission_names,sep='_')
    NPT2 <- emission_data_run$N_sources
    print(paste0("CALPUFF run name: ", run_name, ", n_sources: ", NPT2))
    calpuff_result <- creapuff::runCalpuff(
      # emissions_data = emissions_data,     # For constant emissions 
      # source_names = source_names,         # Optional. If not set: read from emissions_data (if not present, set automatically)
      # FGD = "T",                           # Optional. If not set: read from emissions_data (if not present an error occurs)
      receptors=receptors,                   # Optional. If not set: full domain grid
      o3dat=o3dat,                           # Optional. If not set: no surface data
      bgconcs=bgconcs,                       # Optional. If not set: std values
      species_configuration = "so2_nox_pm",  # Two possible values: "so2_nox_pm" or "so2_nox_pm_hg"
      addparams = list(NPTDAT = 1,           # For arbitrary-varying emissions
                       PTDAT = emission_data_run$Path,
                       NPT2 = NPT2),
      run_name=run_name,
      output_dir = output_dir,
      params_allgrids = calmet_result$params,
      gis_dir = gis_dir,
      calpuff_exe = calpuff_exe,
      calpuff_template = calpuff_template
    )
  } 
  
  for(run in queue) {
    emission_data_run <- emissions_data %>% filter(emission_names == run) %>% head(1)
    run_name <- paste(calmet_result$run_name, emission_data_run$emission_names,sep='_')
    bat_file <- file.path(output_dir, paste0(run_name, '_1', '.bat'))
    shell.exec(normalizePath(bat_file))
  }
}

# calpuff_result = readRDS(file.path(output_dir,"calpuff_result*.RDS" ))

browser()


# POST PROCESSING ##############################################################

creapuff::runPostprocessing(
  output_dir=output_dir,
  sources=calpuff_result$sources,
  out_files_all=calpuff_result$out_files_all,
  pm10fraction=calpuff_result$pm10fraction,
  pu_exe=pu_exe,
  pu_templates=pu_templates,
  calpost_exe=calpost_exe,
  calpost_templates=calpost_templates,
  run_name=run_name,
  inpfiles_created=calpuff_result$inpfiles_created
)