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
expand_ncells = -5  # Number of cells to expand in each direction (use negative values to crop)

input_xls <- file.path("Z:/projects/chile/chile_emissions_test.xlsx") # ChileClusters.xlsx
output_dir <- "Z:/projects/chile/calpuff_suite"  # Where to write all generated files
wrf_dir <-"Z:/projects/chile/calwrf"             # Where calwrf data are stored (if Z disk is not present: mount 10.58.186.210:/wrf_data Z:)

# General parameters
gis_dir <- "F:/gis/"                              # The folder where we store general GIS data
bc_dir  <- file.path(gis_dir, "background") # The folder with background atmospheric concentrations for O3, NH3, H2O2

calmet_exe <- "C:/CALPUFF/CALMET_v6.5.0_L150223/calmet_v6.5.0.exe"
calmet_templates <- list(noobs="F:/templates/CALMET_template.INP", 
                         surfobs="F:/templates/CALMET_surfObs_template.inp")

calpuff_exe <- "C:/CALPUFF/CALPUFF_v7.2.1_L150618/calpuff_v7.2.1.exe"
calpuff_template <- file.path("F:/templates/CALPUFF_7.0_template_Hg.INP")

pu_exe <- "C:/CALPUFF/POSTUTIL_v7.0.0_L150207/postutil_v7.0.0.exe"

template_dir="F:/templates/"
pu_templates <- list (repartition = file.path(template_dir, "Mintia_postutilRepartition.inp"), 
                      deposition = file.path(template_dir, "Mintia_postutil_depo.inp"), 
                      total_pm = file.path(template_dir, "Mintia_postutil_PM10.inp"))

calpost_exe <- file.path("C:/CALPUFF/CALPOST_v7.1.0_L141010/calpost_v7.1.0.exe")
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

# calmet_result = readRDS(file.path(output_dir,"calmet_result.RDS" ))


# INPUT DATA ################################################################

# ---------------------------- Emission data -----------------------------------
# Group the following lines, within a separate function ?
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

read_xlsx(input_xls, sheet='CALPUFF input') -> emissions_data 

# Define which sources should be included in which scenario. COD = commercial operation date  # LC, CHECK
emissions_data$COD %>% substr(.,nchar(.)-3,nchar(.)) %>% as.numeric () <   
  calmet_result$start_dates[[1]] %>% format("%Y") %>% as.numeric() ->  
  emissions_data$existing
emissions_data$status = ifelse(emissions_data$existing,'operating', 'future')  

# Selection of operating/future plants ?
# emissions_data %<>% filter(status == "operating")

# Produce unique ascii names with 8 characters  # LC, CHECK
emissions_data$Plants %>% gsub(' ', '', .) %>% substr(1,5) %>% stringi::stri_trans_general("Latin-ASCII") %>%  
  make.names %>% make.unique(sep='') %>%   
  paste0('_', substr(emissions_data$status,1,1)) -> emissions_data$emission_names
if (emissions_data$emission_names %>% nchar %>% max > 8) stop("ERROR in plant-name length (too much plants with the same name?)")                        

# Ensure numeric values of lat and long  
emissions_data$Lat %<>% as.numeric()
emissions_data$Long %<>% as.numeric()

# Create polygons of grid boundaries  # LC, CHECK
dom_pols = grids_to_domains(calmet_result$grids, target_crs)

# Exclude sources outside domain  # LC, CHECK
emissions_data %<>% to_spdf %>% crop(spTransform(dom_pols, crs(.))) %>% '@'('data')

# If not specified, define if/which emission sources have FGD  # LC, CHECK
if(!is.null(emissions_data$FDG)) emissions_data$FDG=T


# ---------------------------- Test with fewer data ----------------------------
# emissions_data %<>% head(1)


# ------------------------------ Receptors -------------------------------------
# Define receptors for all sites 
nesfact_range = c(30,10) # Radius, in km, of receptor grid disks (from outer to inner disk)
                         # around each source point.

nesting_factors = c(3,9) # MESHDN parameter (in CALPUFF.INP) which defines the grid spacing 
                         # (DGRIDKM/MECHDN) of each disk, wrt the grid spacing of the outer 
                         # meteo grid (DGRIDKM). Higher factors, higher densities of recep.

# Get all receptors for each source point
if(!exists('receptors')) receptors = list()
queue = unique(emissions_data$emission_names)
for(run in queue) {
  emission_data_run <- emissions_data %>% filter(emission_names == run) %>% head(1)
  # target_crs = get_utm_proj(emission_data_run$UTMZ, emission_data_run$UTMH)
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
sources <- emissions_data %>% to_spdf %>% spTransform(target_crs)
receptors %<>% select_receptors(sources=sources,
                                run_name = calmet_result$run_name,
                                nesting_factors=nesting_factors,
                                nesfact_range=nesfact_range,
                                files_met=out_files)


# ------------------------- Background concentrations --------------------------
# sources <- emissions_data %>% to_spdf %>% spTransform(target_crs)  
bgconcs <- get_bg_concs(sources, mod_dir=bc_dir)
o3dat <- NULL # Hourly ozone data file (NULL: no ozone monitoring stations)


# CALPUFF ######################################################################
calpuff_result <- creapuff::runCalpuff(
  emissions_data=emissions_data,             
  source_names=emissions_data$emission_names,  # Optional. If not set, read from emissions_data (if not present, set automatically)
  FGD=emissions_data$FGD,                      # Optional. If not set, read from emissions_data
  receptors=receptors,                         # Optional. If not set, full domain grid 
  o3dat = o3dat,                               # Optional. If not set, no surface data
  bgconcs=bgconcs,                             # Optional. If not set, std values
  # addparams=addparams,                       # Optional. If not set, std values
  run_name=calmet_result$run_name,
  output_dir = output_dir,
  params_allgrids = calmet_result$params,
  gis_dir = gis_dir,
  calpuff_exe = calpuff_exe,
  calpuff_template = calpuff_template
)


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