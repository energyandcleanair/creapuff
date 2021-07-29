# remotes::install_github("energyandcleanair/creapuff", dependencies=T, update=F)
# devtools::reload(pkgload::inst("creapuff"))
library(creapuff)
# library("readxl")

# Parameters ###################################################################

# Project specific
expand_grids = '*' # All grids are expanded 
expand_ncells = -5 # Number of cells to expand in each direction (use negative values to crop)

# input_xls <- file.path("F:/projects/chile_test/chile_emissions_baseline_test.xlsx") # Where constant-emission data are specified 
emissions_dir <-"F:/projects/chile_test/emissions/2019_corrected" # Where arbitrary-varying emission files are stored
output_dir <- "F:/projects/chile_test/calpuff_suite" # Where to write all generated files
wrf_dir <-"F:/projects/chile_test/calwrf" # Where calwrf data are stored

# General parameters
gis_dir <- "F:/gis/" # Where general GIS data we stored

calmet_exe <- "C:/CALPUFF/CALMET_v6.5.0_L150223/calmet_v6.5.0.exe"
calmet_templates <- list(noobs="F:/templates/CALMET_template.INP", 
                         surfobs="F:/templates/CALMET_surfObs_template.inp")

calpuff_exe <- "C:/CALPUFF/CALPUFF_v7.2.1_L150618/calpuff_v7.2.1.exe"
# calpuff_template <- file.path("F:/templates/CALPUFF_7.0_template_Hg.INP")
calpuff_template <- file.path("F:/templates/CALPUFF_7.0_template.INP")

pu_exe <- "C:/CALPUFF/POSTUTIL_v7.0.0_L150207/postutil_v7.0.0.exe"

template_dir="F:/templates/"
pu_templates <- list ( repartition = file.path(template_dir, "Mintia_postutilRepartition.inp"), 
                       deposition = file.path(template_dir, "Mintia_postutil_depo.inp"), 
                       total_pm = file.path(template_dir, "Mintia_postutil_PM10.inp"))

calpost_exe <- file.path("C:/CALPUFF/CALPOST_v7.1.0_L141010/calpost_v7.1.0.exe")
calpost_templates <- list( concentration = file.path(template_dir, "Mintia_AllOutput_calpost.inp"), 
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

calmet_result = readRDS(file.path(output_dir,"calmet_result.RDS" ))


# INPUT DATA ################################################################

#read_xlsx(input_xls, sheet='CALPUFF input') -> emissions_data 

# Find ptemarb.DAT files
file.path(emissions_dir) %>% list.files(pattern='\\.DAT$', recursive = F, full.names=T) -> emissions_file

# CALPUFF ######################################################################

queue = unique(emissions_file)
for(input_emissions_file in queue) {
  
  plant_name <- input_emissions_file %>% gsub('.*/ptemarb_|\\.DAT', '', .) %>% # substr(1,5) %>% 
    stringi::stri_trans_general("Latin-ASCII") %>% make.names()
  NPT2 <- input_emissions_file %>% scan(nmax = 1, sep = "", skip = 9, nlines = 1) %>% as.numeric
  run_name <- paste(calmet_result$run_name, plant_name,sep='_')
  
  print(paste0("Run name: ", run_name, ", n sources: ", NPT2))

    calpuff_result <- creapuff::runCalpuff(
    # emissions_data=emissions_data, # For constant emissions 
    # source_names=source_names,     # Optional. If not set, read from emissions_data (if not present, set automatically)
    # FGD="F",                       # Optional. If not set, read from emissions_data
    # receptors=receptors,           # Optional. If not set, full domain grid
    # o3dat=o3dat,                   # Optional. If not set, no surface data
    # bgconcs=bgconcs,               # Optional. If not set, std values
    species_configuration = "so2_nox_pm",  # "so2_nox_pm" or "so2_nox_pm_hg"
    addparams = list(NPTDAT = 1,     # For arbitrary-varying emissions
                     PTDAT = input_emissions_file,
                     NPT2 = NPT2),
    run_name=run_name,
    output_dir = output_dir,
    params_allgrids = calmet_result$params,
    gis_dir = gis_dir,
    calpuff_exe = calpuff_exe,
    calpuff_template = calpuff_template
  )
}

# calpuff_result = readRDS(file.path(output_dir,"calpuff_result.RDS" ))


# POST PROCESSING ##############################################################

creapuff::runPostprocessing(
  calpuff_inp=calpuff_result$inpfiles_created,
  run_name=calmet_result$run_name,
  pm10fraction=calpuff_result$pm10fraction,
  output_dir=output_dir,
  files_met=calpuff_result$out_files,
  pu_exe=pu_exe,
  pu_templates=pu_templates,
  calpost_exe=calpost_exe,
  calpost_templates=calpost_templates,
  run_pu=T,
  run_calpost=T
)
