# remotes::install_github("energyandcleanair/creapuff", dependencies=T, update=F)
# devtools::reload(pkgload::inst("creapuff"))
library(creapuff)

# Parameters --------------------------------------------------------------

# Project specific
expand_grids = F #'none'
input_xls <- file.path("Z:/projects/chile/ChileClusters.xlsx")
output_dir <- "Z:/projects/chile/calpuff_suite" # Where to write all generated files
wrf_dir <-"Z:/projects/chile/calwrf"            # Where calwrf data are stored

# General parameters
gis_dir <- "F:/gis/"                            # The folder where we store general GIS data

calmet_exe <- "C:/CALPUFF/CALMET_v6.5.0_L150223/calmet_v6.5.0.exe"
calmet_templates <- list(noobs="F:/templates/CALMET_template.INP", 
                         surfobs="F:/templates/CALMET_surfObs_template.inp")

calpuff_exe <- "C:/CALPUFF/CALPUFF_v7.2.1_L150618/calpuff_v7.2.1.exe"
calpuff_template <- file.path("F:/templates/CALPUFF_7.0_template_Hg.INP")

pu_exe <- "C:/CALPUFF/POSTUTIL_v7.0.0_L150207/postutil_v7.0.0.exe"
pu_templates <- file.path("F:/templates/",
                          c("Mintia_postutilRepartition.inp",
                            "Mintia_postutil_depo.inp",
                            "Mintia_postutil_PM10.inp"))

calpost_exe <- file.path("C:/CALPUFF/CALPOST_v7.1.0_L141010/calpost_v7.1.0.exe")
calpost_templates <- file.path("F:/templates/",
                               c("Mintia_AllOutput_calpost.inp",
                                 "Mintia_depo_calpost.inp"))


# CALMET -----------------------------------------------------------------

calmet_result <- creapuff::runCalmet(
  input_xls = input_xls,
  wrf_dir = wrf_dir,
  expand_grids = expand_grids,
  output_dir = output_dir,
  gis_dir = gis_dir,
  calmet_exe = calmet_exe,
  calmet_templates = calmet_templates,
  only_make_additional_files=F,  # F
  run_calmet = T  # T
)

# CALPUFF -----------------------------------------------------------------

calpuff_result <- creapuff::runCalpuff(
  run_name= calmet_result$run_name,
  start_date = calmet_result$start_dates[[1]],
  input_xls = input_xls,
  wrf_dir = wrf_dir,
  expand_grids = expand_grids,
  output_dir = output_dir,
  grids = calmet_result$grids,
  params_allgrids = calmet_result$params,
  gis_dir = gis_dir,
  calpuff_exe = calpuff_exe,
  calpuff_template = calpuff_template
)


# POST PROCESSING -----------------------------------------------------------------

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
