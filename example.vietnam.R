remotes::install_github("energyandcleanair/creapuff", dependencies=T, update=F)
library(creapuff)




expand_grids = 'FullViet'
input_xls <- file.path("~/Vietnam/VN_PDP8_HIA Data.xlsx")

wrf_dir <-"F:/TAPM/VietnamWRF"
gis_dir <- "F:/gis/"
output_dir <- "~/Vietnam/" # Where to write all generated files

calmet_exe <- "C:/CALPUFF/CALMET_v6.5.0_L150223/calmet_v6.5.0.exe"
calmet_templates <- list(noobs="F:/templates/CALMET_template.INP", 
                         surfobs="F:/templatesCALMET_surfObs_template.inp")

calpuff_exe <- "C:/CALPUFF/CALPUFF_v7.2.1_L150618/calpuff_v7.2.1.exe"
calpuff_template <- file.path("F:/CALPUFF_7.0_template_Hg.INP")

pu_exe <- "C:\\CALPUFF\\POSTUTIL_v7.0.0_L150207\\postutil_v7.0.0.exe"
pu_templates <- file.path("F:/templates/",
                          c("Mintia_postutilRepartition.inp",
                            "Mintia_postutil_depo.inp",
                            "Mintia_postutil_PM10.inp"))

calpost_exe <- "C:\CALPUFF\CALPOST_v7.1.0_L141010/calpost_v7.1.0.exe"
calpost_templates <- file.path("F:/templates/",
                               c("Mintia_AllOutput_calpost.inp",
                                 "Mintia_depo_calpost.inp"))
# CALMET -----------------------------------------------------------------

calmet.result <- creapuff::calmet.generate_input(
  input_xls = input_xls,
  wrf_dir = wrf_dir,
  expand_grids = expand_grids,
  output_dir = output_dir,
  gis_dir = gis_dir,
  calmet_exe = calmet_exe,
  calmet_templates = calmet_templates,
  run_calmet = F
)


# CALPUFF -----------------------------------------------------------------

calpuff.result <- creapuff::calpuff.generate_input(
  run_name= calmet.result$run_name,
  start_date = calmet.result$start_dates[[1]],
  input_xls = input_xls,
  wrf_dir = wrf_dir,
  expand_grids = expand_grids,
  output_dir = output_dir,
  grids = calmet.result$grids,
  params_allgrids = calmet.result$params,
  gis_dir = gis_dir,
  calpuff_exe = calpuff_exe,
  calpuff_template = calpuff_template
)

# POST PROCESSING -----------------------------------------------------------------

creapuff::postprocessing(
  pu_exe=pu_exe,
  pu_templates=pu_templates,
  calpost_exe=calpost_exe,
  calpost_templates=calpost_templates,
  run_name=run_name,
  inpfiles_created=calpuff.result$inpfiles_created
)
