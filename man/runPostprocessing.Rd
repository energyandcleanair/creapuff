% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03_postprocessing.R
\name{runPostprocessing}
\alias{runPostprocessing}
\title{Run post-processing with POSTUTIL and CALPOST}
\usage{
runPostprocessing(
  calpuff_inp,
  run_name = names(calpuff_inp),
  run_name_out = run_name,
  cp_run_name = make_srcnam(run_name_out),
  files_met = NULL,
  output_dir = unique(files_met$dir),
  pm10fraction,
  METRUN = 0,
  nper = NULL,
  pu_start_hour = NULL,
  cp_species = c("PM25", "TPM10", "TSP", "SO2", "NO2"),
  cp_period_function = get_cp_period,
  run_discrete_receptors = T,
  run_gridded_receptors = T,
  run_concentrations = T,
  run_deposition = T,
  run_timeseries = T,
  run_hourly = c("PM25", "NO2", "SO2"),
  emissions_scaling = NULL,
  run_pu = F,
  run_calpost = F,
  pu_templates = get_package_file(list(sumruns = "AfsinFut_postutil_sumruns.inp",
    repartition = "Mintia_postutilRepartition.inp", deposition =
    "Mintia_postutil_depo.inp", total_pm = "Mintia_postutil_PM10.inp")),
  calpost_templates = get_package_file(list(concentration =
    "Mintia_AllOutput_calpost.inp", deposition = "Mintia_depo_calpost.inp")),
  pu_exe = "C:/CALPUFF/POSTUTIL_v7.0.0_L150207/postutil_v7.0.0.exe",
  calpost_exe = "C:/Calpuff/CALPOST_v7.1.0_L141010/calpost_v7.1.0.exe"
)
}
\arguments{
\item{calpuff_inp}{CALPUFF.INP file from which to read the basic parameters of the simulation, such as domain, time period and CALMET.DAT files to use.}

\item{run_name}{Names of the CALPUFF run(s) to process. If multiple names are provided, the concentrations and deposition fluxes are summed up. Note that only one calpuff_inp path should be provided also for multiple run_names - for summing up to be possible, the basic parameters of the runs must be identical.}

\item{run_name_out}{If run_name includes multiple runs to be summed up, this is the output run name. Otherwise defaults to run_name.}

\item{cp_run_name}{Run name to use for CALPOST outputs (maximum 8 characters due to CALPOST limitation). If not provided, the function attempts to derive this as make_srcnam(run_name_out).}

\item{files_met}{data.frame with information on the CALMET files used in CALPUFF simulations. Output from runCalmet.}

\item{output_dir}{If not specified, files_met$dir is used. Currently, specifying a different dir isn't fully implemented.}

\item{pm10fraction}{The content of mercury in PM10 used when calculating total mercury deposition, given in kgHg/tPM10.}

\item{METRUN}{CALPOST parameter. Set 1 to run CALPOST on all periods in file.}

\item{nper}{CALPOST parameter, number of periods to run. Calculated by default from the period start and end output by cp_period_function().}

\item{pu_start_hour}{Hour of the day when POSTUTIL execution should start. Specified by read_postutil_params_from_calpuff_inp() by default.}

\item{cp_species}{Species to output from CALPOST. Default: c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2').}

\item{cp_period_function}{Function to determine the period to use for CALPOST. Default is creapuff::get_cp_period()}

\item{run_discrete_receptors}{Logical. Should discrete receptors be processed? Default: TRUE.}

\item{run_gridded_receptors}{Logical. Should gridded receptors be processed? Default: TRUE.}

\item{run_concentrations}{Logical. Should concentrations be processed? Default: TRUE.}

\item{run_deposition}{Logical. Should deposition be processed? Default: TRUE.}

\item{run_timeseries}{= Logical. Should timeseries files be output (these can be quite large)? Default: TRUE.}

\item{run_hourly}{= Character vector. For which species should 1-hour average and maximum values be output? Default: c('PM25', 'NO2', 'SO2').}

\item{emissions_scaling}{A list whose names correspond to (some of) the run_names. Each list item is a list or data.frame whose names include any of c('so2', 'nox', 'pm', 'hg'), with values giving the scaling factors. It's also possible to pass one list or data frame whose names include the species to be scaled. In this case, the scaling factors will be applied to all run_names.}

\item{run_pu}{Logical. Should POSTUTIL be executed from the function? Default: FALSE. If not, the user needs to run the BAT file output by the function.}

\item{run_calpost}{Logical. Should CALPOST be executed from the function? Default: FALSE. If not, the user needs to run the BAT file output by the function.}

\item{pu_templates}{The POSTUTIL.INP templates to use. These templates specify all the input variables which are not set by the function, most of which are U.S. EPA defaults. A list with names sumruns, repartition, deposition, total_pm.}

\item{calpost_templates}{The CALPOST.INP templates to use. These templates specify all the input variables which are not set by the function, most of which are U.S. EPA defaults. A list with names concentration, deposition.}

\item{pu_exe}{POSTUTIL executable. Default: "C:/CALPUFF/POSTUTIL_v7.0.0_L150207/postutil_v7.0.0.exe",}

\item{calpost_exe}{CALPOST executable. Default: "C:/Calpuff/CALPOST_v7.1.0_L141010/calpost_v7.1.0.exe"}
}
\value{
Nothing. The function writes the INP files and BAT files needed to run the post-processing and executes the BAT files if desired.
}
\description{
The CREA CALPUFF workflow uses POSTUTIL to (optionally) sum up results from multiple simulations, repartition nitrogen species, sum up different PM2.5 and PM10 species into total PM concentrations, and combine wet and dry deposition fluxes to total deposition. CALPOST is then used to output average concentrations, different mercury species, total fluxes, 1-hour and 24-hour maximum concentrations for each grid location, peak concentrations across the grid for each hour and day, as well as time series files with all concentration data for all receptors on hourly or 24-hour averaged basis.
}
