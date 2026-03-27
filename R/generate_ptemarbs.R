#' Generate PTEMARB files from sources and emissions
#'
#' Generates one PTEMARB.DAT file per cluster in the indicated folder.
#' These files are used as time-varying point source inputs for CALPUFF.
#'
#' The function proceeds in three steps:
#' \enumerate{
#'   \item Writes \code{sources.csv} and \code{emissions.csv} into \code{folder}.
#'   \item Generates a \code{config.json} file in \code{folder} (via
#'     \code{\link{ptemarb.create_config_file}}) that points to these CSVs
#'     and captures all run parameters (species, dates, UTC zone, etc.).
#'   \item Calls the bundled Python script
#'     \code{inst/python/generate_ptemarb.py} via \code{reticulate},
#'     passing \code{config.json} as argument. This script reads the config
#'     and produces one \code{ptemarb_<cluster_id>.DAT} file per cluster.
#' }
#'
#' @param folder Character. Output directory for generated PTEMARB files.
#' @param sources A data.frame with columns: \code{source_id}, \code{cluster_id},
#'   \code{stack_height_m}, \code{stack_diameter_m}, \code{easting_m},
#'   \code{northing_m}, \code{utm_zone}, \code{temp_k}.
#'   See template at \code{inst/templates/generate_ptemarbs/sources.csv}.
#' @param emissions A data.frame with columns: \code{source_id}, \code{date},
#'   \code{unit} (\code{"kg/h"} or \code{"g/s"}), one column per species
#'   (matching names in \code{species}), and \code{vexit_m_s}.
#'   Hourly resolution. See template at
#'   \code{inst/templates/generate_ptemarbs/emissions.csv}.
#' @param utc_zone Character. UTC offset string, e.g. \code{"UTC+0800"}.
#' @param species Named list mapping species names to molecular weights, e.g.
#'   \code{list(SO2=64, NO=30, NO2=46, PPM25=1)}. Names must match columns
#'   in \code{emissions}.
#' @param scenario Character. Scenario label written into the config.
#' @param prefix Character. Filename prefix for generated PTEMARB files.
#' @param begin_date,end_date Date or POSIXct. Period to generate. Defaults
#'   to the range of \code{emissions$date}.
#'
#' @return Called for side effects (files written to \code{folder}).
#' @export
#'
#' @examples
#' \dontrun{
#' # --- Load template data shipped with the package ---
#' template_dir <- system.file("templates", "generate_ptemarbs", package = "creapuff")
#' sources   <- readr::read_csv(file.path(template_dir, "sources.csv"))
#' emissions <- readr::read_csv(file.path(template_dir, "emissions.csv"))
#'
#' # --- Generate PTEMARB files ---
#' generate_ptemarbs(
#'   folder    = file.path(tempdir(), "ptemarb_output"),
#'   sources   = sources,
#'   emissions = emissions,
#'   utc_zone  = "UTC+0900",
#'   species   = list(SO2 = 64, NO = 30, NO2 = 46, PPM25 = 1)
#' )
#' }
generate_ptemarbs <- function(folder,
                              sources,
                              emissions,
                              utc_zone,
                              species=list(SO2=64, NO=30, NO2=46, PPM25=1),
                              scenario="scenario",
                              prefix="ptemarb_", 
                              begin_date=min(emissions$date),
                              end_date=max(emissions$date)){
  
  dir.create(folder, showWarnings = F)
  
  message("Checking sources and emissions data")
  ptemarb.check_input(sources, emissions, species)
  
  message("Preparing directory: ", folder)
  f_sources <- file.path(folder,"sources.csv")
  write_csv(sources, f_sources)
  f_emissions <- file.path(folder,"emissions.csv")
  emissions$date<-strftime(emissions$date,"%Y-%m-%d %H:%M")
  write_csv(emissions, f_emissions)
  
  message("Generating config file")
  f_config = normalizePath(file.path(folder, "config.json"))
  ptemarb.create_config_file(
    folder=folder,
    scenario=scenario,
    f_config = f_config,
    f_emissions = f_emissions,
    f_sources = f_sources,
    begin_date=begin_date,
    end_date=end_date,
    species=species,
    utc_zone=utc_zone
  )
  
  message("Running python script")
  python_exec <- reticulate::conda_python("creapuff")
  exec_folder <- normalizePath(
    file.path(system.file(package = "creapuff"), "python"),
    mustWork = TRUE
  )
  f_script <- file.path(exec_folder, "generate_ptemarb.py")
  # system2 + script path: no shell "cd" (breaks on Windows); Python adds the
  # script directory to sys.path so local writers/parsers imports still work.
  response <- system2(
    python_exec,
    args = c(f_script, f_config),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_status <- attr(response, "status")
  if (!is.null(exit_status) && exit_status != 0L) {
    msg <- if (length(response)) paste(response, collapse = "\n") else ""
    stop(
      "Python PTEMARB generator failed (exit ", exit_status, ").",
      if (nzchar(msg)) paste0("\n", msg) else "",
      call. = FALSE
    )
  }
  
  message("Done!")
}


ptemarb.check_input <- function(sources, emissions, species){

  # These are the column names expected by our python PTEMARB generator
  col_req_sources <- c("cluster_id", "source_id", "stack_height_m", "stack_diameter_m", "temp_k",
                       "easting_m", "northing_m", "utm_zone")
  if(!all(col_req_sources %in% names(sources))){
    stop("Missing colnames in sources: ", paste(setdiff(col_req_sources, names(sources)), collapse=", "))
  }

  col_req_emissions <- c("source_id", "date", "unit", names(species), "vexit_m_s")
  if(!all(col_req_emissions %in% names(emissions))){
    stop("Missing colnames in emissions: ", paste(setdiff(col_req_emissions, names(emissions)), collapse=", "))
  }
  
  if(!all(unique(emissions$unit) %in% c("kg/h","g/s"))){
    stop("Only kg/h and g/s supported for now. Please convert values.")
  }
}


#' Generate a config file for our python PTEMARB generator
#'
#' @param f 
#' @param file_emission 
#' @param begin_date 
#' @param end_date 
#' @param scenario 
#' @param utc_zone 
#' @param species 
#' @param record_duration_second 
#' @param default_stack_height_m 
#' @param default_stack_diameter_m 
#' @param prefix 
#' @param dataset 
#' @param dataver 
#'
#' @return
#' @export
#'
#' @examples
ptemarb.create_config_file <- function(
  f_config,
  f_sources,
  f_emissions,
  folder,
  begin_date,
  end_date,
  scenario,
  utc_zone,
  species,
  record_duration_second=3600,
  default_stack_height_m=85,
  default_stack_diameter_m=5,
  prefix="ptemarb_",
  dataset="PTEMARB.DAT",
  dataver="2.1")
{
  
  l <- list(
    folder=normalizePath(folder),
    begin_date=strftime(begin_date,"%Y-%m-%d"),
    end_date=strftime(end_date,"%Y-%m-%d"),
    f_sources=normalizePath(f_sources),
    f_emissions=normalizePath(f_emissions),
    species=jsonlite::toJSON(tibble(NAME=names(species), WEIGHT=unlist(species))),
    prefix=prefix,
    scenario=scenario,
    utc_zone=utc_zone,
    dataset=dataset,
    dataver=dataver,
    record_duration_second=record_duration_second,
    default_stack_height_m=default_stack_height_m,
    default_stack_diameter_m=default_stack_diameter_m
  )
  
  
  template <- "[
    {
      'OUTPUT_FOLDER': '{{folder}}',
      'SOURCES_FILE': '{{f_sources}}',
      'EMISSIONS_FILE': '{{f_emissions}}',
      'BEGIN_DATE': '{{begin_date}}',
      'END_DATE': '{{end_date}}',
      'PREFIX': '{{prefix}}',
      'DATASET': '{{dataset}}',
      'DATAVER': '{{dataver}}',
      'SCENARIO': '{{scenario}}',
      'UTCZONE': '{{utc_zone}}',
      'SPECIES': {{species}},
      'RECORD_DURATION_SEC': {{record_duration_second}},
      'DEFAULT_STACK_HEIGHT': {{default_stack_height_m}},
      'DEFAULT_STACK_DIAMETER': {{default_stack_diameter_m}}
    }
  ]"
  

  config <- with(l, glue::glue(template, .open="{{", .close="}}"))
  config <- gsub("'","\"",config)
  write_file(config, f_config)
  
}
  
  
                                       

            