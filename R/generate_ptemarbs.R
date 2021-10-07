#' Generating PTEMARB files from sources end emissions.
#' 
#' The script will generate ONE PTEMARB FILE per cluster in indicated folder.
#'
#' @param folder 
#' @param scenario 
#' @param prefix 
#' @param sources 
#' @param emissions 
#' @param species 
#' @param utc_zone example: UTC+0800
#' @param utm_zone example: 52N
#' @param begin_date 
#' @param end_date 
#'
#' @return
#' @export
#'
#' @examples
generate_ptemarbs <- function(folder,
                              sources,
                              emissions,
                              utc_zone,
                              species=list(SO2=32, NO=30, NO2=46, PPM25=1),
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
  python_exec <- "python3" #reticulate::virtualenv_python("creapuff") 
  exec_folder <- file.path(system.file(package="creapuff"),"python")
  f_mainpy <- "generate_ptemarb.py"
  command <- paste("cd", exec_folder, "; ", python_exec, f_mainpy, f_config, sep = " ")
  response <- system(command, intern=T)
  
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
  
  
                                       

            