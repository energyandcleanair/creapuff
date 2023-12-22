#' Retrieve a CALPOST processing period corresponding to a full calendar year.
#'
#' @details
#' @returns 
#' 
#' @export
#'
#' @examples
get_cp_period = function(params) {
  runyr = as.numeric(params$val[params$name=='ISYR']) + ifelse(params$val[params$name=='ISMO']==12, 1, 0)
  list(start = paste0(runyr, '-01-01 0') %>% ymd_h,
       end = paste0(runyr+1, '-01-01 0') %>% ymd_h)
}

get_package_file = function(files, package="creapuff") {
  files %>% lapply(function(x) system.file("extdata", x, package=package))
}

#' Run post-processing with POSTUTIL and CALPOST
#' 
#' The CREA CALPUFF workflow uses POSTUTIL to (optionally) sum up results from multiple simulations, repartition nitrogen species, sum up different PM2.5 and PM10 species into total PM concentrations, and combine wet and dry deposition fluxes to total deposition. CALPOST is then used to output average concentrations, different mercury species, total fluxes, 1-hour and 24-hour maximum concentrations for each grid location, peak concentrations across the grid for each hour and day, as well as time series files with all concentration data for all receptors on hourly or 24-hour averaged basis.
#' 
#' @param calpuff_inp CALPUFF.INP file from which to read the basic parameters of the simulation, such as domain, time period and CALMET.DAT files to use.
#' @param run_name Names of the CALPUFF run(s) to process. If multiple names are provided, the concentrations and deposition fluxes are summed up. Note that only one calpuff_inp path should be provided also for multiple run_names - for summing up to be possible, the basic parameters of the runs must be identical.
#' @param run_name_out If run_name includes multiple runs to be summed up, this is the output run name. Otherwise defaults to run_name.
#' @param cp_run_name Run name to use for CALPOST outputs (maximum 8 characters due to CALPOST limitation). If not provided, the function attempts to derive this as make_srcnam(run_name_out).
#' @param files_met data.frame with information on the CALMET files used in CALPUFF simulations. Output from runCalmet.
#' @param output_dir If not specified, files_met$dir is used. Currently, specifying a different dir isn't fully implemented.
#' @param pm10fraction The content of mercury in PM10 used when calculating total mercury deposition, given in kgHg/tPM10.
#' @param METRUN CALPOST parameter. Set 1 to run CALPOST on all periods in file.
#' @param nper CALPOST parameter, number of periods to run. Calculated by default from the period start and end output by cp_period_function().
#' @param pu_start_hour Hour of the day when POSTUTIL execution should start. Specified by read_postutil_params_from_calpuff_inp() by default.
#' @param cp_species Species to output from CALPOST. Default: c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2').
#' @param cp_period_function Function to determine the period to use for CALPOST. Default is creapuff::get_cp_period()
#' @param run_discrete_receptors Logical. Should discrete receptors be processed? Default: TRUE.
#' @param run_gridded_receptors Logical. Should gridded receptors be processed? Default: TRUE.
#' @param run_concentrations Logical. Should concentrations be processed? Default: TRUE.
#' @param run_deposition Logical. Should deposition be processed? Default: TRUE.
#' @param run_timeseries = Logical. Should timeseries files be output (these can be quite large)? Default: TRUE.
#' @param run_hourly = Character vector. For which species should 1-hour average and maximum values be output? Default: c('PM25', 'NO2', 'SO2').
#' @param emissions_scaling A list whose names correspond to (some of) the run_names. Each list item is a list or data.frame whose names include any of c('so2', 'nox', 'pm', 'hg'), with values giving the scaling factors. It's also possible to pass one list or data frame whose names include the species to be scaled. In this case, the scaling factors will be applied to all run_names.
#' @param run_pu Logical. Should POSTUTIL be executed from the function? Default: FALSE. If not, the user needs to run the BAT file output by the function.
#' @param run_calpost Logical. Should CALPOST be executed from the function? Default: FALSE. If not, the user needs to run the BAT file output by the function.
#' @param pu_templates The POSTUTIL.INP templates to use. These templates specify all the input variables which are not set by the function, most of which are U.S. EPA defaults. A list with names sumruns, repartition, deposition, total_pm.
#' @param calpost_templates The CALPOST.INP templates to use. These templates specify all the input variables which are not set by the function, most of which are U.S. EPA defaults. A list with names concentration, deposition.
#' @param pu_exe POSTUTIL executable. Default: "C:/CALPUFF/POSTUTIL_v7.0.0_L150207/postutil_v7.0.0.exe",
#' @param calpost_exe CALPOST executable. Default: "C:/Calpuff/CALPOST_v7.1.0_L141010/calpost_v7.1.0.exe"
#' @details
#' @returns Nothing. The function writes the INP files and BAT files needed to run the post-processing and executes the BAT files if desired.
#' @export
#' @examples
runPostprocessing <- function(
  calpuff_inp,
  run_name = names(calpuff_inp),
  run_name_out = run_name, #if run_name includes multiple runs to be summed up, this is the output run name
  cp_run_name = make_srcnam(run_name_out),
  files_met=NULL,
  output_dir=unique(files_met$dir),
  pm10fraction,
  METRUN = 0,  # Set 1 to run CALPOST on all periods in file
  nper = NULL,
  pu_start_hour = NULL,
  cp_species = c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2'),
  pu_species = cp_species,
  cp_period_function = get_cp_period,
  run_discrete_receptors=T,
  run_gridded_receptors=T,
  run_concentrations=T,
  run_deposition=T,
  run_timeseries = T,
  run_hourly = c('PM25', 'NO2', 'SO2'),
  emissions_scaling=NULL,
  run_pu=F,
  run_calpost=F,
  pu_templates = get_package_file(list(sumruns="AfsinFut_postutil_sumruns.inp",
                                       repartition="Mintia_postutilRepartition.inp",
                                       deposition="Mintia_postutil_depo.inp",
                                       total_pm="Mintia_postutil_PM10.inp")),
  calpost_templates = get_package_file(list(concentration="Mintia_AllOutput_calpost.inp",
                                            deposition="Mintia_depo_calpost.inp")), 
  pu_exe = "C:/CALPUFF/POSTUTIL_v7.0.0_L150207/postutil_v7.0.0.exe",
  calpost_exe = "C:/Calpuff/CALPOST_v7.1.0_L141010/calpost_v7.1.0.exe"
  ){
  
  if(nchar(cp_run_name)>8) stop('cp_run_name is too long, the limit is 8 characters') 
  
  # Generate POSTUTIL and CALPOST .inp files
  pu.inp.out <- pu_templates %>% lapply(function(x) gsub("^[^_]*", "", x))
  cp.inp.out <- calpost_templates %>% lapply(function(x) gsub("^[^_]*", "", x))

  paramlist <- read_postutil_params_from_calpuff_inp(calpuff_inp, files_met=files_met, pu_start_hour=pu_start_hour, nper=nper)
  params <- paramlist$params
  
  if(is.null(output_dir)) output_dir = paramlist$calpuff_output_dir
  message(paste("output dir:", output_dir))
  
  if(is.null(nper)) nper <- paramlist$nper
  
  if(length(run_name)==1) { conF <- params$val[params$name=='MODDAT']
  } else { conF <- file.path(output_dir, paste0(run_name, '.CON')) }
  
  names(conF) <- run_name

  params %<>% dplyr::select(-cpuname) %>% filter(!grepl("^IE", name))
  
  # Set params determined by program
  params[nrow(params)+1,] <- c('UTLLST', file.path(output_dir, paste0(run_name_out, '_POSTUTIL_REPART.LST')))
  params[nrow(params)+1,] <- c('UTLDAT', file.path(output_dir, paste0(run_name_out, '_repart.CON')))
  
  params %>% filter(name!='ABTZ', name != 'MODDAT') -> pu_params
  
  nscaled=0
  if(!is.null(emissions_scaling)) {
    nscaled=length(emissions_scaling)
    if(!all(names(emissions_scaling) %in% run_name)) {
      if(!all(names(emissions_scaling) %in% c('so2', 'nox', 'pm', 'hg'))) stop('emissions_scaling names must match with run_names')
      nscaled=length(run_name)
      emissions_scaling <- list(rep(emissions_scaling, nscaled))
      names(emissions_scaling) <- run_name
    }
    
    names(emissions_scaling) %>% 
      lapply(function(run) {
        make_emissions_scaling_lines(conF[run_name==run], emissions_scaling[[run]])
      }) %>% unlist ->
      emissions_scaling_lines
    
    emissions_scaling = list('2d'=emissions_scaling_lines)
    message('---adding emissions scaling---')
    message(emissions_scaling_lines)
  }
  
  if(run_concentrations) {
    
    if(!is.null(files_met))
      pu_params[pu_params$name == 'UTLMET', 'val'] <- files_met %>% arrange(desc(GridD)) %>% use_series(METDAT) %>%
        head(1)
    
    repart_params = bind_rows(tibble(name='MODDAT', val=conF),
                              tibble(name='NFILES', val=as.character(length(run_name))),
                              tibble(name='NSCALED', val=as.character(nscaled)))
    
    # Write repartitioning INP file
    write_input(pu_templates$repartition, 
                file.path(output_dir, paste0(run_name_out, pu.inp.out$repartition)),
                bind_rows(pu_params, repart_params),
                subgroup=emissions_scaling)
    
    # Write_input file to calculate total PM
    pu_params %<>% bind_rows(tibble(name='MODDAT', val=pu_params$val[pu_params$name == 'UTLDAT'])) 
    pu_params[pu_params$name == 'UTLDAT', 'val'] %<>% gsub("repart", "TotalPM", .)
    pu_params[pu_params$name == 'UTLLST', 'val'] %<>% gsub("REPART", "TotalPM", .)
    pu_params[!(pu_params$name %in% c('BCKNH3', 'UTLMET')), ] -> pu_params
    
    #save file name for input to calpost
    totalpm.con <- pu_params[pu_params$name == 'UTLDAT', 'val']
    
    pu_params %>% bind_rows(tibble(name='ASPECO', val=pu_species)) -> totalPM_params
    totalPM_params %<>% rbind(tibble(name='NSPECOUT', val=length(pu_species)))
    
    write_input(pu_templates$total_pm, 
                file.path(output_dir, paste0(run_name_out, pu.inp.out$total_pm)),
                totalPM_params)
  }
  
  if(run_deposition) {
    # Write deposition INP file
    pu.depo.out = file.path(output_dir, paste0(run_name_out, pu.inp.out$deposition))
    pu_params[pu_params$name == 'UTLDAT', 'val'] %<>% gsub('_TotalPM.CON', "_Depo.FLX", .)
    pu_params[pu_params$name == 'UTLLST', 'val'] %<>% gsub("TotalPM", "Depo", .)
    
    pu_params %<>% filter(name != 'MODDAT', name != 'NFILES') %>% 
      bind_rows(tibble(name='MODDAT', val=c(gsub("\\.CON", ".WET", conF), gsub("\\.CON", ".DRY", conF))),
                tibble(name='NFILES', val=as.character(length(run_name)*2)),
                tibble(name='NSCALED', val=as.character(nscaled*2)))
    
    if(!is.null(emissions_scaling))
      emissions_scaling[['2d']] %<>% (function(x) c(c(gsub("\\.CON", ".WET", x), gsub("\\.CON", ".DRY", x))))
    
    #save file name for input to calpost
    depo.flx <- pu_params[pu_params$name == 'UTLDAT', 'val']
    
    write_input(pu_templates$deposition, 
                pu.depo.out,
                pu_params,
                subgroup=emissions_scaling)
    
    # Add the Hg fraction in PM
    pu.depo.out %>% readLines -> pu.depo
    pu.depo %>% gsub(' ', '', .) %>% grep("!CSPECCMP=Hg!", .) -> hgcmp_startline
    pu.depo %>% gsub(' ', '', .) %>% grep("!PM10=", .) %>% 
      subset(.>hgcmp_startline) %>% head(1) -> hgcmp_pm10line
    pu.depo[hgcmp_pm10line] = paste("!    PM10  =     ",pm10fraction," !")
    writeLines(pu.depo, pu.depo.out)
  }

  # Make CALPOST INP files
  cp.period = cp_period_function(params)

  params %<>% subset(name == 'ABTZ')
  params[nrow(params)+1,] <- c('METRUN', METRUN)
  
  if(METRUN == 0) {
    params[nrow(params)+1,] <- c('ISYR', year(cp.period$start))
    params[nrow(params)+1,] <- c('ISMO', month(cp.period$start))
    params[nrow(params)+1,] <- c('ISDY', day(cp.period$start))
    params[nrow(params)+1,] <- c('ISHR', hour(cp.period$start))
    
    params[nrow(params)+1,] <- c('IEYR', year(cp.period$end))
    params[nrow(params)+1,] <- c('IEMO', month(cp.period$end))
    params[nrow(params)+1,] <- c('IEDY', day(cp.period$end))
    params[nrow(params)+1,] <- c('IEHR', hour(cp.period$end))
  }
  
  output_dir_cp <- paste0(gsub('\\$|/$','',output_dir), '/')
  params[nrow(params)+1,] <- c('MODDAT', totalpm.con)
  params[nrow(params)+1,] <- c('PSTLST', file.path(output_dir, paste0(run_name_out, "_CALPOST.LST")))
  params[nrow(params)+1,] <- c('TSUNAM', cp_run_name)
  params[nrow(params)+1,] <- c('TUNAM', cp_run_name)
  params[nrow(params)+1,] <- c('PLPATH', output_dir_cp,"not set")
  params[nrow(params)+1,] <- c('TSPATH', ifelse(run_timeseries, output_dir_cp,"not set"))
  params[nrow(params)+1,] <- c("LD", run_discrete_receptors %>% as.character %>% substr(1,1))
  params[nrow(params)+1,] <- c("LG", run_gridded_receptors %>% as.character %>% substr(1,1))
  
  if(run_concentrations) {
    # write_input file to get all concentration outputs
  
    species_params = list(LTIME=ifelse(run_timeseries, 'T', 'F'),
                          NSPEC = length(cp_species),
                          ASPEC   = cp_species %>% paste(collapse=', '),
                          ILAYER  = rep(1, length(cp_species)) %>% paste(collapse=', '),
                          IPRTU = rep(3, length(cp_species)) %>% paste(collapse=', '),
                          A = rep('0.0', length(cp_species)) %>% paste(collapse=', '),
                          B = rep('0.0', length(cp_species)) %>% paste(collapse=', '),
                          L1PD = rep('F', length(cp_species)) %>% paste(collapse=', '),
                          L3HR = rep('F', length(cp_species)) %>% paste(collapse=', '),
                          LRUNL = rep('T', length(cp_species)) %>% paste(collapse=', '),
                          L1HR = (cp_species %in% run_hourly) %>% as.character() %>% substr(1,1) %>% paste(collapse=', '),
                          L24HR   = rep('T', length(cp_species)) %>% paste(collapse=', ')) %>% 
      unlist %>% data.frame(name = names(.), val= .)
    
    write_input(calpost_templates$concentration, 
                file.path(output_dir, paste0(run_name_out, cp.inp.out$concentration)),
                bind_rows(params, species_params),
                set.all=F)
  }
  
  if(run_deposition) {
    # Write_input file to get all deposition outputs
    params[params$name == 'MODDAT', 'val'] <- depo.flx
    params[params$name == 'PSTLST', 'val'] <- file.path(output_dir, paste0(run_name_out, "_Depo_CALPOST.LST"))
    write_input(calpost_templates$deposition, 
                file.path(output_dir, paste0(run_name_out, cp.inp.out$deposition)),
                params,
                set.all=F) 
  }
  
  # Make bat files to run POSTUTIL and CALPOST
  pu.bat <- file.path(output_dir, paste0("pu_", run_name_out, ".bat"))
  
  pu_runs = NULL
  if(run_concentrations) pu_runs = c('repartition', 'total_pm')
  if(run_deposition) pu_runs %<>% c('deposition')
  
  writeLines(c(paste("cd", output_dir),
               paste0(pu_exe, " ", normalizePath(file.path(output_dir, 
                                                           paste0(run_name_out, pu.inp.out[pu_runs])))),
               "pause"), 
             pu.bat)
  
  cp_runs = NULL
  if(run_concentrations) cp_runs = 'concentration'
  if(run_deposition) cp_runs %<>% c('deposition')
  
  calpost.bat <- file.path(output_dir, paste0("calpost_", run_name_out, ".bat"))
  writeLines(c(paste("cd", output_dir),
               paste0(calpost_exe, " ", normalizePath(file.path(output_dir, 
                                                  paste0(run_name_out, cp.inp.out[cp_runs])))), "pause"), 
             calpost.bat)
  
  old_wd <- getwd()
  setwd(output_dir)
  on.exit(setwd(old_wd))
  if(run_pu) system(sprintf('cmd /c "%s"', normalizePath(pu.bat)))
  if(run_calpost) system(sprintf('cmd /c "%s"',normalizePath(calpost.bat)))
}


make_emissions_scaling_lines <- function(con_file, emissions_scaling, pm_species=c('PM15', 'PM10', 'PPM25'), precision=6) {
  scaling_lines = paste("!  MODDAT =",con_file,"!")
  
  so2_species=c('SO2', 'SO4')
  nox_species=c('NO', 'NO2', 'NO3', 'HNO3')
  emissions_scaling %<>% lapply(signif, precision)
  
  if(!is.null(emissions_scaling$so2)) scaling_lines %<>% c(paste("!",so2_species,"=",emissions_scaling$so2,", 0.0 !"))
  if(!is.null(emissions_scaling$nox)) scaling_lines %<>% c(paste("!",nox_species,"=",emissions_scaling$nox,", 0.0 !"))
  if(!is.null(emissions_scaling$pm)) scaling_lines %<>% c(paste("!",pm_species,"=",emissions_scaling$pm,", 0.0 !"))
  if(!is.null(emissions_scaling$hg)) scaling_lines %<>% c(paste("!",c('Hg0', 'RGM'),"=",emissions_scaling$hg,", 0.0 !"))
  
  scaling_lines %>% c("! END !")
}


read_postutil_params_from_calpuff_inp <- function(calpuff_inp, files_met, pu_start_hour=NULL, nper=NULL) {
  # Read CALPUFF.INP
  puffInp <- readLines(calpuff_inp)
  
  params <- data.frame(name=NA,val=NA)
  inparams <- data.frame(cpuname=NA,name=NA)
  
  # Set params read from CALPUFF.INP
  inparams[1,c('name', 'cpuname')] <- c("ISYR","IBYR")
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("ISMO","IBMO")
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("ISDY","IBDY")
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("ISHR","IBHR")
  
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("IEYR","IEYR")
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("IEMO","IEMO")
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("IEDY","IEDY")
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("IEHR","IEHR")
  
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("ABTZ","ABTZ")
  
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("BCKNH3","BCKNH3")
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("MODDAT","CONDAT")
  if(nrow(files_met)==1) { inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("UTLMET","METDAT") 
  } else inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("UTLMET","METDAT1")
  
  params <- inparams
  params$val <- NA
  for(p in 1:nrow(inparams)) {
    puffInp %>% gsub(" ", "", .) %>% gsub("//", "/", .) %>% gsub("\\", "/", ., fixed=T) %>% 
      get_param_val(params$cpuname[p], .) -> params$val[p]
  }
  
  # Calculate POSTUTIL time period if not set
  if(is.null(pu_start_hour)) {
    params$val[params$name == 'ISHR'] %<>% as.numeric %>% add(2)
  } else params$val[params$name == 'ISHR'] <- pu_start_hour
  
  calpuff_output_dir = dirname(params$val[params$name=='MODDAT']) %>% normalizePath()
  
  if(is.null(nper)) {
    start_time=paste(params$val[match(c('ISYR', 'ISMO', 'ISDY', 'ISHR'), params$name)], collapse = ' ') %>% ymd_h
    #set end hour to zero - CALMET/PUFF require end hours during the night
    end_time=paste(params$val[match(c('IEYR', 'IEMO', 'IEDY', 'ISHR'), params$name)], collapse = ' ') %>% ymd_h %>% 'hour<-'(0)
    
    nper = difftime(start_time, end_time, units='hours') %>% 
      as.numeric() %>% abs %>% subtract(3)
  }
  
  params %<>% bind_rows(tibble(name='NPER', val=as.character(nper)))
  
  return(list(params=params,
              calpuff_output_dir=calpuff_output_dir))
}
