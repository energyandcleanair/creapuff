#' Post-processing
#'
#' @return
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

runPostprocessing <- function(
  calpuff_inp,
  run_name = names(calpuff_inp),
  cp_run_name = run_name,
  files_met=NULL,
  output_dir=unique(files_met$dir),
  pm10fraction,
  METRUN = 0, #set 1 to run CALPOST on all periods in file
  nper = NULL,
  pu_start_hour = NULL,
  cp_species = c('PM25', 'TPM10', 'TSP', 'SO2', 'NO2'),
  get_cp_period = get_cp_period,
  run_discrete_receptors=T,
  run_gridded_receptors=T,
  run_concentrations=T,
  run_deposition=T,
  run_timeseries = T,
  run_hourly = c('PM25', 'NO2', 'SO2'),
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
  
  #generate POSTUTIL and CALPOST .inp files
  pu.inp.out <- pu_templates %>% lapply(function(x) gsub("^[^_]*", "", x))
  cp.inp.out <- calpost_templates %>% lapply(function(x) gsub("^[^_]*", "", x))
  
  #read CALPUFF.INP
  puffInp <- readLines(calpuff_inp)
  
  params <- data.frame(name=NA,val=NA)
  inparams <- data.frame(cpuname=NA,name=NA)
  
  #set params read from CALPUFF.INP
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
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("UTLMET","METDAT1")
  
  params <- inparams
  params$val <- NA
  
  for(p in 1:nrow(inparams)) {
    puffInp %>% gsub(" ", "", .) %>% 
      get_param_val(params$cpuname[p], .) -> params$val[p]
  }
  
  if(is.null(output_dir)) output_dir = dirname(params$val[params$name=='MODDAT']) %>% normalizePath()
  conF <- params$val[params$name=='MODDAT']
  
  #calculate POSTUTIL time period if not set
  if(is.null(pu_start_hour)) {
    params$val[params$name == 'ISHR'] %<>% as.numeric %>% add(2)
  } else params$val[params$name == 'ISHR'] <- pu_start_hour
    
  
  
  if(is.null(nper)) {
    nper <- difftime(paste(params$val[match(c('ISYR', 'ISMO', 'ISDY', 'ISHR'), params$name)], collapse = ' ') %>% ymd_h,
                     paste(params$val[match(c('IEYR', 'IEMO', 'IEDY', 'IEHR'), params$name)], collapse = ' ') %>% ymd_h, units='hours') %>% 
      as.numeric() %>% abs %>% subtract(3)
  }
  
  params %<>% dplyr::select(-cpuname) %>% filter(!grepl("^IE", name))
  
  #set params determined by program
  params[nrow(params)+1,] <- c('UTLLST', gsub("\\.CON", "_POSTUTIL_REPART.LST", conF))
  params[nrow(params)+1,] <- c('UTLDAT', gsub("\\.CON", "_repart.CON", conF))
  params[nrow(params)+1,] <- c('NPER', nper)
  
  if(run_concentrations) {
    if(!is.null(files_met))
      params[params$name == 'UTLMET', 'val'] <- files_met %>% arrange(desc(GridD)) %>% use_series(path) %>% 
        head(1)
    
    #write repartitioning INP file
    write_input(pu_templates$repartition, 
                file.path(output_dir, paste0(run_name, pu.inp.out$repartition)),
                params)
    
    #write_input file to calculate total PM
    params[params$name == 'MODDAT', 'val'] <- params[params$name == 'UTLDAT', 'val']
    params[params$name == 'UTLDAT', 'val'] <- gsub("\\.CON", "_TotalPM.CON", conF)
    params[params$name == 'UTLLST', 'val'] %<>% gsub("REPART", "TotalPM", .)
    params[!(params$name %in% c('BCKNH3', 'UTLMET')), ] -> params
    
    write_input(pu_templates$total_pm, 
                file.path(output_dir, paste0(run_name, pu.inp.out$total_pm)),
                params)
  }
  
  if(run_deposition) {
    #write deposition INP file
    pu.depo.out = file.path(output_dir, paste0(run_name, pu.inp.out$deposition))
    params[params$name == 'UTLDAT', 'val'] <- gsub("\\.CON", "_Depo.FLX", conF)
    params[params$name == 'MODDAT', 'val'] <- gsub("\\.CON", ".WET", conF)
    params[params$name == 'UTLLST', 'val'] %<>% gsub("TotalPM", "Depo", .)
    
    params[nrow(params)+1,] <- c('MODDAT', gsub("\\.CON", ".DRY", conF))
    write_input(pu_templates$deposition, 
                pu.depo.out,
                params)
    
    #add the Hg fraction in PM
    pu.depo.out %>% readLines -> pu.depo
    pu.depo %>% gsub(' ', '', .) %>% grep("!CSPECCMP=Hg!", .) -> hgcmp_startline
    pu.depo %>% gsub(' ', '', .) %>% grep("!PM10=", .) %>% 
      subset(.>hgcmp_startline) %>% head(1) -> hgcmp_pm10line
    pu.depo[hgcmp_pm10line] = paste("!    PM10  =     ",pm10fraction," !")
    writeLines(pu.depo, pu.depo.out)
  }
  
  
  #make CALPOST INP files
  cp.period = get_cp_period(params)
  
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
  
  params[nrow(params)+1,] <- c('MODDAT', gsub("\\.CON", "_TotalPM.CON", conF))
  params[nrow(params)+1,] <- c('TSUNAM', cp_run_name)
  params[nrow(params)+1,] <- c('TUNAM', cp_run_name)
  params[nrow(params)+1,] <- c('TSPATH', ifelse(run_timeseries, output_dir,"not set"))
  params[nrow(params)+1,] <- c("LD", run_discrete_receptors %>% as.character %>% substr(1,1))
  params[nrow(params)+1,] <- c("LG", run_gridded_receptors %>% as.character %>% substr(1,1))
  
  if(run_concentrations) {
    #write_input file to get all concentration outputs
    
    species_params = list(LTIME=ifelse(run_timeseries, 'T', 'F'),
                          ASPEC   = cp_species %>% paste(collapse=', '),
                          ILAYER  = rep(1, length(cp_species)) %>% paste(collapse=', '),
                          IPRTU = rep(3, length(cp_species)) %>% paste(collapse=', '),
                          A = rep('0.0', length(cp_species)) %>% paste(collapse=', '),
                          B = rep('0.0', length(cp_species)) %>% paste(collapse=', '),
                          L1PD = rep('F', length(cp_species)) %>% paste(collapse=', '),
                          L3HR = rep('F', length(cp_species)) %>% paste(collapse=', '),
                          LRUNL = rep('T', length(cp_species)) %>% paste(collapse=', '),
                          L1HR = (cp_species %in% run_hourly) %>% as.character() %>% substr(1,1),
                          L24HR   = rep('T', length(cp_species)) %>% paste(collapse=', ')) %>% 
      unlist %>% data.frame(name = names(.), val= .)
    
    write_input(calpost_templates$concentration, 
                file.path(output_dir, paste0(run_name, cp.inp.out$concentration)),
                bind_rows(params, species_params),
                set.all=F)
  }
  
  if(run_deposition) {
    #write_input file to get all deposition outputs
    params[params$name == 'MODDAT', 'val'] <- gsub("\\.CON", "_Depo.FLX", conF)
    write_input(calpost_templates$deposition, 
                file.path(output_dir, paste0(run_name, cp.inp.out$deposition)),
                params,
                set.all=F) 
  }
  
  #make bat files to run POSTUTIL and CALPOST
  pu.bat <- file.path(output_dir, paste0("pu_", run_name, ".bat"))
  
  run_pu = NULL
  if(run_concentrations) run_pu = c('repartition', 'total_pm')
  if(run_deposition) run_pu %<>% c('deposition')
  
  writeLines(c(paste("cd", output_dir),
               paste0(pu_exe, " ", normalizePath(file.path(output_dir, 
                                                           paste0(run_name, pu.inp.out[run_pu])))),
               "pause"), 
             pu.bat)
  
  run_cp = NULL
  if(run_concentrations) run_cp = 'concentration'
  if(run_deposition) run_cp %<>% c('deposition')
  
  calpost.bat <- file.path(output_dir, paste0("calpost_", run_name, ".bat"))
  writeLines(c(paste("cd", output_dir),
               paste0(calpost_exe, " ", file.path(output_dir, cp.inp.out[run_cp])), "pause"), 
             calpost.bat)
  
  if(run_pu) system(sprintf('cmd /c "%s"',normalizePath(pu.bat)))
  if(run_calpost) system(sprintf('cmd /c "%s"',normalizePath(calpost.bat)))
}
