#' Post-processing
#'
#' @return
#' @export
#'
#' @examples
postprocessing <- function(
  output_dir,
  sources,
  outfiles_all,
  pm10fraction,
  pu_exe,
  pu_templates,
  calpost_exe,
  calpost_templates,
  run_name,
  inpfiles_created,
  run_calpost=T,
  run_pu=T
  ){
  
  output_dir %<>% normalizePath()
  
  #generate POSTUTIL and CALPOST .inp files
  pu.inp.out <- gsub("^[^_]*", "", pu_templates)
  
  make.time.series = T
  #nper = 8760
  #start_hour <- 1
  
  cp.inp.out <- gsub("^[^_]*", "", calpost_templates)
  
  get_cp_period = function(params) {
    runyr = as.numeric(params$val[params$name=='ISYR']) + ifelse(params$val[params$name=='ISMO']==12, 1, 0)
    list(start = paste0(runyr, '-01-01 3') %>% ymd_h,
         end = paste0(runyr+1, '-01-01 22') %>% ymd_h)
  }
  
  METRUN = 1 #set 1 to run CALPOST on all periods in file
  discrete_receptors=T
  
  #generate POSTUTIL and CALPOST .inp files
  runNames <- names(inpfiles_created)
  
  for(s in runNames) {
    runName = s
    cp.runName <- runName
    
    metrun = sources$runName[sources$source.name==s]
    metFiles = outfiles_all %>% filter(runName == metrun)
    runDir = metFiles$dir[1]
    
    # setwd(pu.dir)
    
    params <- data.frame(name=NA,val=NA)
    inparams <- data.frame(cpuname=NA,name=NA)
    
    #read CALPUFF.INP
    puffrun = s
    puffInp <- file.path(output_dir, paste0(puffrun,"_CALPUFF_7.0.inp")) %>% readLines()
    
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
        getParamVal(params$cpuname[p], .) -> params$val[p]
    }
    
    params$val[params$name == 'ISHR'] %<>% as.numeric %>% add(2)
    
    nper <- difftime(paste(params$val[match(c('ISYR', 'ISMO', 'ISDY', 'ISHR'), params$name)], collapse = ' ') %>% ymd_h,
                     paste(params$val[match(c('IEYR', 'IEMO', 'IEDY', 'IEHR'), params$name)], collapse = ' ') %>% ymd_h, units='hours') %>% 
      as.numeric() %>% abs %>% subtract(3)
    
    params %>% dplyr::select(-cpuname) %>% filter(!grepl("^IE", name)) -> params
    conF <- file.path(output_dir, paste0(runName, '.CON'))
    
    #set params determined by program
    params[nrow(params)+1,] <- c('UTLLST', gsub("\\.CON", "_POSTUTIL_REPART.LST", conF))
    params[nrow(params)+1,] <- c('UTLDAT', gsub("\\.CON", "_repart.CON", conF))
    params[nrow(params)+1,] <- c('NPER', nper)
    #write repartitioning INP file
    write.inp(pu_templates[1], 
              file.path(output_dir, paste0(runName, pu.inp.out[1])),
              params)
    
    #write INP file to calculate total PM
    params[params$name == 'MODDAT', 'val'] <- params[params$name == 'UTLDAT', 'val']
    params[params$name == 'UTLDAT', 'val'] <- gsub("\\.CON", "_TotalPM.CON", conF)
    params[params$name == 'UTLLST', 'val'] %<>% gsub("REPART", "TotalPM", .)
    params[!(params$name %in% c('BCKNH3', 'UTLMET')), ] -> params
    
    write.inp(pu_templates[3], 
              file.path(output_dir, paste0(runName, pu.inp.out[3])),
              params)
    
    #write deposition INP file
    pu.depo.out = file.path(output_dir, paste0(runName, pu.inp.out[2]))
    params[params$name == 'UTLDAT', 'val'] <- gsub("\\.CON", "_Depo.FLX", conF)
    params[params$name == 'MODDAT', 'val'] <- gsub("\\.CON", ".WET", conF)
    params[params$name == 'UTLLST', 'val'] %<>% gsub("TotalPM", "Depo", .)
    
    params[nrow(params)+1,] <- c('MODDAT', gsub("\\.CON", ".DRY", conF))
    write.inp(pu_templates[2], 
              pu.depo.out,
              params)
    
    #add the Hg fraction in PM
    pu.depo.out %>% readLines -> pu.depo
    pu.depo %>% gsub(' ', '', .) %>% grep("!CSPECCMP=Hg!", .) -> hgcmp_startline
    pu.depo %>% gsub(' ', '', .) %>% grep("!PM10=", .) %>% 
      subset(.>hgcmp_startline) %>% head(1) -> hgcmp_pm10line
    pu.depo[hgcmp_pm10line] = paste("!    PM10  =     ",pm10fraction[[s]]," !")
    writeLines(pu.depo, pu.depo.out)
    
    #make CALPOST INP files
    # setwd(cp.dir)
    
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
    params[nrow(params)+1,] <- c('TSUNAM', cp.runName)
    params[nrow(params)+1,] <- c('TUNAM', cp.runName)
    params[nrow(params)+1,] <- c('LTIME', ifelse(make.time.series, 'T', 'F'))
    params[nrow(params)+1,] <- c('TSPATH', ifelse(make.time.series, gsub("[^/]*$","", conF),"not set"))
    params[nrow(params)+1,] <- c("L1HR", "T, F, T, T")
    params[nrow(params)+1,] <- c("L24HR", "T, T, T, T")
    params[nrow(params)+1,] <- c("LD", discrete_receptors %>% as.character %>% substr(1,1))
    
    #write INP file to get all concentration outputs
    write.inp(calpost_templates[1], 
              file.path(output_dir, paste0(runName, cp.inp.out[1])),
              params,
              set.all=F)
    
    params$val[params$name=="L1HR"] <- "F, F, F"
    params$val[params$name=="L24HR"] <- "F, F, F"
    
    #write INP file to get all deposition outputs
    params[params$name == 'MODDAT', 'val'] <- gsub("\\.CON", "_Depo.FLX", conF)
    write.inp(calpost_templates[2], 
              file.path(output_dir, paste0(runName, cp.inp.out[2])),
              params,
              set.all=F)
  }
  
  #make bat files to run POSTUTIL and CALPOST
  batches = 1
  batchsize = ceiling(length(runNames) / batches)
  jobname <- "VNM"
  
  for(batch in 1:batches) {
    startN <- (batchsize*(batch-1)+1)
    source.subset <- startN:(startN+batchsize-1)
    source.subset <- source.subset[source.subset %in% 1:length(runNames)]
    
    if(length(source.subset)>0) {
      
      pu.bat <- file.path(output_dir, paste0("pu_", jobname, "_batch_", batch, ".bat"))
      expand.grid(runNames[source.subset], pu.inp.out) %>% apply(1, paste, collapse="") -> pu.runs
      writeLines(c(paste("cd", output_dir),
                   paste0(pu_exe, " ", normalizePath(file.path(output_dir, pu.runs))), "pause"), 
                 pu.bat)
      
      if(run_pu){
        shell.exec(normalizePath(pu.bat))
      }
      
      calpost.bat <- file.path(output_dir, paste0("calpost_", jobname, "_batch_", batch, ".bat"))
      expand.grid(runNames[source.subset], cp.inp.out) %>% apply(1, paste, collapse="") -> cp.runs
      writeLines(c(paste("cd", output_dir),
                   paste0(calpost_exe, " ", file.path(output_dir, cp.runs)), "pause"), 
                 calpost.bat)
      
      if(run_calpost){
        shell.exec(normalizePath(calpost.bat))
      }
    }
  }
  
  
}
