
#' Generate CALPUFF input
#'
#' @param input_xls 
#' @param start_date 
#' @param wrf_dir 
#' @param expand_grids 
#' @param output_dir 
#' @param grids 
#' @param params_allgrids 
#' @param gis_dir 
#' @param calpuff_exe 
#' @param only_make_additional_files 
#' @param calpuff_template 
#'
#' @return
#' @export
#'
#' @examples
calpuff.generate_input <- function(
  input_xls,
  start_date, # Used to know which power plants are 'operating' or 'new'
  wrf_dir,
  mod_dir,
  expand_grids,
  output_dir,
  grids,
  params_allgrids,
  gis_dir,
  calpuff_exe,
  only_make_additional_files=F,
  calpuff_template
){
  
  
  # Normalise paths: CALPUFF doesn't like ~
  wrf_dir %<>% normalizePath()
  output_dir %<>% normalizePath()
  gis_dir %<>% normalizePath()
  calpuff_exe %<>% normalizePath()
  
  # Read from input
  target_crs <- raster::crs(grids[[1]])
  
  # params_allgrids = readRDS(file.path(output_dir, paste0('params_allgrids_', run_name, '.RDS')))
  params_allgrids %>% lapply(data.frame) %>% bind_rows(.id='gridName') %>% mutate(runName=run_name) %>% 
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
           TZ=ABTZ %>% gsub('UTC', '', .) %>% as.numeric %>% divide_by(100)) -> outFiles
  
  outFiles$dir <- output_dir
  if(!exists('outFilesAll')) outFiles -> outFilesAll
  
  #create multiple .INP files for individual sources or source clusters
  calpuff_dir <- dirname(calpuff_exe)

  ## Viet
  read_xlsx(input_xls, sheet='CALPUFF input') -> emis # LC
  
  ## define which sources should be included in which scenario. COD = commercial operation date
  emis$COD %>% substr(.,nchar(.)-3,nchar(.)) %>% as.numeric () < start_date %>% format(., format = "%Y") %>% as.numeric() -> emis$existing_plant # LC
  emis$Status_Simple = ifelse(emis$existing_plant,'operating', 'new')
  
  
  #produce unique ascii names with 8 characters -Lauri -LC
  emis$Plants %>% gsub(' ', '', .) %>% substr(1,5) %>% stringi::stri_trans_general("Latin-ASCII") %>% 
    make.names %>% make.unique(sep='') %>% 
    paste0('_', substr(emis$Status_Simple,1,1)) -> emis$scenario
  # LC : using full names. I see no need for shortness, but I do for clarity  
  #emis$Plants %>% gsub(' ', '', .) %>% stringi::stri_trans_general("Latin-ASCII") %>% 
  #  make.names %>% make.unique(sep='') %>% 
  #  paste0('_', emis$Status_Simple) -> emis$scenario
  emis$scenario %>% nchar %>% max
  
  #combine emissions data with outFiles
  emis$runName <- outFiles$runName %>% unique
  merge(emis, outFiles[, c('runName', 'UTMZ', 'UTMH')], all.x=T, all.y=F) %>% unique -> sources
  
  sources$Lat %<>% as.numeric()
  sources$Long %<>% as.numeric()
  
  #exclude sources outside domain  -Lauri
  domPols = gridsToDomPols(grids, target_crs)
  sources %<>% spdf %>% crop(spTransform(domPols, crs(.))) %>% '@'('data')
  
  sources$source.name <- sources$scenario # LC
  sources$puffrun <- sources$scenario
  #all plants have FGD -Lauri
  sources$FGD = T
  #Hg speciation in % -Lauri # LC, FGD flue gas desulfurization (FGD) systems
  "FGD	HG0	RGM	Hgp
  F	43.9	54	2.1
  T	74.2	24	1.8" %>% textConnection %>% read.table(header=T) %>% 
    mutate_if(is.numeric, divide_by, 100) ->
    hg_species
  #rename columns to use default variable names -Lauri
  sources %<>% rename(Stack.height=contains("Stack height"), 
                      velocity=contains("velocity"),
                      diameter = contains("Diameter"),
                      exit.temp = contains("Temperature"))
  
  # outDir = runDir()
  
  #---------------------- 
  #get receptors for all
  nesfactL = c(3, 9) #nesting factor of 24 with resolution of 9km would be overkill! -Lauri
  nesfact_range = c(30, 10)
  if(!exists('topoAll')) topoAll = list()
  
  queue = unique(sources$source.name)
  
  for(run in queue) {
    
    outF = sources %>% filter(source.name == run) %>% head(1)
    
    target_crs = getUTMproj(outF$UTMZ, outF$UTMH)
    
    loc = outF %>% spdf %>% spTransform(target_crs)
    
    #get discrete receptors with 400x400 dim and 1km, 2.5km, 10km resos
    get_recep(loc, nesfactL=nesfactL, output_dir=output_dir, calpuff_exe=calpuff_exe) -> topoAll[[run]]
    
    print(run)
  }
  
  bgconcs = get_bgconcs(sources,  mod_dir=mod_dir)
  
  o3dat = NULL #NULL : hourly Ozone Data File (Optional). No ozone monitoring stations, in both PH or Vietnam
  
  inpfiles_created <- character(0)
  
  emitted.polls = c('SO2','NO','NO2','PM15','PM10','PPM25','HG0','RGM')
  
  inpfiles_created <- list()
  runsources_out=list()
  pm10fraction=list()
  
  queue = sources$runName %>% unique
  for(metrun in queue) {
    sources %>% filter(runName == metrun) %>% spdf -> runsources
    metfiles <- outFilesAll %>% filter(runName == unique(runsources$runName))
    
    targetcrs = getUTMproj(metfiles$UTMZ[1], metfiles$UTMH[1])
    runsources %<>% spTransform(targetcrs)
    runsources %>% coordinates() %>% data.frame() %>% 
      set_names('UTMx', 'UTMy') %>% data.frame(runsources@data, .) -> runsources@data
    runsources$base.elevation..msl <- getPlantElev(runsources,
                                                   dir=output_dir,
                                                   outfiles=metfiles)
    
    runsources@data %<>% mutate(SO2 = SO2_tpa,
                                SO4 = 0,
                                NO  = NOx_tpa * .95 * 30/46,
                                NO2 = NOx_tpa * .05,
                                HNO3 = 0,
                                NO3 = 0,
                                PM15 = PM_tpa * 26/80,
                                PM10 = PM_tpa * 30/80,
                                PPM25 = PM_tpa * 24/80,
                                HG0 = Hg_kgpa * hg_species$HG0[match(runsources$FGD, hg_species$FGD)], 
                                RGM = Hg_kgpa * hg_species$RGM[match(runsources$FGD, hg_species$FGD)],
                                Hgp = Hg_kgpa * hg_species$Hgp[match(runsources$FGD, hg_species$FGD)],
                                downwash = 0)
    
    runsources$exit.temp %<>% (function(x) x+ifelse(x < 273, 273.15, 0))
    
    runsources@data %>% ungroup %>% 
      sel(UTMx, UTMy, Stack.height, base.elevation..msl, diameter, velocity, exit.temp, downwash,
          SO2,SO4,NO,NO2,HNO3,NO3,PM15,PM10,PPM25, HG0, RGM) -> runsources2
    
    runsources2 %>% 
      mutate_all(signif, 6) %>% mutate_all(format, nsmall=1) %>% 
      apply(1, paste, collapse=", ") -> emislines
    
    #  Source       X         Y       Stack    Base     Stack    Exit  Exit    Bldg.  Emission
    #No.     Coordinate Coordinate Height Elevation Diameter  Vel.  Temp.   Dwash   Rates
    #(km)      (km)       (m)      (m)       (m)  (m/s) (deg. K)         
    #------   ---------- ---------- ------  ------   -------- ----- -------- ----- --------
    
    sourceLines <- list()
    for(i in 1:nrow(runsources))
      sourceLines[[runsources$source.name[i]]] <- c(paste0("! SRCNAM =",runsources$source.name[i],"!"),
                                                    paste("! X =", emislines[i], "!"),
                                                    "! ZPLTFM  =       .0 !",
                                                    "! FMFAC  =      1.0 !   !END!")
    
    topoAll %>% do.call(rbind, .) -> intopo
    intopo %<>% subset(!duplicated(coordinates(.)))
    r=raster(extent(intopo), res=1, crs=crs(intopo))
    
    runsources$flag=1
    sourcesR=rasterize(runsources, r, 'flag')
    dist_to_source=distance(sourcesR)
    extract(dist_to_source, intopo) -> intopo$dist_to_source
    
    intopo$include=F
    
    for(i in seq_along(nesfactL))
      intopo$include[intopo$dist_to_source<nesfact_range[i] & intopo$nesfact==nesfactL[i]] <- T
    
    print(paste(metrun, sum(intopo$include), 'receptors'))
    if(sum(intopo$include)+metfiles$GridNX[1]*metfiles$GridNY[1]>=10000) stop('too many receptors!')
    
    plotadm = getadm(0, 'coarse') %>% cropProj(r) # LC : #
    quickpng(paste0(metrun, ' receptors.png'))     
    intopo %>% subset(include) %>% plot(col='gray', cex=.5)
    plotadm %>% plot(add=T, border='steelblue') # LC : #
    
    runsources %>% plot(add=T)
    dev.off()
    
    addparams = list(DATUM="WGS-84")
    
    for(puffrun in runsources$source.name) {
      make_calpuff_inp(metfiles,
                       puffrun=puffrun,
                       bgconcs = bgconcs[bgconcs$puffrun == puffrun,],
                       OZONE.DAT = o3dat,
                       sourceLines=sourceLines[[puffrun]],
                       receptors=intopo %>% subset(include) %>% make_toporows,
                       addparams = addparams,
                       addsubgroups = NULL) ->
        inpfiles_created[[puffrun]]
      
      pm10fraction[[puffrun]] = sum(runsources$Hgp) / sum(runsources$PM10)
    }
  }
  
  #run CALPUFF
  setwd(calpuffDir)
  inpfiles_created %>% paste('calpuff_v7.2.1.exe', .) %>% pblapply(system)
  
  #create .bat files to run CALPUFF
  inpfiles_created %>% split(1) -> batches
  jobname='TESTVIET'
  for(i in seq_along(batches)) {
    batches[[i]] %>% paste('calpuff_v7.2.1.exe', .) %>% c('pause') %>% 
      writeLines(paste0(jobname, '_', i, '.bat'))
  }
}
