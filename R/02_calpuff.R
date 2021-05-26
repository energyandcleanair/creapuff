
#' Generate CALPUFF input
#'
#' @param wrf_dir 
#' @param output_dir 
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
runCalpuff <- function(
  run_name,
  params_allgrids,
  emissions_data=NULL, #data.frame with emissions data for all sources
  source_names=NULL,
  FGD=NULL,
  species_configuration = "so2_nox_pm_hg",
  emitted.polls = list(so2_nox_pm_hg=c('SO2','NO','NO2','PM15','PM10','PPM25','HG0','RGM'),
                       so2_nox_pm=c('SO2','NO','NO2','PM15','PM10','PPM25'))[[species_configuration]],
  receptors=NULL,
  bgconcs = NULL,
  o3dat = NULL,
  addparams = list(),
  addsubgroups = NULL,
  output_dir=unique(dirname(params_allgrids$METDAT)),
  gis_dir=get_gis_dir(),
  calpuff_exe='C:/CALPUFF/CALPUFF_v7.2.1_L150618/calpuff_v7.2.1.exe',
  only_make_additional_files=F,
  calpuff_template = list(so2_nox_pm_hg=system.file("extdata", "CALPUFF_7.0_template_Hg.INP", package="creapuff"),
                          so2_nox_pm=system.file("extdata", "CALPUFF_7.0_template.INP", package="creapuff"))[[species_configuration]]
){
  
  result = list() # Storing results we'll need for CALPUFF
  
  # Normalise paths: CALPUFF doesn't like ~
  output_dir %<>% normalizePath()
  gis_dir %<>% normalizePath()
  calpuff_exe %<>% normalizePath()
  
  params_allgrids %>% lapply(data.frame) %>% bind_rows(.id='grid_name') %>% mutate(run_name=run_name) %>% 
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
           TZ=ABTZ %>% gsub('UTC', '', .) %>% as.numeric %>% divide_by(100)) -> out_files
  
  target_crs <- getUTMproj(zone = unique(params_allgrids$UTMZ), hem = unique(params_allgrids$UTMH))
  
  out_files$dir <- output_dir
  
  calpuff_dir <- dirname(calpuff_exe)
  
  if(!is.null(emissions_data)) {
    #default Hg speciation in %, FGD flue gas desulfurization (FGD) systems
    "FGD  HG0 RGM Hgp
  F 43.9  54  2.1
  T 74.2  24  1.8" %>% textConnection %>% read.table(header=T) %>% 
      mutate_if(is.numeric, divide_by, 100) ->
      hg_species
    
    #rename columns to use default variable names
    emissions_data %<>% rename(Stack.height=contains("Stack height"), 
                        velocity=contains("velocity"),
                        diameter = contains("Diameter"),
                        exit.temp = contains("Temperature"))
    
    inpfiles_created <- character(0)
    
    inpfiles_created <- list()
    runsources_out=list()
    pm10fraction=list()
    
    emissions_data %>% to_spdf() %<>% spTransform(targetcrs) -> runsources
    
    runsources$source.name <- source_names
    runsources %>% coordinates() %>% data.frame() %>% 
      set_names(c('UTMx', 'UTMy')) %>% data.frame(runsources@data, .) -> runsources@data
    
    if(is.null(runsources$base.elevation..msl))
      runsources$base.elevation..msl <- get_plant_elev(runsources,
                                                       dir=output_dir,
                                                       out_files=out_files)
    
    for(emission_col in c('SO2_tpa', 'NOx_tpa', 'PM_tpa', 'Hg_kgpa', emitted.polls, 'downwash'))
      if(is.null(runsources[[emission_col]])) runsources[[emission_col]]=0
    
    if(is.null(runsources$SO2)) runsources@data %<>% mutate(SO2 = SO2_tpa)
    
    if(is.null(runsources$NO)) runsources@data %<>% mutate(NO  = NOx_tpa * .95 * 30/46,
                                                           NO2 = NOx_tpa * .05)
    
    if(is.null(runsources$PPM25)) runsources@data %<>% mutate(PM15 = PM_tpa * 26/80,
                                                              PM10 = PM_tpa * 30/80,
                                                              PPM25 = PM_tpa * 24/80)
    
    if(is.null(runsources$RGM)) 
      runsources@data %<>% mutate(HG0 = Hg_kgpa * hg_species$HG0[match(runsources$FGD, hg_species$FGD)], 
                                  RGM = Hg_kgpa * hg_species$RGM[match(runsources$FGD, hg_species$FGD)],
                                  Hgp = Hg_kgpa * hg_species$Hgp[match(runsources$FGD, hg_species$FGD)])
    
    runsources$exit.temp %<>% (function(x) x+ifelse(x < 273, 273.15, 0))
    
    runsources@data %>% ungroup %>% 
      select(UTMx, UTMy, Stack.height, base.elevation..msl, diameter, velocity, exit.temp, downwash,
             all_of(emitted.polls)) -> runsources2
    
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
  }

  
  
  if(is.null(addparams$DATUM)) addparams$DATUM="WGS-84"
  
  make_calpuff_inp(out_files,
                   calpuff_template=calpuff_template,
                   output_dir=output_dir,
                   puffrun=puffrun,
                   bgconcs = bgconcs,
                   OZONE.DAT = o3dat,
                   sourceLines=sourceLines,
                   receptors=receptors %>% subset(include) %>% make_topo_rows) ->
    inpfiles_created
  
  pm10fraction = sum(runsources$Hgp) / sum(runsources$PM10)
  

  #run CALPUFF
  org_dir <- getwd()
  setwd(output_dir)
  inpfiles_created %>% paste(calpuff_exe, .) %>% pbapply::pblapply(system)
  
  #create .bat files to run CALPUFF
  inpfiles_created %>% split(1) -> batches
  jobname='TESTVIET'
  for(i in seq_along(batches)) {
    batches[[i]] %>% paste(calpuff_exe, .) %>% c('pause') %>% 
      writeLines(paste0(jobname, '_', i, '.bat'))
  }
  setwd(org_dir)
  
  # Results needed for PostProcessing
  result$inpfiles_created <- inpfiles_created
  result$sources <- sources
  result$out_files_all <- out_files_all
  result$pm10fraction <- pm10fraction
  
  return(result)
}
