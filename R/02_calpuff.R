
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
  AQCS=NULL,
  species_configuration=NULL, # "so2_nox_pm" or "so2_nox_pm_hg"
  emitted_polls = list(so2_nox_pm_hg=c('SO2','NO','NO2','PM15','PM10','PPM25','HG0','RGM'), 
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
    mutate(StartDate=paste(IBYR, IBMO, IBDY,IBHR) %>% ymd_h %>% format("%Y%m%d%H"),
           EndDate=paste(IEYR, IEMO, IEDY,IEHR) %>% ymd_h %>% format("%Y%m%d%H"),
           TZ=ABTZ %>% gsub('UTC', '', .) %>% as.numeric %>% divide_by(100)) -> out_files
  
  target_crs <- get_utm_proj(zone = unique(out_files$UTMZ), hem = unique(out_files$UTMH)) # LC
  
  out_files$dir <- output_dir
  
  calpuff_dir <- dirname(calpuff_exe)
  
  if(!is.null(emissions_data)) {
    
  # Lauri. Speciation data from Lee et al. (2006).  
  #   "FGD  HG0 RGM Hgp
  # F 43.9  54  2.1
  # T 74.2  24  1.8" %>% textConnection %>% read.table(header=T) %>% 
  #     mutate_if(is.numeric, divide_by, 100) ->
  #     hg_species
  
  # LC. Speciation data from Zhang et al. (2016), "Mercury transformation and speciation in flue gas... "
   speciation_matrix <- data.frame(AQCS=c("ESP+wFGD","ESP","FF","none","CFBC"),
                                   FGD=c(T,F,F,F,F),
                                   HG0=c(84,58,50,56,72),
                                   RGM=c(16,41,49,34,27),
                                   Hgp=c(0.6,1.3,0.5,10,0.6)) %>%  mutate_if(is.numeric, divide_by, 100)

    #rename columns to use default variable names
    emissions_data %<>% rename(Stack.height=contains("Stack height"), 
                        velocity=contains("velocity"),
                        diameter = contains("Diameter"),
                        exit.temp = contains("Temperature"))
    
    inpfiles_created <- character(0)
    
    inpfiles_created <- list()
    runsources_out=list()
    pm10fraction=list()
    
    emissions_data %>% to_spdf() %>% spTransform(.,target_crs) -> runsources
    runsources %>% coordinates() %>% data.frame() %>% 
      set_names(c('UTMx', 'UTMy')) %>% data.frame(runsources@data, .) -> runsources@data
    
    if (!is.null(source_names))  # LC : priority to external parameters
      runsources$source_names <- source_names %>% gsub(' ', '', .) %>% substr(1,5) %>% make.names %>% make.unique(sep='')
    else 
      if (is.null(runsources$source_names)) {
        source_names <- rep("s", nrow(emissions_data)) %>% make.names %>% make.unique(sep='')
        runsources$source_names <- source_names
      }
    
    ifelse (!is.null(FGD), FGD, ifelse(!is.null(runsources$FGD), runsources$FGD, stop("error : no information on FGD of emission sources (TRUE / FALSE)"))
            ) %>% as.logical() -> runsources$FGD # LC : priority to external parameters 

    ifelse (!is.null(AQCS), AQCS, ifelse(!is.null(runsources$AQCS), runsources$AQCS, stop("error : no information on AQCS of emission sources (ESP+wFGD / ESP / FF / none / CFBC)"))
    ) -> runsources$AQCS # LC : priority to external parameters
    
    if(is.null(runsources$base.elevation..msl))
      runsources$base.elevation..msl <- get_plant_elev(runsources,
                                                       dir=output_dir,
                                                       files_met=out_files)
    
    for(emission_col in c('SO2_tpa', 'NOx_tpa', 'PM_tpa', 'Hg_kgpa', emitted_polls, 'downwash'))
      if(is.null(runsources[[emission_col]])) runsources[[emission_col]]=0
    
    if(!is.null(runsources$SO2)) runsources@data %<>% mutate(SO2 = SO2_tpa)  # LC, mutate if "!is.null"
    
    if(!is.null(runsources$NO)) runsources@data %<>% mutate(NO  = NOx_tpa * .95 * 30/46,  # LC, mutate if "!is.null"
                                                            NO2 = NOx_tpa * .05)
    
    if(!is.null(runsources$PPM25)) runsources@data %<>% mutate(PM15 = PM_tpa * 26/80,  # LC, mutate if "!is.null"
                                                               PM10 = PM_tpa * 30/80,
                                                              PPM25 = PM_tpa * 24/80)
    if(!is.null(runsources$RGM)){
        if(runsources$FGD==T) runsources$AQCS = "ESP+wFGD" # If FGD is TRUE we use the standard values for ESP+wFGD. For the moment, no data for other AQCS types. 
        runsources@data %<>% mutate(HG0 = Hg_kgpa * speciation_matrix %>% filter (FGD==runsources$FGD) %>% filter (AQCS==runsources$AQCS) %>% pull(HG0),
                                    RGM = Hg_kgpa * speciation_matrix %>% filter (FGD==runsources$FGD) %>% filter (AQCS==runsources$AQCS) %>% pull(RGM),
                                    Hgp = Hg_kgpa * speciation_matrix %>% filter (FGD==runsources$FGD) %>% filter (AQCS==runsources$AQCS) %>% pull(Hgp))
    }
    
    runsources$exit.temp %<>% (function(x) x+ifelse(x < 273, 273.15, 0))
    
    runsources@data %>% ungroup %>% 
      select(UTMx, UTMy, Stack.height, base.elevation..msl, diameter, velocity, exit.temp, downwash,
             all_of(emitted_polls)) -> runsources2
    
    # Non-emitted species
    runsources2  %<>% mutate(SO4=0) %<>% relocate (SO4, .after=SO2)
    runsources2  %<>% mutate(HNO3=0) %<>% relocate (HNO3, .after=NO2)
    runsources2  %<>% mutate(NO3=0) %<>% relocate (NO3, .after=HNO3)
    
    runsources2 %>% 
      mutate_all(signif, 6) %>% mutate_all(format, nsmall=1) %>% 
      apply(1, paste, collapse=", ") -> emislines
    
    print("__________ Plant specifications ___________")
    print(runsources2[1:8])
    print("______________ Emission rates _____________")
    print(runsources2[9:19])
    print("")
    
    # Modeled species (11 species)
    # SO2  : 
    # SO4  : not emitted -> 0
    # NO   : 
    # NO2  :
    # HNO3 : not emitted -> 0
    # NO3  : not emitted -> 0
    # PM15 : 
    # PM10 : 
    # PPM25: 
    # Hg0  : gas-phase elemental mercury
    # RGM  : inorganic reactive gaseous mercury (particulate mercury, Hgp, is not considered)
    
    #  Source       X         Y       Stack    Base     Stack    Exit  Exit    Bldg.  Emission
    #No.     Coordinate Coordinate Height Elevation Diameter  Vel.  Temp.   Dwash   Rates
    #(km)      (km)       (m)      (m)       (m)  (m/s) (deg. K)         
    #------   ---------- ---------- ------  ------   -------- ----- -------- ----- --------
    
    source_lines <- list()
    for(i in 1:nrow(runsources))
      source_lines[[runsources$source_names[i]]] <- c(paste0("! SRCNAM = ",runsources$source_names[i]," !"),
                                                    paste("! X = ", emislines[i], "!"),
                                                    "! ZPLTFM  =       .0 !",
                                                    "! FMFAC  =      1.0 !   !END!")
  }
  else
  {
    # LC, what if(is.null(emissions_data)) ?
    source_lines=NULL  # LC
    runsources=NULL    # LC
  }  
  
  if(is.null(addparams$DATUM)) addparams$DATUM="WGS-84"
  receptors_selected = NULL
  if(!is.null(receptors)) receptors %>% subset(include) %>% make_topo_rows -> receptors_selected 
  
  make_calpuff_inp(out_files,
                   calpuff_template=calpuff_template,
                   output_dir=output_dir,
                   puffrun=run_name,
                   bgconcs = bgconcs,        
                   OZONE.DAT = o3dat,        
                   source_lines = source_lines,  
                   addparams = addparams,
                   addsubgroups = addsubgroups,
                   receptors = receptors_selected)  -> 
                   # receptors=receptors %>% subset(include) %>% make_topo_rows) ->   # LC : subset (only if not NULL) outside the function call   
    inpfiles_created

  if (species_configuration == "so2_nox_pm_hg") 
    pm10fraction = sum(runsources$Hgp) / sum(runsources$PM10)
  if (species_configuration == "so2_nox_pm") 
    pm10fraction = NULL

  # Run CALPUFF
  org_dir <- getwd()
  setwd(output_dir)
  # inpfiles_created %>% paste(calpuff_exe, .) %>% pbapply::pblapply(system)
  
  # Create .bat files to run CALPUFF
  inpfiles_created %>% split(1) -> batches
  jobname=run_name
  for(i in seq_along(batches)) {
    batches[[i]] %>% paste(calpuff_exe, .) %>% c('pause') %>% 
      writeLines(paste0(jobname, '_', i, '.bat'))
  }
  setwd(org_dir)
  
  # Results needed for PostProcessing
  result$inpfiles_created <- inpfiles_created
  result$sources <- runsources   
  result$out_files <- out_files  
  result$pm10fraction <- pm10fraction
  
  saveRDS(result, file.path(output_dir, paste0('calpuff_result_',run_name,'.RDS')))  # LC : save result
  
  return(result)
}
