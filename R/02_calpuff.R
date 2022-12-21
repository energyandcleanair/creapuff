
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
  area_sources=NULL,
  FGD=NULL,
  AQCS=NULL,
  species_configuration=NULL, # "so2_nox_pm" or "so2_nox_pm_hg"
  emitted_polls = list(so2_nox_pm_hg=c('SO2','NO','NO2','PM15','PM10','PPM25','HG0','RGM'), 
                       so2_nox_pm=c('SO2','NO','NO2','PM15','PM10','PPM25'))[[species_configuration]],
  receptors=NULL,
  bgconcs = NULL,
  o3dat = NULL,
  monthly_scaling = NULL, #data frame with colums apply_to, pollutant, monthscaling; apply to can include emission_names in emissions_data or "all" for all sources; pollutants are SO2, NOx, PM, Hg; monthsscaling is a comma-separated string of 12 scaling factors
  addparams = list(),
  addsubgroups = list(),
  output_dir=unique(dirname(params_allgrids$METDAT)),
  gis_dir=get_gis_dir(),
  calpuff_exe='C:/CALPUFF/CALPUFF_v7.2.1_L150618/calpuff_v7.2.1.exe',
  only_make_additional_files=F,
  calpuff_template = list(so2_nox_pm_hg=system.file("extdata", "CALPUFF_7.0_template_Hg.INP", package="creapuff"),
                          so2_nox_pm=system.file("extdata", "CALPUFF_7.0_template.INP", package="creapuff"))[[species_configuration]]
) {
  # Add non-emitted species
  modeled_polls <- c("SO2","SO4","NO","NO2","HNO3","NO3","PM15","PM10","PPM25")
  if("RGM" %in% emitted_polls) modeled_polls %<>% c("HG0", "RGM")
  
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
    
  
    #rename columns to use default variable names
    emissions_data %<>% rename(Stack.height=matches("Stack.height"), 
                        velocity=contains("velocity"),
                        diameter = contains("Diameter"),
                        exit.temp = contains("Temperature"))
    
    runsources_out=list()
    pm10fraction=list()
    
    emissions_data %>% to_spdf() %>% spTransform(.,target_crs) -> runsources
    runsources %>% coordinates() %>% data.frame() %>% 
      set_names(c('UTMx', 'UTMy')) %>% data.frame(runsources@data, .) -> runsources@data
    
    if(!is.null(source_names)) { # LC : priority to external parameters
      if(max(nchar(source_names))>8 | any(duplicated(source_names)))
        source_names %<>% gsub(' ', '', .) %>% make_srcnam
    
      runsources$source_names <- source_names
    }
    
    if(is.null(runsources$source_names)) {
      source_names <- rep("s", nrow(emissions_data)) %>% make.names %>% make.unique(sep='')
      runsources$source_names <- source_names
    }
    
    if('RGM' %in% emitted_polls) {
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
      
      if(is.null(FGD) & is.null(runsources$FGD)) stop("error : no information on FGD of emission sources (TRUE / FALSE)")
      
      if(!is.null(FGD)) runsources$FGD <- FGD # LC : priority to external parameters 
      
      if(!is.null(monthly_scaling)) {
        
        #monthly emissions scaling
        same_scaling_for_all = all(monthly_scaling$apply_to=='all')
        
        #Subgroup (13d)
        expand.grid(SRCNAM = runsources$emission_names,
                    SPEC = emitted_polls,
                    stringsAsFactors = F) %>% 
          mutate(scale_by_pollutant=case_when(SPEC=='SO2'~'SO2', grepl('^NO', SPEC)~'NOx', grepl('^P?PM', SPEC)~'PM', grepl('^HG|RGM', SPEC)~'Hg'),
                 scale_by_facility=case_when(same_scaling_for_all~'all', T~SRCNAM),
                 nr=seq_along(SRCNAM),
                 str=paste0(nr," ! SCALEFACTOR = ",SRCNAM,", ",SPEC,", ",scale_by_facility,'_',scale_by_pollutant," !  !END!")) %>% 
          use_series(str) ->
          addsubgroups$X13d
        
        #Subgroup (19b)
        monthly_scaling %>% 
          group_by(apply_to, pollutant) %>% 
          group_map(function(df, group) {
            c(paste0("!FACTORNAME = ",group$apply_to,'_',group$pollutant," !"),
              "!FACTORTYPE = MONTH12 !",
              paste("!FACTORTABLE =",df$monthscaling,"!"),
              "!END!")
          }) %>% unlist %>% unname -> 
          addsubgroups$X19b
        
        addparams$NSPT1 = length(monthscalingsubgroups$X13d)
        addparams$NSFTAB = length(monthscalingsubgroups$X19b)/4
      }
    }
    
    if(is.null(runsources$base.elevation..msl))
      runsources$base.elevation..msl <- get_plant_elev(runsources,
                                                       dir=output_dir,
                                                       files_met=out_files)
    
    runsources %<>% add_missing_columns(c('SO2_tpa', 'NOx_tpa', 'PM_tpa', 'Hg_kgpa', 'downwash'))
    
    if(is.null(runsources$SO2)) runsources@data %<>% mutate(SO2 = SO2_tpa)  # LC, mutate if "!is.null"
    
    if(is.null(runsources$NO)) runsources@data %<>% mutate(NO  = NOx_tpa * .95 * 30/46,  # LC, mutate if "!is.null"
                                                            NO2 = NOx_tpa * .05)
    
    if(is.null(runsources$PPM25)) runsources@data %<>% mutate(PM15 = PM_tpa * 26/80,  # LC, mutate if "!is.null"
                                                               PM10 = PM_tpa * 30/80,
                                                              PPM25 = PM_tpa * 24/80)
    if(is.null(runsources$RGM) & 'RGM' %in% emitted_polls){
        runsources$AQCS[runsources$FGD] <- "ESP+wFGD" # If FGD is TRUE we use the standard values for ESP+wFGD. For the moment, no data for other AQCS types. 
        speciation_matrix %>% 
          left_join(runsources@data %>% select(-any_of(c('HG0', 'RGM', 'Hgp'))), .) %>% 
          mutate(across(c(HG0,RGM,Hgp), multiply_by, Hg_kgpa)) ->
          runsources@data
    }
    
    runsources$exit.temp %<>% (function(x) x+ifelse(x < 273, 273.15, 0))
    
    runsources %<>% add_missing_columns(modeled_polls)
    runsources@data %>% ungroup %>% 
      select(UTMx, UTMy, Stack.height, base.elevation..msl, diameter, velocity, exit.temp, downwash,
             all_of(modeled_polls)) -> runsources2
    
    
    runsources2 %>% 
      mutate_all(signif, 6) %>% mutate_all(format, nsmall=1) %>% 
      apply(1, paste, collapse=", ") -> emislines
    
    print("__________ Plant specifications ___________")
    print(runsources2[1:8])
    print("______________ Emission rates _____________")
    print(runsources2[-1:-8])
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
  
  if(!is.null(area_sources)) {
    if(!('sf' %in% class(area_source_coords)))
      area_sources %<>% to_spdf %>% st_as_sf() %>% group_by(emission_names) %>% summarise('MULTIPOINT') %>% st_cast('POLYGON')
    
    area_sources %<>% st_transform(crs=target_crs)
    
    if(is.null(area_sources$base_elevation))
      area_sources %>% 
      st_centroid() %>% 
      as('Spatial') %>% 
      get_plant_elev(dir=output_dir,
                     files_met=out_files) ->
      area_sources$base_elevation
    
    if(is.null(area_sources$emission_height)) area_sources$emission_height <- 5
    if(is.null(area_sources$height_sigma)) area_sources$height_sigma <- area_sources$emission_height
    
    area_sources %<>% add_missing_columns(modeled_polls)
    
    #divide emission rates by area
    #required emission unit is metric tons/m**2/yr
    area_sources %>% st_area() %>% as.numeric() %>% multiply_by(1e6) -> 
      area_sources$area_m2
    
    area_sources %<>% mutate(across(all_of(modeled_polls), divide_by, area_m2))
    
    addsubgroups$X14b = 
      area_sources %>% group_by(emission_names) %>% 
      mutate(across(all_of(modeled_polls), signif, 6)) %>% 
      unite('array_str', emission_height, base_elevation, height_sigma, all_of(modeled_polls), sep=', ') %>% 
      group_map(function(df, group) {
        c(paste("! SRCNAM =",group$emission_names,"!"),
          paste("! X = ", df$array_str, "!"),
          "!END!")
      })
    
    
    addsubgroups$X14c = 
      area_sources %>% group_by(emission_names) %>% 
      group_map(function(df, group) {
        df %>% st_coordinates() %>% as_tibble %>% head(4) %>% 
          mutate(across(c(X, Y), round, 3)) %>% 
          summarise(across(c(X, Y), paste, collapse=', ')) -> coords_out
        c(paste("! SRCNAM =",group$emission_names,"!"),
          paste("! XVERT = ", coords_out$X, "!"),
          paste("! YVERT = ", coords_out$Y, "!"),
          "!END!")
      })
    
    addparams$NAR1 <- nrow(area_sources)
  }
  
  if(is.null(addparams$DATUM)) addparams$DATUM="WGS-84"
  receptors_selected = NULL
  if(!is.null(receptors)) receptors %>% subset(include) %>% make_topo_rows -> receptors_selected 
  
  print("______________ Additional subgroups written to CALPUFF.INP _____________")
  print(addsubgroups)
  print("")
  print("______________ Additional parameters written to CALPUFF.INP _____________")
  print(addparams)
  print("")
  
  
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
