

#' Generate input for CALMET
#'
#' @param input_xls
#' @param wrf_dir directory where to find m3d files
#' @param expand_grids regex string - if grid name matches, the scripts attempts to expand the grid
#' @param expand_ncells number of cells to expand the selected grid(s) in each direction (use negative values to crop domain area)
#' @param output_dir directory where to write generated input and bat files
#' @param gis_folder directory where to find land cover files
#' @param calmet_exe filepath (full) of CALMET exe
#' @param calmet_templates (list) list with noobs and surfobs template full file paths
#' @param only_make_additional_files F: make files anyway and do not skip; T: skip if output file already exists
#' @param out_files MYSTERY: this one seems to be generated after this function is called, yet used here
#' @param run_calmet whether to actually run CALMET 
#'
#' @return
#' @export
#'
#' @examples
runCalmet <- function(
  input_xls,
  wrf_dir,
  expand_grids,
  expand_ncells,
  output_dir,
  gis_dir,
  calmet_exe,
  calmet_templates,
  only_make_additional_files=F,
  out_files=NULL,
  run_calmet=F
){
  
  result = list() # Storing results we'll need for CALPUFF
  dir.create(output_dir, showWarnings = F, recursive = T)
  
  # Normalise paths: CALPUFF doesn't like ~
  input_xls %<>% normalizePath()
  wrf_dir %<>% normalizePath()
  output_dir %<>% normalizePath()
  gis_dir %<>% normalizePath()
  calmet_exe %<>% normalizePath()

  # WRF ---------------------------------------------------------------------
  wd_org <- getwd()
  setwd(output_dir)
  
  # Find m3d files in wrf dir
  m3d <- list.files(path = wrf_dir, pattern = '\\.m3d$', recursive = F)
  if(length(m3d)==0) stop(sprintf("no .m3d file in %s",wrf_dir))
  m3d %<>% strsplit('_') %>% ldply %>%
    set_names(c('run_name','grid_name','filename_start_date','filename_end_date')) %>%
    tibble(path=file.path(wrf_dir, m3d), .)
  m3d$filename_start_date %<>% gsub(".m3d","",.) %>% paste(.,"00", sep = " ") %>%  ymd_h  # LC
  m3d$filename_end_date %<>% gsub(".m3d","",.) %>% paste(.,"23", sep = " ") %>%  ymd_h  # LC
  end_date <- m3d$filename_end_date[length(m3d$filename_end_date)]  # LC
  run_name <- m3d$run_name[1] # LC
  
  #build a data frame with the header info from all files
  # m3d %<>% tibble(path=.) %>% mutate(grid_name=gsub('\\.m3d', '', .),
  #                                   file=basename(path))
  
  crs_header <- m3d %>% ddply(.(grid_name), 
                function(df) {
                  crsinfo=df$path[1] %>% scan(skip=4, nlines=1, what=character())
                  starttime=df$path[1] %>% scan(skip=6, nlines=1, what=character()) 
                  c(crsinfo, starttime[1])
                })
  names(crs_header)[-1] <- c('proj', 'rlat', 'rlon', 'xlat1', 'xlat2',
                             'xorigkm', 'yorigkm', 'd', 'nx', 'ny', 'nz', 'starttime')
  crs_header[,3:12] %<>% mutate_all(as.numeric)
  crs_header$starttime %<>% ymd_h
  m3d %<>% left_join(crs_header)
  m3d$expand <- m3d$grid_name %>% grepl(expand_grids, .)
  
  #read grid data from file
  m3d_grid <- m3d %>% dlply(.(grid_name), 
                function(df) {
                  df=df[1,]
                  read.table(df$path,
                             skip=8+df$nz,
                             nrows=df$nx*df$ny) %>% 
                    set_names(c('i', 'j', 'lat', 'lon', 'elev', 'lu', 'x1', 'x2', 'x3'))
                })
  
  #center of domain
  domain_center <- m3d_grid %>% bind_rows() %>% summarise_all(mean) %>% to_spdf
  
  #determine coordinate system to use
  target_crs <- get_utm_proj(loc=domain_center)
  
  #UTM zone and hemisphere
  UTMZ <- target_crs %>% strsplit(' ') %>% unlist %>% 
    grep('\\+zone', ., value=T) %>% gsub('\\+zone=', '', .) %>% as.numeric
  UTMH <- ifelse(grepl('+south', target_crs), 'S', 'N')
  
  #time zone
  TZ <- round(domain_center$lon / 15, 0)
  
  #create raster objects in target crs that fit inside each WRF grid
  grids = list()
  for(g in seq_along(m3d_grid)) {
    grid_name = names(m3d_grid)[g]
    res = m3d$d[m3d$grid_name==grid_name][1]
    expand_degs = ifelse(m3d$expand[m3d$grid_name==grid_name][1],
                         2*expand_ncells*res/100, 0)
    m3d_grid[[g]] %>% to_spdf %>% extent() %>% add(expand_degs) %>% 
      as('SpatialPolygons') -> lldomain # Non-skewed domain, in the original ll rcs
    crs(lldomain) <- creapuff.env$llproj
    lldomain %>% spTransform(target_crs) -> lldomain_utm # LC : Skewed domain, in the UTM crs
    lldomain_utm %>% extent -> bb # LC : Non-skewed rectangular boundary, with larger area than skewed domain
    step <- res/10
    shrink <- 0
    contains <- F
    
    while(!contains) {
      # reduction of non-skewed and bigger domain to completely fits within the original skewed domain  
      bb %>% subtract(shrink) -> bb_new 
      bb_new %>% as('SpatialPolygons') -> utmdomain
      crs(utmdomain) <- target_crs
      # spplot(lldomain_utm,sp.layout = utmdomain) # LC : for plotting and check
      contains <- lldomain_utm %>% gContains(utmdomain) # lldomain_utm (g)Contains utmdomain? If NO, continue reduction
      shrink %<>% add(step)
    }
    
    grids[[grid_name]] = raster(bb_new, res=res, crs=target_crs)
  }
  
  #create polygons of grid boundaries for plotting
  domPols = grids_to_domains(grids, target_crs)
  
  #admin boundaries for plotting
  get_adm(level=0, res='low') %>% cropProj(domPols) -> admUTM
  
  #plot sources and domains
  read_xlsx(input_xls, sheet='CALPUFF input') %>% 
    mutate_at(c('Lat', 'Long'), as.numeric) %>% to_spdf -> sources
  
  ggplot() + annotation_spatial(admUTM) + layer_spatial(domPols, fill=NA) +
    theme(panel.grid=element_blank(), panel.background=element_rect(fill='lightblue')) + 
    annotation_spatial(sources, col='orange')
  ggsave(file.path(output_dir, paste0(run_name, '_', 'domains.png')))
  
  surf.dat <- NA  # NA : don't make SURF.DAT and run without it. 
  
  #make GEO.DAT and CALMET.INP files
  params_allgrids <- list()
  start_dates <- list()
  
  for(g in seq_along(grids)) {
    grid_name = names(grids)[g]
    gridR = grids[[g]]
    res=res(gridR)[1]
    
    start_date = m3d$starttime[m3d$grid_name==grid_name][1] + as.difftime(TZ, units='hours')
    if(hour(start_date)>5) {
      start_date %<>% add(as.difftime(1, units='days'))
      hour(start_date)=0
    }
    end_date   = tail(m3d$filename_end_date [m3d$grid_name==grid_name],n=1) + as.difftime(TZ, units='hours') # LC, CHECK

    nx = ncol(gridR)
    ny = nrow(gridR)
    bb = extent(gridR)
    
    # make GEO.DAT -> grid_name.geo file (in output dir: working dir). Skip it with --> only_make_additional_files=T
    geo.file=file.path(output_dir, paste0(grid_name, '.geo'))
    if(!file.exists(geo.file) | !only_make_additional_files) {
      zoom=8-floor(log(res)/log(2))
      
      lu_gridR <- raster::disaggregate(gridR, round(res/.3, 0)) # LC : 3.3x WRF grid resolution
      elevatr::get_elev_raster(as(gridR, "SpatialPoints"), z=zoom) -> elevR #HT convert to SpatialPoints first to prevent nrow(.)==NULL bug in elevatr
      elevR %<>% max(0)
      resample(elevR, gridR) -> elev.out # LC : WRF grid resolution
      
      raster(file.path(gis_dir,
                       "landcover",
                       'C3S-LC-L4-LCCS-Map-300m-P1Y-2018-v2.1.1.nc')) %>% # LC : Global Land Use/Cover Product for 2016-2018, only integer values
        cropProj(lu_gridR, method='ngb') -> luR
      
      read_xlsx(file.path(gis_dir, "landcover", 'ESACCI-LC-Legend-USGS.xlsx')) -> lc_codes  # LC : Land Use/Cover Codes
      luR_USGS <- luR
      lc_codes$USGS.code[match(values(luR), lc_codes$NB_LAB)] -> values(luR_USGS) # LC : Land Use/Cover Codes in USGS format
      
      aggregate(luR_USGS, round(res/res(luR_USGS)/2, 0), statmode, na.rm=T) -> luR_coarse
      resample(luR_coarse, gridR, method='ngb') -> lu.out
      #resample(luR_USGS, gridR, method='ngb') -> lu.out2 # LC : other possibility, less accurate (?)
      
      # setwd(runDir())
      
      geo.lu = lu.out %>% raster::as.matrix() %>% round(0) %>% apply(1, paste, collapse=' ')
      geo.elev = elev.out %>% raster::as.matrix() %>% round(0) %>% apply(1, paste, collapse=' ')
      
      geo.header <- c('GEO.DAT         2.0             Header structure with coordinate parameters',
                      '   0',
                      'UTM',
                      paste0('  ', UTMZ, UTMH),
                      'WGS-84   10-10-2002  ',
                      paste0(
                        paste(format(c(nx, ny),digits=0,width=8),collapse=""),
                        paste(format(c(bb@xmin, bb@ymin, res, res),nsmall=3,width=12),
                              collapse="")),
                      'KM  M   ', '  0')
      
      geo.out = c(geo.header, geo.lu, '1.0', geo.elev, rep(0, 6))
      geo.out %>% writeLines(geo.file)
      
      quickpng(file.path(output_dir, paste0(grid_name, '_', 'terrain.png')))
      levelplot(elev.out, col.regions=c('steelblue', terrain.colors(255)), margin=F, 
                main='Terrain elevations') -> p
      print(p)
      dev.off()
      
      values(lu.out) <- lc_codes$LCCOwnLabel[match(lu.out[], lc_codes$USGS.code)] %>% as.factor()
      quickpng(file.path(output_dir, paste0(grid_name, '_', 'landuse.png')))
      levelplot(lu.out,main='Land use', 
                col.regions=brewer.pal(12, 'Paired')[rev(c(1, 10, 4, 8, 3, 7))]) -> p
      print(p)
      dev.off()
    }
    
    m3d_to_use = m3d$path[m3d$grid_name==grid_name] # LS : Select all days for each drid
    
    params <- list(
      GEODAT= file.path(output_dir, paste0(grid_name, '.geo')),
      METLST = paste0(grid_name,"_CALMET.LST"),
      METDAT = file.path(output_dir, paste0(grid_name,"_CALMET.DAT")),
      NM3D = length(m3d_to_use),
      M3DDAT = 'not set',
      IBYR = year(start_date),
      IEYR = year(end_date),
      IBMO = month(start_date),
      IBDY = day(start_date),
      IEMO = month(end_date),
      IEDY = day(end_date),
      IBHR = hour(start_date),
      IEHR = hour(end_date),
      ABTZ = paste0("UTC",
                    ifelse(TZ>=0,"+","-"),
                    ifelse(TZ<10,"0",""),
                    abs(TZ),"00"),
      IUTMZN = UTMZ,
      UTMHEM = UTMH,
      DGRIDKM = format(res,width=6,nsmall=3),
      XORIGKM = format(bb@xmin,nsmall=3),
      YORIGKM = format(bb@ymin,nsmall=3),
      NX = format(nx,nsmall=3),
      NY = format(ny,nsmall=3)
    )
    
    #set surface station params
    if(!is.na(surf.dat)) {
      if(!exists('station_information_strings')) {
        station_information_strings <- readLines(paste0(out_files$dir[i],"surf_stat_specs.txt"))
        gsub("!|=|\\'","",station_information_strings) -> statstr
        gsub(" +"," ",statstr) -> statstr
        gsub("^ | $","",statstr) -> statstr
        stations <- read.table(text = statstr,sep=' ',colClasses='character')
        stations <- stations[,c(2,3,4,5,6,7)]
      }
      
      params[nrow(params)+1,] <- c('NSSTA', length(station_information_strings))
      params[nrow(params)+1,] <- c('SRFDAT', paste0(out_files$dir[i],surf.dat))
    }
    
    # LC : make CALMET.INP (in CALMET dir)
    calmet_templates$noobs %>% readLines -> calmetinp
    set_puff(calmetinp, params) -> inp.out
    
    #add all m3d files
    m3d_loc = grep('M3DDAT', inp.out)
    m3d_lines = paste('MM51.DAT       input     1  ! M3DDAT =',
                      m3d_to_use,
                      ' !    !END!')
    
    inp.out = c(inp.out[1:(m3d_loc-1)],
                m3d_lines,
                inp.out[-1:(-m3d_loc)])
    
    inp_file = file.path(output_dir, paste0(grid_name, '_CALMET.INP'))
    inp.out %>% writeLines(inp_file)
    
    bat_file = file.path(output_dir, paste0(grid_name, '.bat'))
    
    paste(calmet_exe, inp_file) %>% c('pause') %>% 
      writeLines(bat_file)
    
    if(run_calmet){
      shell.exec(normalizePath(bat_file))
    }
    
    params -> params_allgrids[[grid_name]]
    start_date -> start_dates[[grid_name]]
  }
  
  # Results needed for CALPUFF
  result$run_name <- run_name
  result$grids <- grids
  result$params <- params_allgrids
  result$start_dates <- start_dates
  
  saveRDS(result, file.path(output_dir, paste0('calmet_result', '.RDS')))
  
  return(result)
}