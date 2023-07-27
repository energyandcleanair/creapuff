#process CALPUFF daily timeseries .dat files into long format .RDS files
tseries_to_rds <- function(infiles, overwrite=F) {
  outfiles <- infiles %>% gsub('\\.dat', '.RDS', ., ignore.case=T)
  queue <- !file.exists(outfiles) | overwrite
  
  for(i in seq_along(infiles)[queue]) {
    alldata <- read.table(infiles[i], skip=14)
    recnames <- paste0('R', 1:(ncol(alldata)-3))
    names(alldata) <- c('Y', 'J', 'HM', recnames)
    alldata %<>% 
      mutate(HM=HM %>% gsub('00', '', .)) %>% unite(dt, Y, J, HM, sep=' ') %>% 
      mutate(dt=as.POSIXct(dt, format='%Y %j %H'))
    
    alldata %>% pivot_longer(starts_with('R'), names_to='receptor') ->
      alldata_df
    
    
    rec <- read.table(infiles[i], skip=6, nrows=5)
    rec_df <- rec[,-1] %>% t %>% data.frame
    names(rec_df) <- c('Type', 'ix', 'iy', 'Xkm', 'Ykm')
    rec_df$receptor <- recnames
    rec_df %<>% mutate(across(c(Xkm, Ykm), as.numeric))
    
    
    infiles[i] %>% basename() %>% gsub('\\.dat', '', .) %>% strsplit('_') %>% unlist -> filedata
    
    saveRDS(list(receptors=rec_df,
                 run_info=tibble(poll=filedata[2],
                                 scenario=filedata[5]),
                 concentrations=alldata_df), outfiles[i])
    message(outfiles[i], ' written')
  }
  return(outfiles)
}

sum_tseries_rds <- function(infiles, case_name, overwrite=F) {
  outfile <- infiles[1] %>% gsub('_[^_/\\]+$', '', .) %>% paste0('_', case_name, '.RDS')
  
  if(!file.exists(outfile) | overwrite) {
    message('reading ', infiles[1])
    outdata <- readRDS(infiles[1])
    outdata$run_info$scenario <- case_name
    
    for(f in infiles[-1]) {
      message('reading ', f)
      indata <- readRDS(f)
      
      receptor_check <- all(identical(outdata$receptors, indata$receptors),
                            identical(outdata$concentrations$dt, indata$concentrations$dt),
                            identical(outdata$concentrations$receptor, indata$concentrations$receptor))
      
      if(!receptor_check) stop(paste("receptors and/or time stamps don't match between", infiles[i], f))
      outdata$concentrations$value %<>% add(indata$concentrations$value)
    }
    
    outdata %>% saveRDS(outfile)
    message(outfile, ' written')
  }
  return(outfile)
}

tseries_rds_to_raster <- function(infiles, grids, times=NULL, output_dir='.', output_case_name=NULL, overwrite=F) {
  r <- as(grids$gridR, 'SpatRaster')
  
  for(inF in infiles) {
    name_prefix = inF %>% basename() %>% gsub('\\.RDS', '', .)
    if(!is.null(output_case_name)) name_prefix %<>% gsub('_[^_\\/]+$', '', .) %>% paste0('_', output_case_name)
      
    outfiles <- name_prefix %>% 
      paste0('_', format(times, "%Y-%m-%dZ%H%M"), '.grd') %>% 
      file.path(output_dir, .)
    
    queue <- !file.exists(outfiles) | overwrite
    
    if(any(queue)) {
      message('processing ', inF)
      indata <- readRDS(inF)
      receptors <- indata$receptors %>% to_spdf(crs=crs(grids$gridR)) %>% vect
      
      if(!is.null(times)) indata$concentrations %<>% filter(dt %in% times)
      
      indata$concentrations %>% 
        merge(receptors[,'receptor'], .) ->
        point_data
      
      point_data %<>% terra::crop(r)
      
      for(i in seq_along(times)[queue]) {
        message('writing ', outfiles[i])
        if(!file.exists(outfiles[i]) | overwrite)
          point_data %>% subset(point_data$dt==times[i]) %>% 
          interpNear(r, ., 'value', radius=75, interpolate=T) %>% 
          'names<-'('layer') %>% 
          writeRaster(outfiles[i], overwrite=overwrite)
      }
    }
  }
}


read_times_from_rds <- function(infiles) {
  infiles[1] %>% readRDS %>% '[['('concentrations') %>% use_series(dt) %>% unique
}

sum_tseries_rasters <- function(infiles, case_name, times=read_times_from_rds(infiles), overwrite=F) {
  outfiles <- infiles[1] %>% basename() %>% gsub('_[a-z0-9]+\\.RDS', '', .) %>% 
    paste0('_', case_name, '_', format(times, "%Y-%m-%dZ%H%M"), '.grd') %>% 
    file.path(output_dir, .)
  
  queue <- !file.exists(outfiles) | overwrite
  
  for(i in seq_along(times)[queue]) {
    sourcefiles <- infiles %>% basename() %>% gsub('\\.RDS', '', .) %>% 
      paste0('_', format(times[i], "%Y-%m-%dZ%H%M"), '.grd') %>% 
      file.path(output_dir, .)
    
    sourcefiles %>% rast %>% sum(na.rm=T) %>% 
      'names<-'('layer') %>% 
      (function(r){r[is.na(r)]<-0;r}) %>% 
      writeRaster(outfiles[i], overwrite=overwrite)
    message(outfiles[i], ' written')
  }
}


plot_video_frames <- function(calpuff_files, 
                              plot_bb, 
                              basemap=get_basemap(plot_bb), 
                              contour_breaks=NULL, 
                              contour_break_probs=c(0,.8,.99),
                              contour_type='filled', 
                              label_contours=F, 
                              color_scale=c(crea_palettes$change[4:7], 'black'),
                              fill_alpha_function = (function(x) x^.7*1),
                              label_sources=F,
                              ...) {
  if(is.null(contour_breaks)) {
    calpuff_files$path %>% sapply(function(x) x %>% raster %>% values %>% max(na.rm=T)) -> maxvals
    maxvals %>% (function(x) which(x==max(x))) -> break_basis
    
    break_basis_raster <- calpuff_files$path[break_basis] %>% raster %>% (function(r){r[is.na(r)]<-0;r})
    
    plot_bb_in_input_crs <- projectRaster(raster(plot_bb, crs=creapuff.env$llproj), crs=break_basis_raster)
    
    break_basis_raster %>% 
      crop(plot_bb_in_input_crs) %>% 
      make_contour_breaks(probs=contour_break_probs) %>% 
      (function(x) c(x, pretty(c(last(x), quantile(maxvals, .75))))) %>% 
      signif(2) %>% 
      unique %>% 
      subset(.>0) %>% sort -> contour_breaks
  }
  
  if(is.null(calpuff_files$plot_filename)) calpuff_files %<>% mutate(plot_filename=name)
  
  calpuff_files %>% group_by(datetime) %>% 
    plot_contours(plot_bb=plot_bb,
                  basemap=basemap,
                  contour_breaks = contour_breaks,
                  contour_type=contour_type,
                  label_contours=label_contours,
                  include_threshold_as_break=F,
                  color_scale=color_scale,
                  fill_alpha_function = fill_alpha_function,
                  label_sources=label_sources,
                  ...)
  
  return(calpuff_files$plot_filename)
}


add_quotes <- function(txt) paste0('"', txt, '"')

output_video <- function(frame_files,
                         ffmpeg_exe = "C:/ffmpeg/bin/ffmpeg.exe",
                         frame_duration_secs = 0.5,
                         output_dir='.',
                         case_name='video') {
  output_file = file.path(output_dir, paste0(case_name,'.mp4'))
  
  message("Running python script")
  python_exec <- reticulate::conda_python("creapuff")
  #exec_folder <- file.path(system.file(package="creapuff"),"python")
  exec_folder <- "inst/python"
  f_mainpy <- file.path(exec_folder, "run_ffmpeg.py")
  args <- paste(add_quotes(output_file), frame_duration_secs, paste(add_quotes(frame_files), collapse=" "))
  
  command <- paste(add_quotes(python_exec), f_mainpy, args, sep = " ")
  response <- system(command, intern=T) %>% paste(collapse="")
  
  message('Running ffmpeg')
  system(paste(add_quotes(ffmpeg_exe), response))
}