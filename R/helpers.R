sel <- dplyr::select


get_ll <- function(loc){
  loc <- creahelpers::to_spdf(loc)
  if(grepl("Spatial", class(loc))) {
    if(proj4string(loc) != creapuff.env$llproj) loc <- spTransform(loc, CRS(creapuff.env$llproj))
    ll <- colMeans(coordinates(loc))
  }
  return(unname(ll))
}

get_utm_hem <- function(loc){
  ll <- get_ll(loc)
  ifelse(ll[2]<0, "s", "n")
}

get_utm_zone <- function(loc){
  ll <- get_ll(loc)
  return(floor((ll[1] + 180)/6) %% 60 + 1)
}


get_utm_proj <- function(zone=NULL, hem=NULL, loc=NULL, units="km") {
  
  if(!is.null(loc) & (!is.null(zone) | !is.null(hem)))
    warning("using explicit zone / hemisphere settings to override coordinate input")
  
  if(!is.null(loc) & is.null(zone)){
    zone <- get_utm_zone(loc)
  } 
  
  if(is.null(hem)){
    hem <- get_utm_hem(loc)
  }
  southhemi <- tolower(substr(hem,1,1)) == "s"
  
  paste0("+proj=utm +datum=WGS84 +no_defs +zone=",zone,ifelse(southhemi," +south ","")," +units=",units)
}


quickpng <- function(file, width=2000, height=1500, res=300, ...) {
  png(filename=file, width=width, height=height, res=res, ...)
}

statmode <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(table(match(x, ux)))]
}

grids_to_domains <- function(grids, target_crs){
  
  #create polygons of grid boundaries for plotting
  domPols=list()
  for(g in seq_along(grids)) {
    grids[[g]] %>% extent %>% as('SpatialPolygons') -> domPols[[g]]
    domPols[[g]]@polygons[[1]]@ID <- names(grids)[g]
  }
  
  domPols %<>% Reduce(rbind, .)
  crs(domPols) <- target_crs
  
  return(domPols)
}

textbuffer <- function(coords,width=5,steps=8) {
  theta <- seq(0, 2 * pi, length.out = steps + 1)[-1]
  coords %>%
    matrix(ncol=2) %>%
    plyr::alply(1,
                function(c.in) {
                  matrix(c(c.in[1] + width * cos(theta),
                           c.in[2] + width * sin(theta)),
                         ncol=2)
                }) %>%
    do.call(rbind, .)
}


#'Create contour polygons from a raster
#'
#' credit: StackOverflow user 'Paul Regular'
#' @param r Input raster
#' @param levels Contour levels. Numeric vector, or "auto" to set automatically (default).
#' @export
raster2contourPolys <- function(r, levels = NULL) {

    ## set-up levels
  if(levels != "auto") {
    levels <- sort(levels)
    plevels <- c(min(values(r), na.rm=TRUE), levels, max(values(r), na.rm=TRUE)) # pad with raster range
    llevels <- paste(plevels[-length(plevels)], plevels[-1], sep=" - ")
    llevels[1] <- paste("<", min(levels))
    llevels[length(llevels)] <- paste(">", max(levels))
  } else levels = NULL
  
  ## convert raster object to matrix so it can be fed into contourLines
  xmin <- extent(r)@xmin
  xmax <- extent(r)@xmax
  ymin <- extent(r)@ymin
  ymax <- extent(r)@ymax
  rx <- seq(xmin, xmax, length.out=ncol(r))
  ry <- seq(ymin, ymax, length.out=nrow(r))
  rz <- t(raster::as.matrix(r))
  rz <- rz[,ncol(rz):1] # reshape
  
  ## get contour lines and convert to SpatialLinesDataFrame
  cat("Converting to contour lines...\n")
  cl <- contourLines(rx,ry,rz,levels=levels)
  if(length(cl)==0) { warning('levels too high - no contours generated'); return(NULL) }
  
  #convert to contour lines while catching 'too short' error
  tryCatch(cl <- maptools::ContourLines2SLDF(cl),
           error = function(e) {
             if(grepl('too short',as.character(e))) {
               warning('levels too high - no contours generated'); return(NULL)
             } else stop('unknown error')
           } )
  
  
  ## extract coordinates to generate overall boundary polygon
  xy <- coordinates(r)[which(!is.na(values(r))),]
  i <- chull(xy)
  b <- xy[c(i,i[1]),]
  b <- SpatialPolygons(list(Polygons(list(Polygon(b, hole = FALSE)), "1")))
  
  ## add buffer around lines and cut boundary polygon
  cat("Converting contour lines to polygons...\n")
  bcl <- gBuffer(cl, width = 0.0001) # add small buffer so it cuts bounding poly
  cp <- gDifference(b, bcl)
  
  ## restructure and make polygon number the ID
  polys <- list()
  for(j in seq_along(cp@polygons[[1]]@Polygons)) {
    polys[[j]] <- Polygons(list(cp@polygons[[1]]@Polygons[[j]]),j)
  }
  cp <- SpatialPolygons(polys)
  cp <- SpatialPolygonsDataFrame(cp, data.frame(id=seq_along(cp)))
  
  ## cut the raster by levels
  rc <- raster::cut(r, breaks=plevels)
  
  ## loop through each polygon, create internal buffer, select points and define overlap with raster
  cat("Adding attributes to polygons...\n")
  l <- character(length(cp))
  for(j in seq_along(cp)) {
    p <- cp[cp$id==j,]
    bp <- gBuffer(p, width = -max(res(r))) # use a negative buffer to obtain internal points
    if(!is.null(bp)) {
      xy <- SpatialPoints(coordinates(bp@polygons[[1]]@Polygons[[1]]))[1]
      l[j] <- llevels[raster::extract(rc,xy)]
    }
    else {
      xy <- coordinates(gCentroid(p)) # buffer will not be calculated for smaller polygons, so grab centroid
      l[j] <- llevels[raster::extract(rc,xy)]
    }
  }
  
  ## assign level to each polygon
  cp$level <- factor(l, levels=llevels)
  cp$min <- plevels[-length(plevels)][cp$level]
  cp$max <- plevels[-1][cp$level]
  cp <- cp[!is.na(cp$level),] # discard small polygons that did not capture a raster point
  if(nrow(cp)==0) return(NULL)
  df <- unique(cp@data[,c("level","min","max")]) # to be used after holes are defined
  df <- df[order(df$min),]
  row.names(df) <- df$level
  llevels <- df$level
  
  ## define depressions in higher levels (ie holes)
  
  if(length(llevels) > 1) {
    cat("Defining holes...\n")
    spolys <- list()
    p <- cp[cp$level==llevels[1],] # add deepest layer
    p <- gUnaryUnion(p)
    spolys[[1]] <- Polygons(p@polygons[[1]]@Polygons, ID=llevels[1])
    for(i in seq(length(llevels)-1)) {
      p1 <- cp[cp$level==llevels[i+1],] # upper layer
      p2 <- cp[cp$level==llevels[i],] # lower layer
      x <- numeric(length(p2)) # grab one point from each of the deeper polygons
      y <- numeric(length(p2))
      id <- numeric(length(p2))
      for(j in seq_along(p2)) {
        xy <- coordinates(p2@polygons[[j]]@Polygons[[1]])[1,]
        x[j] <- xy[1]; y[j] <- xy[2]
        id[j] <- as.numeric(p2@polygons[[j]]@ID)
      }
      xy <- SpatialPointsDataFrame(cbind(x,y), data.frame(id=id))
      holes <- over(xy, p1)$id
      holes <- xy$id[which(!is.na(holes))]
      if(length(holes)>0) {
        p2 <- p2[p2$id %in% holes,] # keep the polygons over the shallower polygon
        p1 <- gUnaryUnion(p1) # simplify each group of polygons
        p2 <- gUnaryUnion(p2)
        p <- gDifference(p1, p2) # cut holes in p1
      } else { p <- gUnaryUnion(p1) }
      spolys[[i+1]] <- Polygons(p@polygons[[1]]@Polygons, ID=llevels[i+1]) # add level
    }
  }
  
  cp <- SpatialPolygons(spolys, pO=seq_along(llevels), proj4string=CRS(proj4string(r))) # compile into final object
  try(cpdf <- SpatialPolygonsDataFrame(cp, df,match.ID = T))
  if(!exists('cpdf')) cpdf <- SpatialPolygonsDataFrame(cp, df,match.ID = F)
  cat("Done!")
  cpdf
  
}

#' Title
#'
#' @param gridR
#' @param makeGrump
#' @param grumpPath
#'
#' @return
#' @export
#'
#' @examples
make_pop <- function(grids,
                    makeGrump=F,
                    grumpPath=NULL) {
  
  gridR <- grids$gridR
  gridLL <- grids$gridLL
  
  pop <- raster(creahelpers::get_population_path("gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif"))
  
  # if(!exists('gridLL')) gridLL <- projectRaster(gridR,crs = proj4string(countriesLow)) %>%
  #     extend(c(40,40))

  popC <- crop(pop,gridLL)
  popUTM <- projectRaster(popC,crs = CRS(proj4string(gridR)))
  popD_CP <- resample(popUTM,gridR)
  popCP <- popD_CP  * area(popD_CP)
  names(popCP) <- "pop"
  
  if(makeGrump) {
    if(is.null(grumpPath)) {
      tryPaths <- c(creahelpers::get_landcover_path('grumpv1/glurextents.bil'))
      for(tryPath in tryPaths)
        if(file.exists(tryPath)) tryPath -> grumpPath
    }
    
    if(is.null(grumpPath) | !file.exists(grumpPath))
      stop('GRUMP data not found')
    
    raster(grumpPath) -> grump
    crs(grump) <- crs(rworldmap::countriesLow)
    crop(grump,gridLL) -> grump_C
    projectRaster(grump_C,gridR,method='ngb') ->> grumpUTM
  }
  
  return(popCP)
}

get_crs_4326 <- function(){
  raster::crs(rworldmap::countriesLow)
}


get_wdpa_areas <- function(grids){
  grids$gridR %>%
    projectExtent(get_crs_4326()) %>%
    extent %>%
    magrittr::multiply_by(1.1) %>%
    as.matrix %>%
    creahelpers::get_wdpa()
}



#' Build concentration (additional) dataset from CALPUF results
#'
#' @param ext
#' @param gasunit are gas concentrations desired in ug or ppb (CALPOST outputs are assumed to be in ug/m3)
#' @param utm_hem 'N' or 'S'
#' @param map_res in kilometers
#'
#' @return
#' @export
#'
#' @examples
get_calpuff_files <- function(ext=".csv", gasunit="ug", dir=".", hg_scaling=1) {
  
  ext <- gsub("^\\.","\\\\.",ext)
  files <- list.files(path=dir, pattern=paste0("rank.*",ext), full.names = T)
  
  if(length(files)==0){
    stop("Couldn't find calpuff files (", ext, ") in ", dir)
  }
  
  calpuff_files <- data.frame(path = files, name=basename(files), scale = 1,
                              stringsAsFactors = F) %>%
    separate(name,c("X1","species","hr","type","scenario"), "_", remove=F) %>% sel(-X1)
  calpuff_files$type[calpuff_files$type=="conc"] <- "concentration"
  calpuff_files$unit <- "ug/m3"
  calpuff_files[grep("tflx",calpuff_files$name),"type"] <- "deposition"
  calpuff_files[grep("tflx",calpuff_files$name),"scale"] <- 8760*3600/1e9*1e4
  calpuff_files[grep("tflx",calpuff_files$name),"unit"] <- "kg/ha/yr"
  
  calpuff_files[calpuff_files$species == 'hg','scale'] %<>% multiply_by(1e6 * hg_scaling)
  message(paste0('mercury scaling: ', hg_scaling, '. Enter 1e-3 if you input Hg in t in CALPUFF.'))
  calpuff_files[calpuff_files$species == 'hg','unit'] <- "mg/ha/yr"
  
  calpuff_files$hr <- as.numeric(gsub("[_hr]","",calpuff_files$hr))
  calpuff_files$FUN <- "mean"
  calpuff_files[calpuff_files$hr<=24,"FUN"] <- "max"
  calpuff_files$scenario <- gsub(ext,"",calpuff_files$scenario)
  
  calpuff_files$period <- NA
  calpuff_files[calpuff_files$hr ==1,"period"] <- "hourly"
  calpuff_files[calpuff_files$hr ==24,"period"] <- "daily"
  calpuff_files[calpuff_files$hr > 7000,"period"] <- "annual"
  
  calpuff_files$speciesName <- toupper(calpuff_files$species)
  calpuff_files[calpuff_files$species == "so2eq","speciesName"] <- "acid"
  calpuff_files[calpuff_files$species == "pm25","speciesName"] <- "PM2.5"
  calpuff_files[calpuff_files$species == "tpm10","speciesName"] <- "PM10"
  calpuff_files[calpuff_files$species == "so2eq","unit"] <- paste0(calpuff_files[calpuff_files$species == "so2eq","unit"]," SO2-equivalent")
  calpuff_files[calpuff_files$species == "pm","speciesName"] <- "fly ash"
  calpuff_files[calpuff_files$species == "hg","speciesName"] <- "mercury"
  
  #scaling for non-standard units
  calpuff_files$plotscale <- 1
  calpuff_files$plotunit <- calpuff_files$unit
  
  if(gasunit=='ppb') {
    calpuff_files[calpuff_files$speciesName=="SO2","plotscale"] <- 0.355
    calpuff_files[calpuff_files$speciesName=="NO2","plotscale"] <- 0.494
    calpuff_files[calpuff_files$speciesName=="SO2","plotunit"] <- 'ppb'
    calpuff_files[calpuff_files$speciesName=="NO2","plotunit"] <- 'ppb'
  }
  
  # Exceedance thresholds - these will be recorded and included as a threshold level in contour plots
  # WHO-2021 : https://www.who.int/news-room/fact-sheets/detail/ambient-(outdoor)-air-quality-and-health
  # NAAQS : https://www.epa.gov/criteria-air-pollutants/naaqs-table
  calpuff_files$threshold <- NA
  calpuff_files[calpuff_files$speciesName=="PM2.5" & calpuff_files$hr==24,"threshold"]               <- 15  # WHO-2021 [ug/m3]
  calpuff_files[calpuff_files$speciesName=="PM2.5" & calpuff_files$hr > 7000,"threshold"]            <- 5   # WHO-2021
  calpuff_files[calpuff_files$speciesName=="PM10" & calpuff_files$hr==24,"threshold"]                <- 45  # WHO-2021
  calpuff_files[calpuff_files$speciesName=="PM10" & calpuff_files$hr > 7000,"threshold"]             <- 5   # WHO-2021
  calpuff_files[calpuff_files$speciesName=="NO2" & calpuff_files$hr==1,"threshold"]                  <- 100*46.01*0.0409 ## U.S. NAAQS at 1atm, 25°C (EPA)  # 98th percentile of 1-hour daily maximum concentrations, averaged over 3 years
  calpuff_files[calpuff_files$speciesName=="NO2" & calpuff_files$hr==24,"threshold"]                 <- 25  # WHO-2021
  calpuff_files[calpuff_files$speciesName=="NO2" & calpuff_files$hr > 7000,"threshold"]              <- 10  # WHO-2021
  calpuff_files[calpuff_files$speciesName=="SO2" & calpuff_files$hr==1,"threshold"]                  <- 75*64.06*0.0409 # U.S. NAAQS at 1atm, 25°C (EPA)  # 99th percentile of 1-hour daily maximum concentrations, averaged over 3 years
  calpuff_files[calpuff_files$speciesName=="SO2" & calpuff_files$hr==24,"threshold"]                 <- 40  # WHO-2021
  calpuff_files[calpuff_files$speciesName=="mercury" & calpuff_files$type=="deposition","threshold"] <- 125 # Great lakes study
  calpuff_files$threshold.plotunit <- calpuff_files$threshold * calpuff_files$plotscale
  print("WHO-2021 standards")

  # Indonesian standards
  # calpuff_files$threshold <- NA
  # calpuff_files[calpuff_files$speciesName=="SO2" & calpuff_files$hr==1,"threshold"]       <- 150
  # calpuff_files[calpuff_files$speciesName=="SO2" & calpuff_files$hr==24,"threshold"]      <- 75
  # calpuff_files[calpuff_files$speciesName=="SO2" & calpuff_files$hr>7000,"threshold"]     <- 45
  # calpuff_files[calpuff_files$speciesName=="NO2" & calpuff_files$hr==1,"threshold"]       <- 200
  # calpuff_files[calpuff_files$speciesName=="NO2" & calpuff_files$hr==24,"threshold"]      <- 65
  # calpuff_files[calpuff_files$speciesName=="NO2" & calpuff_files$hr>7000,"threshold"]     <- 50  
  # calpuff_files[calpuff_files$speciesName=="PM10" & calpuff_files$hr==24,"threshold"]     <- 75
  # calpuff_files[calpuff_files$speciesName=="PM10" & calpuff_files$hr > 7000,"threshold"]  <- 40
  # calpuff_files[calpuff_files$speciesName=="PM2.5" & calpuff_files$hr==24,"threshold"]    <- 55
  # calpuff_files[calpuff_files$speciesName=="PM2.5" & calpuff_files$hr > 7000,"threshold"] <- 15
  # calpuff_files$threshold.plotunit <- calpuff_files$threshold * calpuff_files$plotscale
  # print("Indonesian standards")
  
  # Old WHO standards
  # calpuff_files$threshold <- NA
  # calpuff_files[calpuff_files$speciesName=="SO2" & calpuff_files$hr==24,"threshold"]                 <- 20  # WHO-2005
  # calpuff_files[calpuff_files$speciesName=="NO2" & calpuff_files$hr==1,"threshold"]                  <- 200 # WHO-2005
  # calpuff_files[calpuff_files$speciesName=="PM2.5" & calpuff_files$hr==24,"threshold"]               <- 25  # WHO-2005
  # calpuff_files[calpuff_files$speciesName=="PM10" & calpuff_files$hr==24,"threshold"]                <- 50  # WHO-2005
  # calpuff_files[calpuff_files$speciesName=="mercury" & calpuff_files$type=="deposition","threshold"] <- 125 # Great lakes study
  # calpuff_files[calpuff_files$speciesName=="SO2" & calpuff_files$hr==1,"threshold"]                  <- 75/0.355 # U.S. NAAQS  at 1atm, 0°C 
  # calpuff_files$threshold.plotunit <- calpuff_files$threshold * calpuff_files$plotscale
  # print("Old WHO standards")
  
  return(calpuff_files)
}


get_grids_calpuff <- function(calpuff_files,
                              utm_zone=NULL,
                              utm_hem=NULL,
                              map_res_km=NULL,
                              filepath=NULL) {
  
  if(is.null(filepath)) filepath <- calpuff_files$path[1]
  
  if(grepl('\\.tif$', filepath)) {
    gridR <- filepath %>% raster %>% raster %>% fixproj()
    gridSP <- NULL
  } else {
    if(is.null(utm_zone)) utm_zone=get('utm_zone', envir=.GlobalEnv)
    if(is.null(utm_hem)) utm_hem=get('utm_hem', envir=.GlobalEnv)
    if(is.null(map_res_km)) map_res_km=get('map_res_km', envir=.GlobalEnv)
    
    poll <- read.table(filepath,
                       skip=7,header=F,col.names=c("Xkm","Ykm","PM25"), sep=",")
    
    pollSP <- SpatialPointsDataFrame(coords = subset(poll,select=c(Xkm,Ykm)),
                                     data = subset(poll,select=-c(Xkm,Ykm)),
                                     proj4string = CRS(paste0("+proj=utm +zone=",utm_zone,
                                                              ifelse(utm_hem=="S"," +south",""),
                                                              " +datum=WGS84 +units=km +no_defs")))
    domain <- extent(pollSP)
    res <- (domain@xmax - domain@xmin)/49
    domain <- extend(domain,res/2)
    
    r <- raster(domain, resolution=map_res_km, crs=crs(pollSP))
    
    gridSP <- as(r, 'SpatialPixels') #CHECK We removed global variable
    gridR <- raster(gridSP)
  }
  
  gridLL <- projectRaster(gridR, crs = proj4string(rworldmap::countriesLow))
  gridLL <- extend(gridLL, c(40,40))
  
  return(list("gridR"=gridR,
              "gridSP"=gridSP,
              "gridLL"=gridLL))
}

#wrapper for readPuffInp that returns the CRS and grid properties
get_grid_from_calpuff.inp <- function(scenarios=NULL,
                                      dir="C:/CALPUFF/CALPUFF_v7.2.1_L150618",
                                      filename_suffix="_CALPUFF_7.0.inp",
                                      file_paths=file.path(dir, paste0(scenarios, filename_suffix)),
                                      params_to_read = c('IUTMZN','UTMHEM','XORIGKM','YORIGKM','DGRIDKM','NX','NY'),
                                      ...) {
  if(is.null(scenarios)) scenarios <- file_path %>% basename %>% gsub(filename_suffix, '', .)
  file_paths %>%
    lapply(creapuff::readPuffInp, ...) %>%
    lapply('[', params_to_read) %>%
    lapply(data.frame) %>% bind_rows %>% tibble(scenario=scenarios, .)
}



#' Title
#'
#' @param calpuff_files
#' @param grids
#' @param ext
#' @param queue
#' @param subsets
#' @param max.ranks
#' @param overwrite
#' @param nmax
#' @param idp
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
make_tifs <- function(calpuff_files,
                      grids,
                      ext='',
                      queue=NULL,
                      subsets = F,
                      max.ranks = 1,
                      overwrite=F,
                      nmax=8, idp=1.5, ...) {
  
  if(is.null(queue)) queue = 1:nrow(calpuff_files)
  
  files = calpuff_files$path
  
  if(subsets %>% typeof == "logical") {
    if(subsets) { subsets <- c('_g','_d')
    } else subsets <- ''
  }
  
  queue <- queue[queue %in% grep(subsets[1], calpuff_files$path)]
  
  for(file in queue) {
    ranks <- 1
    if(grepl('rank\\(all)', files[file])) ranks <- max.ranks
    for(rank.n in 1:ranks) {
      files[file] %>% gsub("\\.csv",paste0(ext, ".tif"),.) %>%
        gsub(subsets[1],"",.) %>%
        gsub('rank\\(all)', paste0('rank(',rank.n,')'), .) -> rfile
      
      if(file.exists(rfile)) message('tif file exists: ', rfile, ifelse(overwrite, ". [OVERWRITING]", ". [IGNORING]"))
      if(!file.exists(rfile) | overwrite) {
        inF <- sapply(subsets, function(x) gsub(subsets[1],x,files[file]))
        
        #create interpolated raster
        inF %>% lapply(read_calpost) %>% do.call(rbind, .) -> poll
        poll[, c(1:2, 2+rank.n)] -> poll
        colnames(poll) = c("Xkm","Ykm","conc")
        poll$conc <- calpuff_files[file,"scale"] * poll$conc
        
        pollSP <- SpatialPointsDataFrame(coords = subset(poll,select=c(Xkm,Ykm)),
                                         data = subset(poll,select=-c(Xkm,Ykm)),
                                         proj4string = CRS(proj4string(grids$gridSP)))
        pollSP %<>% crop(extent(grids$gridR)+30.1)
        
        conc_idw <- idw(as.formula(paste0("conc"," ~ 1")),
                        pollSP, grids$gridSP, nmax=nmax, idp=idp, ...)
        conc_R <- raster(conc_idw,values=T)
        conc_R %<>% crop(grids$gridR)
        
        writeRaster(conc_R, rfile, format="GTiff",overwrite=T)
        raster::plot(conc_R, main=basename(rfile))
        print(paste(files[file],'processed'))
      }
    }
  }
}

get_conc_raster <- function(calpuff_files, scenario, species, period='annual') {
  calpuff_files %>% filter(scenario==!!scenario &
                             species==!!species &
                             period==!!period) %>%
    pull(path) %>%
    gsub("\\.csv","\\.tif",.) %>%
    raster() %>%
    creahelpers::fixproj()
}


#read coords and timezones from .def files
readDef = function(files_def) {
  cbind(files_def,Lat=NA,Lon=NA,TZ=NA,GridNX=NA,GridNY=NA,
        UTMZ=NA,UTMH=NA,StartDate=NA,EndDate=NA) -> files_def
  
  for(i in 1:nrow(files_def)) {
    def_con <- file(as.character(files_def[i,"file_def"]), open="rt")
    readLines(def_con) -> indef
    files_def[i,c("Lat","Lon")] <- as.numeric(indef[c(4,5)])
    files_def[i,c("StartDate","EndDate")] <- indef[c(8,9)]
    files_def[i,c("TZ")] <- round(as.numeric(indef[10]),0)
    files_def[i,c("GridNX","GridNY")] <- as.numeric(indef[c(25,26)])
    close(def_con)
  }
}


#find all .def files and .out files in the run directory and subdirectories
get_out_files <- function(runDir, out_file_ext, batch_subset=NULL) {
  out_files <- data.frame(file_out = gsub("//","/",list.files(path=runDir,pattern=paste0("*",out_file_ext,"$"),recursive=T,full.names=T,include.dirs=T)))
  files_def <- data.frame(file_def = gsub("//","/",list.files(path=runDir,pattern="*.def",recursive=T,full.names=T,include.dirs=T)))
  
  out_files <- cbind(out_files,
                     run_name = out_files$file_out %>% 
                       gsub("[a-zA-Z0-9_:]*/+","", .) %>% 
                       get_run_name,
                     grid_name = gsub(out_file_ext,"",gsub("[a-zA-Z0-9_:]*/+","",out_files$file_out)),
                     dir = gsub(paste0("[a-zA-Z0-9_]*",out_file_ext),"",out_files$file_out))
  files_def$run_name = files_def$file_def %>% 
    gsub("[a-zA-Z0-9_:]*/+","", .) %>% get_run_name
  
  files_def %<>% subset(!duplicated(run_name))
  
  if(!is.null(batch_subset)) out_files %<>% subset(run_name %in% batch_subset)
  
  #read coords and timezones from .def files
  files_def %<>% readDef
  out_files %>% join(files_def,by="run_name",match="first")
}


#function to set parameters in a CALPUFF input file
set_puff <- function(inpfile, paramdf, set.all=T) {
  if(!is.data.frame(paramdf))
    paramdf %<>% unlist %>% data.frame(name = names(.), val= .)
  for(param in paramdf$name) {
    val.count = sum(paramdf$name==param)
    ln = grep(paste0('!',param,'='),gsub(' ','',inpfile))
    if(length(ln) < val.count) {
      missing.count <- val.count - length(ln)
      ln %<>% c(grep(paste0('\\*',param,'='),gsub(' ','',inpfile))[1:missing.count])
      inpfile[ln] %<>% gsub("\\*", "!", .)
    }
    
    
    if(any(is.na(paramdf[paramdf$name==param,'val']))) stop(paste('parameter ',param,' has value NA!'))
    
    if(length(ln) == val.count) {
      if(paramdf[paramdf$name==param,'val'] != "not set") {
        for(i in 1:length(ln)) {
          m <- regexpr(paste0('=[^!]*!'),inpfile[ln][i])
          regmatches(inpfile[ln][i],m) <-  paste0('= ',paramdf[paramdf$name==param,'val'][i],' !')
        }
      } else inpfile[ln] <- gsub('!','*',inpfile[ln])
    } else {
      msg = paste('parameter ',param,' not set, found ',length(ln),' matches!')
      if(set.all) { stop(msg)
      } else { warning(msg) }
    }
  }
  return(inpfile)
}

TZstring <- function(x) paste0("UTC", 
                               ifelse(x>=0, "+", "-"),
                               stringr::str_pad(abs(x), 2, "left", "0"), "00")


write_input <- function(file_template, file_out, params, ...) {
  readLines(file_template) -> pu.inp
  set_puff(pu.inp, params, ...) -> pu.inp
  outcon <- file(file_out, "w")
  writeLines(pu.inp, outcon)
  close(outcon)
}


make_params <- function(out_files) {
  #define parameters to set in CALPUFF.INP
  
  #start and end times
  paramlist <- 
    list(
      #times
      IBYR = substr(out_files$StartDate, 1, 4),
      IBMO = substr(out_files$StartDate, 5, 6),
      IBDY=substr(out_files$StartDate, 7, 8),
      IBHR=substr(out_files$StartDate, 9, 10),
      IEYR=substr(out_files$EndDate, 1, 4),
      IEMO=substr(out_files$EndDate, 5, 6),
      IEDY=substr(out_files$EndDate, 7, 8),
      IEHR=substr(out_files$EndDate, 9, 10),
      
      #timezone
      ABTZ=TZstring(out_files$TZ),
      
      #grid settings
      IUTMZN=out_files$UTMZ,
      UTMHEM=out_files$UTMH,
      DGRIDKM=out_files$GridD,
      NX=out_files$GridNX,
      NY=out_files$GridNY,
      IECOMP=out_files$GridNX,
      JECOMP=out_files$GridNY,
      IESAMP=out_files$GridNX,
      JESAMP=out_files$GridNY,
      XORIGKM=out_files$GridX,
      YORIGKM=out_files$GridY)
  
  set_param(name=paramlist)
}


set_param <- function(df=NULL, name, value=NULL) {
  if(is.null(df)) df = data.frame(name=NA, val=NA)[F, ]
  if(!is.list(name)) {
    name = as.list(value)
    names(name) <- name
  }
  
  for(i in 1:length(name)) {
    param = names(name)[i]
    value = name[[i]]
    if(param %in% df$name) {
      df[df$name==param, 'val'] <- value
    } else df[nrow(df)+1, ] <- c(param,value)
  }
  return(df)
}


get_param_val <- function(parname, inp, max.number=1) {
  inp %<>% gsub(" ", "", .)
  lineN <- grep(paste0('!',parname,'=.*!'), inp) %>% head(max.number)
  inp[lineN] %>% 
    gsub("!END!", "", .) %>% 
    gsub(".*=", "", .) %>% 
    gsub("!", "", .) %>% 
    gsub("^ *", "", .) %>% gsub(" *$", "", .)
}


#read information from cALPUFF.INP files
readPuffInp <- function(file,
                        inparams = c('IBYR', 'IEYR', 
                                     'BCKNH3', 'BCKO3', 'BCKH2O2',
                                     'SRCNAM', 'IUTMZN',
                                     'UTMHEM', 'ABTZ', 'CONDAT', 'DFDAT', 'WFDAT', 'METDAT1',
                                     'XORIGKM', 'YORIGKM', 'DGRIDKM', 'NX', 'NY', 'CSPEC'),
                        additional_params = NULL, #additional parameters to read, can be used to add params without re-listing the default ones
                        get_emissions = T)  { #return a data.frame of modeled emissions
  
  file %>% readLines %>% gsub(" ", "", .) -> inp
  
  inparams <- unique(c(inparams, additional_params))
  inparams %>% lapply(get_param_val, inp, max.number=Inf) %>% set_names(inparams) -> d
  
  d[['datadir']] <- dirname(d[['CONDAT']]) %>% paste0('/')
  
  if(get_emissions) {
    nspec <- length(d[["CSPEC"]])
    npt1 <- get_param_val("NPT1", inp)
    get_param_val("X", inp, max.number=npt1) -> d[["X"]]
    d[["X"]] %>% lapply(function(x) x %>% strsplit(',') %>% unlist %>% as.numeric()) -> emis
    emis %>% lapply(function(e) e[(length(e) - nspec + 1):length(e)]) %>% 
      lapply(function(e) data.frame(species = d[["CSPEC"]],
                                    modeled = e)) -> d[["emissions"]]
    names(d[["emissions"]]) <- d[['SRCNAM']]
  }    
  
  return(d)
}


#remove discrete receptors and emissions sources from calpuff INP
clean_inp <- function(calpuff_inp) {
  grep('^Subgroup \\(13b\\)', calpuff_inp) -> psst
  grep('^Subgroup \\(13c\\)', calpuff_inp) -> psend
  grep('!', calpuff_inp) %>% subset(. %in% psst:psend) -> psln
  if(length(psln)>0) calpuff_inp[-psln] -> calpuff_inp
  
  grep('^Subgroup \\(20c\\)', calpuff_inp) -> drheader
  grep('!', calpuff_inp) %>% subset(. > drheader) -> drln
  if(length(drln)>0) calpuff_inp[-drln] -> calpuff_inp
  return(calpuff_inp)
}


make_calpuff_inp <- function(files_met,
                             calpuff_template,
                             output_dir=unique(dirname(files_met$METDAT)),
                             puffrun=NULL,
                             source_lines=NULL,
                             receptors=NULL,
                             OZONE.DAT=NULL,
                             # bgconcs=lapply(list(O3=rep(25, 12),NH3=rep(10, 12),H2O2=rep(1, 12)), paste, collapse=','),
                             bgconcs=NULL,
                             addparams=list(),
                             addsubgroups=list()) {
  
  
  
  if(is.null(puffrun)) puffrun=files_met$run_name[1]
  
  #read CALPUFF.INP
  calpuff_inp <- readLines(calpuff_template)
  
  file_out <- file.path(output_dir, puffrun) %>% gsub('//', '/', .)
  
  files_met %<>% arrange(desc(GridD))
  metrun=files_met$run_name[1] %>% as.character()
  
  
  params <- files_met %>% head(1) %>% make_params()
  
  #additional parameters
  addparams$NREC = length(receptors)     # Number of receptors
  addparams$NPT1 = length(source_lines)  # Number of point sources with constant stack parameters
  
  #met domains and data files
  print(paste0("Number of domains is ", nrow(files_met)))
  addparams$NMETDOM = nrow(files_met)
  addparams$NMETDAT = nrow(files_met)
  
  if(nrow(files_met) == 1)
    addparams$METDAT = file.path(output_dir, paste0(files_met$grid_name,"_CALMET.DAT")) %>% gsub('//', '/', .)
  
  if(is.null(files_met$grid_name)) stop('grid_name cannot be NULL')
  for(r in 1: max( nrow(files_met),5)) {  # LC
    gridLevelAvailable <- (!is.na(files_met[r,"grid_name"]) & nrow(files_met)>1)
    addparams[[paste0('DOMAIN',r)]] <-
      ifelse(gridLevelAvailable,
             as.character(files_met[r,"grid_name"]),
             "not set")
    
    addparams[[paste0('METDAT',r)]] <- 
      ifelse(gridLevelAvailable,
             file.path(output_dir, #files_met[r, "dir"],  # LC
                       paste0(files_met[r,"grid_name"],"_CALMET.DAT")) %>% 
               gsub('//', '/', .),
             "not set")
  }
  
  #output file names
  addparams$CONDAT = paste0(file_out,".CON")
  addparams$DFDAT = paste0(file_out,".DRY")
  addparams$WFDAT = paste0(file_out,".WET")
  addparams$VISDAT = paste0(file_out,".VIS")
  addparams$BALDAT = paste0(file_out,".BAL")
  addparams$PUFLST = paste0(file_out,"_CALPUFF.LST")
  
  #background concentrations
  addparams$MOZ = ifelse(!is.null(OZONE.DAT), 1, 0)  # 0 = use a monthly background ozone value; 1 = read hourly ozone concentrations from OZONE.DAT data file
  addparams$MNH3 = 0
  addparams$MH2O2 = 0
  if (is.null(bgconcs)) lapply(list(O3=rep(25, 12),NH3=rep(10, 12),H2O2=rep(1, 12)), paste, collapse=',') -> bgconcs
  addparams$BCKO3 = ifelse(!is.null(OZONE.DAT), "not set", bgconcs$O3)
  addparams$BCKNH3 = bgconcs[["NH3"]]
  if(is.null(bgconcs[["H2O2"]])) bgconcs[["H2O2"]]=bgconcs[["H2O_"]]
  addparams$BCKH2O2 = bgconcs[["H2O2"]]
  addparams$OZDAT = ifelse(!is.null(OZONE.DAT), file.path(metdir, OZONE.DAT), "not set")
  
  #set parameter values
  set_param(params, addparams) -> params
  set_puff(calpuff_inp,params) -> calpuff_inp
  
  #remove discrete receptors and emissions sources from calpuff INP
  calpuff_inp %<>% clean_inp()
  
  if(!is.null(source_lines)) addsubgroups %<>% c(list(X13b = source_lines))
  if(!is.null(receptors))   addsubgroups %<>% c(list(X20c = receptors))
  
  for(sg in names(addsubgroups))
    calpuff_inp %<>% add_subgroup_lines(addsubgroups[[sg]], subgroup=gsub("^X", "", sg))
  
  #write into file
  outF <- file.path(output_dir, paste0(puffrun,"_CALPUFF_7.0.inp"))
  if(is.list(calpuff_inp)) calpuff_inp %<>% unlist()  # LC
  writeLines(calpuff_inp, outF)  
  
  return(outF)
}


read_geo <- function(geoPath) {
  gridData <- data.frame("GridNX"=NA,"GridNY"=NA,"GridX"=NA,"GridY"=NA,"GridDX"=NA,"GridDY"=NA)
  gridData[1,] <- scan(geoPath,nmax=6,what=numeric(),skip=5, nlines=1)
  UTMzh <- gsub(" ","",readLines(geoPath,n=4)[[4]])
  UTMz <- as.numeric(gsub("[A-Z]","",UTMzh))
  UTMh <- gsub("[0-9]{1-2}","",UTMzh)
  
  geoPath %>% readLines %>% gsub("^ ", "", .) %>% textConnection() %>% 
    read.table(sep=" ",skip=(gridData$GridNY + 9),nrow=gridData$GridNY,header=F) -> topo
  colnames(topo) <- 1:gridData$GridNX
  cbind(Y=gridData$GridNY:1,topo) -> topo 
  
  
  topoXYZ <- reshape2::melt(topo,id.vars="Y",variable.name = "X",value.name = "alt")
  cbind(topoXYZ,
        Xkm=gridData$GridX + (as.numeric(topoXYZ$X) - .5)*gridData$GridDX,
        Ykm=gridData$GridY + (as.numeric(topoXYZ$Y) - .5)*gridData$GridDY) -> topoXYZ
  
  topoR <- topoXYZ[,c("Xkm","Ykm","alt")]
  coordinates(topoR) <- ~ Xkm + Ykm
  gridded(topoR) <- T
  topoR <- raster(topoR)
  crsobj <- CRS(paste0("+proj=utm +zone=",UTMz,ifelse(UTMh=="S"," +south",""),
                       " +datum=WGS84 +units=km +no_defs"))
  crs(topoR) <- crsobj
  return(topoR)
}


get_plant_elev <- function(sources.sp, files_met, dir=unique(files_met$dir)) {
  files_met %>% arrange(GridD) %>%
    mutate(path = file.path(dir, paste0(grid_name,".geo"))) %>%
    magrittr::use_series(path) %>% 
    lapply(read_geo) -> topoR
  sources.sp %<>% spTransform(crs(topoR[[1]]))
  topoR %>% lapply(extract, sources.sp) %>% data.frame -> elevs
  elevs %>% apply(1, function(x) x[!is.na(x)][1])
}


add_subgroup_lines <- function(inp, lines, subgroup, skip_lines=NULL) {
  if(!grepl("Subgroup", subgroup)) subgroup %<>% paste0("Subgroup (",.,")")
  header_ln = grep(subgroup,inp, fixed=T)
  if(is.null(skip_lines)) {
    end_ln = grep("^-{7,}$",inp) %>% subset(.>header_ln+1) %>% head(1)
    ln=end_ln-2
  } else ln = header_ln + skip_lines + 1
  
  c(inp[1:(ln-1)],
    lines,
    inp[(ln+1):length(inp)])
}



get_recep <- function(loc,
                      run_name,
                      files_met,
                      nesting_factors=c(3, 10, 30),
                      output_dir=unique(dirname(files_met$METDAT)),
                      calpuff_exe='C:/CALPUFF/CALPUFF_v7.2.1_L150618/calpuff_v7.2.1.exe',
                      calpuff_template=system.file("extdata", "CALPUFF_7.0_template_Hg.INP", package="creapuff"),
                      target_crs) {

  files_met %<>% arrange(desc(GridD))
  calmetRes <- files_met$GridD[1] #resolution of the CALMET grid, in km
  calmetXY <- c(X=files_met$GridX[1],Y=files_met$GridY[1]) #origin (LL corner) of CALMET grid
  GridNX <- files_met$GridNX[1]
  GridNY <- files_met$GridNY[1]
  
  cluster.center <- loc %>% coordinates %>% data.frame %>% set_names(c("X", "Y"))
  
  for(nesfact in nesting_factors) {
    #calculate source position in CALMET grid
    sourceij <- ceiling((cluster.center - calmetXY) / calmetRes) %>% data.frame
    
    #is the source in the lower left corner of the cell?
    sourceLL <- ((cluster.center - calmetXY) %% calmetRes < calmetRes/2) %>% data.frame
    
    #calculate sampling grid extent (400 is the max dimension of receptor grid)
    rng <- floor(400 / nesfact)
    IBSAMP=sourceij["X"] - round(rng/2,0) - ifelse(sourceLL["X"],1,0)
    JBSAMP=unlist(sourceij["Y"] - round(rng/2,0) - ifelse(sourceLL["Y"],1,0)) %>% max(1) %>% min()
    IBSAMP %<>% max(1)
    JBSAMP %<>% max(1)
    
    #read CALPUFF.INP template
    files_met %>% 
      make_calpuff_inp(puffrun=run_name,
                       calpuff_template=calpuff_template,
                       output_dir=output_dir) %>% 
      readLines -> calpuff_inp
    
    
    params <- list(NREC = 0, #no non-gridded receptors
                   #set sampling grid boundaries based on rng parameter but making sure not to go outside of the domain
                   IBSAMP=IBSAMP,JBSAMP=JBSAMP,
                   IESAMP=min(IBSAMP+rng-1, GridNX),
                   JESAMP=min(JBSAMP+rng-1, GridNY),
                   DATUM='WGS-84',
                   MESHDN=nesfact,
                   ITEST=1,  # STOPS program after SETUP phase
                   NPT1=0)  # No emission sources
    
    #remove discrete receptors and emissions sources from calpuff INP
    grep('^Subgroup \\(13b\\)', calpuff_inp) -> psst
    grep('^Subgroup \\(13c\\)', calpuff_inp) -> psend
    grep('!', calpuff_inp) %>% subset(. %in% psst:psend) -> psln
    if(length(psln) > 0) calpuff_inp[-psln] -> calpuff_inp
    
    grep('^Subgroup \\(20c\\)', calpuff_inp) -> drheader
    grep('!', calpuff_inp) %>% subset(. > drheader) -> drln
    if(length(drln) > 0) calpuff_inp[-drln] -> calpuff_inp
    
    set_puff(calpuff_inp, params) -> calpuff_inp
    
    #write into file
    calpuff_dir <- dirname(calpuff_exe)
    outinp <- file.path(output_dir, paste0(run_name,"_elevgen_CALPUFF_7.0.inp"))
    writeLines(calpuff_inp, outinp)  
    
    org_dir <- getwd()
    setwd(output_dir)
    
    #run CALPUFF setup to generate receptor grid  
    system2(calpuff_exe, args=outinp) -> exit_code
    if(exit_code != 0) stop("errors in CALPUFF execution")
    
    #rename receptor file and move to input directory
    file.rename("qarecg.dat", 
                paste(run_name,'nesfact',nesfact,"qarecg.dat",sep="_"))
    
    setwd(org_dir)
  }
  
  
  ##read the receptor file written by CALPUFF and generate nested grids of discrete receptors
  
  #read in and merge files generated above
  nesting_factors %>% 
    lapply(function(nf) {
      file.path(output_dir, paste(run_name,'nesfact',nf,"qarecg.dat",sep="_")) %>%
        read.table(stringsAsFactors = F,header=T) %>% 
        mutate(nesfact=nf)
    }) %>% bind_rows %>% to_spdf(crs=target_crs)
}

select_receptors <- function(receptors, run_name='CALPUFF', sources, nesting_factors, nesfact_range, files_met) {
  if(is.list(receptors)) receptors %<>% do.call(rbind, .)
  receptors %<>% subset(!duplicated(coordinates(.)))
  r=raster(extent(receptors), res=1, crs=crs(receptors))
  
  sources$flag=1
  sourcesR=sources %>% to_spdf %>% spTransform(crs(r)) %>% rasterize(r, 'flag')
  dist_to_source=distance(sourcesR)
  extract(dist_to_source, receptors) -> receptors$dist_to_source
  
  receptors$include=F
  
  for(i in seq_along(nesting_factors))
    receptors$include[receptors$dist_to_source<nesfact_range[i] & receptors$nesfact==nesting_factors[i]] <- T
  
  print(paste(run_name, sum(receptors$include), 'receptors'))
  # if(sum(receptors$include)+files_met$GridNX[1]*files_met$GridNY[1]>=10000*2) stop('too many receptors!')  # LC : *2
  if(sum(receptors$include)>=10000) stop('too many receptors!')  # LC 
  
  plotadm = creahelpers::get_adm(0, 'coarse') %>% cropProj(r)
  quickpng(file.path(output_dir, paste0(run_name, '_', 'receptors.png'))  )
  receptors %>% subset(include) %>% sp::plot(col='gray', cex=.5)
  plotadm %>% sp::plot(add=T, border='steelblue')  
  
  sources %>% sp::plot(add=T, lwd=3)
  dev.off()
  
  return(receptors)
}

make_topo_rows <- function(topoXYZ) {
  paste0("Disc",1:nrow(topoXYZ),
         "  !  X = ",
         topoXYZ$Xkm,", ",
         topoXYZ$Ykm,", ",
         format(round(topoXYZ$Elev,1),nsmall=1, scientific=F), 
         " ! ! END !")
}


get_bg_concs = function(locs, mod_dir=file.path(get_gis_dir(), "background")) {
  if(is.null(locs$ID)) locs$ID <- 1:nrow(locs)
  locs %<>% to_spdf
  #retrieve concentrations from Asia nested runs
  #file names
  files_bg <- list("Max_8-hour_ozone" = "present_mda8.nc",
                   NH3 = "nested_nh3_present.2011.nc",
                   H2O_ = "nested_h2o2_present.2011.nc")
  scaling <- c(1, 1, 1) #source data is in ppb
  
  
  #grid specs
  xmin=70
  xmax=150
  xres=2/3
  ymin=-11
  ymax=55
  yres=.5
  
  concs <- list()
  avgconcs <- list()
  
  for(i in 1:length(files_bg)) {
    nc_open(file.path(mod_dir, files_bg[[i]])) -> nc
    ncvar_get(nc, names(files_bg)[i]) -> d
    aperm(d, c(2, 3, 1)) -> d
    datamonths = as.Date("2011-01-01") %>% 
      seq.Date(by="day",length.out = 365) %>% 
      month %>% list
    
    # dim(d) = 121 133 365
    apply(d, c(1, 2), 
          function(x) aggregate(x, by=datamonths, mean)$x) -> d.m
    
    # dim(d.m) = 12 123 133
    #CHECK HT: This is the line I had to add
    aperm(d.m, c(2, 3, 1)) -> d.m
    # dim(d.m) = 121 133  12
    
    brick(d.m) -> r
    extent(r) <- extent(c(xmin-xres/2,
                          xmax+xres/2,
                          ymin-yres/2,
                          ymax+yres/2))
    
    raster::extract(r, locs) -> conc
    
    conc %>% 
      multiply_by(scaling[i]) %>% 
      round(3) %>% 
      apply(1, paste, collapse=", ") -> 
      concs[[names(files_bg)[i]]]
  }
  
  #read data from global grid for locations outside Asia
  concs[[1]] %>% grep("NA", .) -> miss
  
  spec = c('O3', 'NH3', 'H2O2')
  names(concs) <- spec
  
  if(length(miss)>0) {
    file.path(mod_dir, 'Geos-Chem_v8-02-04-geos5-Run2_bgconcs.grd') %>% 
      stack %>% raster::extract(locs[miss, ]) %>% 
      data.frame(locs@data[miss, ], .) %>% 
      gather(var, val, contains("_M")) %>% 
      separate('var', c('spec', 'M')) %>% 
      spread(M, val) -> globconcs
    
    globconcs %<>% full_join(data.frame(spec=spec, scaling=scaling))
    
    
    globconcs[, paste0('M', 1:12)] %>% 
      multiply_by(globconcs$scaling) %>% 
      round(3) %>% 
      apply(1, paste, collapse=", ") -> globconcs$str
    
    globconcs %<>% plyr::dlply(plyr::.(spec))
    
    for(s in spec) {
      ind <- match(locs$ID[miss], globconcs[[s]]$ID)
      concs[[s]][miss] <- globconcs[[s]]$str[ind]
    } 
  }
  
  
  concs %>% set_names(spec) %>% 
    lapply(data.frame) %>% lapply(set_names, 'str') %>% 
    lapply(data.frame, locs@data) %>% 
    bind_rows(.id='spec') %>% 
    spread(spec, str)
}


#' Title
#'
#' @param csvfile
#'
#' @return
#' @export
#'
#' @examples
read_calpost = function(csvfile) {
  readLines(csvfile, n=10) -> inlines
  startline = inlines %>% gsub(" ", "", .) %>% nchar %>% equals(0) %>% which %>% '['(2)
  read.table(csvfile, skip=startline,header=F,sep=",")
}

get_wdpa_for_grid = function(grids) {
  grids$gridR %>% projectExtent(crs(rworldmap::countriesLow)) %>% extent %>% 
    magrittr::multiply_by(1.1) %>% as.matrix %>% creahelpers::get_wdpa() %>% 
    sp::spTransform(crs(grids$gridR))
}
