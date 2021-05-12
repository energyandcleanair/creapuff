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


getUTMproj <- function(zone=NULL, hem=NULL, loc=NULL, units="km") {
  
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


cropProj <- function(shapeobj, rasterobj, expand=4, ...) {
  shapeobj %>%
    crop(extent(projectExtent(rasterobj,
                              crs(shapeobj)))+expand) -> shapeobj
  if(grepl("Raster", class(shapeobj))) {
    shapeobj %>% projectRaster(rasterobj, ...) %>% return
  } else shapeobj %>% spTransform(crs(rasterobj)) %>% return
}

quickpng <- function(file, width=2000, height=1500, res=300, ...) {
  png(filename=file, width=width, height=height, res=res, ...)
}

statmode <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(table(match(x, ux)))]
}

gridsToDomPols <- function(grids, target_crs){
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
    alply(1,
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

initkml <- function() {
  if(!file.exists("zip.exe"))
    file.copy(boxpath("tools&templates/zip.exe"),"zip.exe")
  labelF <- "factoryTransp3.png"
  if(!file.exists(labelF))
    file.copy(paste0("~/../Desktop/Box Sync/tools&templates/",labelF),labelF)
}


writeConcKML <- function(outFileName,plotTitle=outFileName,
                         contours,lvls,calpuff_files,
                         CFPPplot=get('CFPPplot',envir=.GlobalEnv),
                         sourceNameCol='Source.Name',
                         times=NULL,
                         initFile=T,closeFile=T,
                         leaveLabels=F, #should the label image files be left in the directory for checking
                         labelSize=.5,
                         iconScale=.5,
                         iconScaleCol=NULL) {
  require(plotKML)
  initkml()
  colorRampPalette(c("steelblue","yellow","orange","red","darkred"))(length(lvls)) -> yorb
  
  #open file for writing and make label
  if(initFile) {
    legendlvls <- lvls
    #legendlvls[length(lvls)] <- paste0(legendlvls[length(lvls)],calpuff_files[,"plotunit"])
    plotTitle <- paste0(plotTitle,' (',calpuff_files[,"plotunit"],')')
    labwidth <- max(sum(nchar(legendlvls))*56,
                    nchar(plotTitle)*11)
    
    png("label.png",width=labwidth,height=100,pointsize=18,bg = "transparent")
    
    par(mar = rep(.5, 4))
    plot(1, type="n", axes=FALSE, xlab="", ylab="")
    legend("topleft", legend = legendlvls, col=yorb, pch = 15,
           xjust=0.5, yjust=0,horiz=T,title = plotTitle,bg="white"
    )
    dev.off()
    
    kml_open(file.name=paste0(outFileName,".kml"))
  }
  
  #write contours
  if(!is.null(contours)) {
    if(!is.list(contours))
      list(contours) -> contours
    
    
    formatTime <- function(x) ifelse(is.null(x),NULL,format(x,'%Y-%m-%dT%H:%M:%SZ'))
    for(i in which(sapply(contours,nrow) > 0)) {
      
      if(!is.null(times)) {
        start.end <- c(start=formatTime(times[i]),
                       end=formatTime(times[i+1]-1))
      } else start.end <- NULL
      
      contours[[i]]$colN <- rank(contours[[i]]$max)
      kml_layer(obj=contours[[i]], subfolder.name=calpuff_files[,"plotunit"],
                colour=colN,
                colour_scale=c("steelblue","yellow","orange","red","darkred"),
                alpha=0.5,altitude=0,plot.labpt=F,
                labels=level,LabelScale=0.5,
                TimeSpan.begin=start.end['start'],
                TimeSpan.end=start.end['end'])
    }
  }
  
  
  if(closeFile) {
    if(!is.null(iconScaleCol)) {
      CFPPplot$iconScale = iconScale(CFPPplot[[iconScaleCol]])
    } else CFPPplot$iconScale = iconScale
    
    if(!is.null(labelSize)) {
      CFPPplot$KMLlabel <- enc2utf8(as.character(CFPPplot@data[[sourceNameCol]]))
      CFPPplot$labelSize <- labelSize
    } else {
      CFPPplot$KMLlabel <- ''
      CFPPplot$labelSize <- .5
    }
    
    kml_layer(obj=CFPPplot, subfolder.name="Modeled sources",
              size=iconScale,
              alpha=1,altitude=0,
              labels=KMLlabel,
              LabelScale=labelSize,sname="labels",shape="factoryTransp3.png")
    kml_screen(image.file="label.png",position="UL",sname="Label")
    kml_close(file.name=paste0(outFileName,".kml"))
    zip(paste0(outFileName,".kmz"),c(paste0(outFileName,".kml"),"factoryTransp3.png","label.png"))
    file.remove(paste0(outFileName,".kml"))
    
    if(leaveLabels) {
      file.rename("label.png",paste0('label-',outFileName,'.png'))
    } else file.remove("label.png")
  }
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
makePop <- function(grids,
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
