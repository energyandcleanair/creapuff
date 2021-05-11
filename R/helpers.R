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
