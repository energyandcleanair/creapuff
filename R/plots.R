
get_cities = function(plot_bb, grids, n=8, additional_city_regex='$^') {
  # Add cities
  cities <- creahelpers::get_boundaries_path('citiesDistToLarge.shp') %>% sf::read_sf() %>% as("Spatial") %>% 
    raster::crop(grids$gridLL) %>% spTransform(crs(grids$gridR))
  
  cities$ID = 1:nrow(cities)
  cities %>% raster::crop(plot_bb * .8) -> cityPlot
  cityPlot$ID[c(order(-cityPlot$dstTLrg)[1:n], grep(additional_city_regex, cityPlot$name))] %>% unique -> plot_cities

  cities$plot = cities$ID %in% plot_cities
  cities$pos = ifelse(cities@coords[, 1] < plot_bb@xmin + (plot_bb@xmax - plot_bb@xmin) * 1/3, 4, 2)
  
  return(cities)
}

make_titletxt = function(calpuff_files) {
  paste0(ifelse(calpuff_files[["hr"]]<=24,paste0("Maximum ",calpuff_files[["hr"]],"-hour "),
                ifelse(calpuff_files[["type"]]=="concentration","Annual mean ","Annual total ")),
         calpuff_files[["speciesName"]]," ",calpuff_files[["type"]],
         "\nfrom ",calpuff_files[["scenarioName"]])
}

#' Title
#'
#' @param plants
#' @param plants_file
#' @param outputs
#' @param dir 
#' @param map_res in kilometers
#'
#' @return
#' @export
#'
#' @examples
plot_results <- function(calpuff_files,
                         dir=dirname(calpuff_files$path[1]),
                         map_res=1,
                         plants=NULL,
                         plant_names=NULL,
                         get_plants=function(x, ...) { return(x) },
                         adm_level=0,
                         plot_km=400,
                         plot_bb=NULL,
                         cities=NULL,
                         colorkeybasis=NULL,
                         queue=seq_along(calpuff_files$path),
                         filename_suffix="",
                         outputs=c("png", "kml", "expPop", "cityconcs"),
                         zipping_function=zip::zip){
  
  if(!is.null(plants)) {
    if(!is.null(plant_names)) plants$Source <- plant_names
    if(is.null(plant_names) & is.null(plants$Source)) plants$Source <- ""
  }
  
  # Get grids
  grids <- creapuff::get_grids_calpuff(calpuff_files=calpuff_files)
  files <- calpuff_files$path
  
  # Get boundary box
  if(!is.null(plants)) plants %<>% creahelpers::to_spdf() %>% spTransform(crs(grids$gridR))
  
  if(is.null(plot_bb)) plot_bb <- plants %>% extent %>% magrittr::add(plot_km)
  if(is.null(cities)) cities <- get_cities(plot_bb, grids)
  if(is.null(cities$plot)) cities$plot = T
  cityPlot <- cities %>% subset(plot)
  
  # Output maps
  if("kml" %in% outputs) {
    label_file <- system.file("extdata", "factory.png", package="creapuff")
    file.copy(label_file, file.path(dir, basename(label_file)))
  }
  
  adm_utm <- creahelpers::get_adm(adm_level, res="low") %>% cropProj(grids$gridR)
  
  expPop <- list()
  popCP = makePop(grids=grids)
  
  if(is.null(calpuff_files$scenarioName)) calpuff_files$scenarioName <- calpuff_files$scenario
  
  if(is.null(calpuff_files$titletxt)) calpuff_files$titletxt <- make_titletxt(calpuff_files)
  
  #set max value shown in graphs for each variable
  
  if(is.null(calpuff_files$k)) calpuff_files$k <- NA
  
  if(!is.null(colorkeybasis)){
    for(file in which(calpuff_files$scenario==colorkeybasis &
                      1:nrow(calpuff_files) %in% queue &
                      is.na(calpuff_files$k))){
      
      rfile <- files[file]
      
      raster(rfile) -> conc_R
      prb = ifelse(calpuff_files$type[file] == 'deposition', .999625, 1)
      calpuff_files[calpuff_files$species == calpuff_files[file,"species"] &
                      calpuff_files$period == calpuff_files[file,"period"]
                    ,"k"] <- quantile(values(conc_R),probs=prb)
      print(files[file])  
      
    }
  }
  
  
  #output maps
  for(file in queue) {
    rfile <- files[file]
    raster(rfile) %>% disaggregate(2, method='bilinear') -> conc_R
    max(values(conc_R)) -> maxVal
    
    plants_plot = get_plants(plants, calpuff_files$scenario[file])
    
    if(is.null(colorkeybasis)) {
      k=quantile(values(conc_R),probs=.9995)
      calpuff_files[file,"k"] <- k
    } else calpuff_files[file,"k"] -> k
    
    thr <- calpuff_files[file,"threshold"]
    exceed <- !is.na(thr) & max(values(conc_R)) >= thr
    if(!is.na(thr)) print(paste("threshold", ifelse(exceed,"","not"),"exceeded for",calpuff_files[file,"titletxt"]))
    
    if("png" %in% outputs) {
      plumeBreaks <- c(seq(0,1,1/40)^3,2000)
      if(calpuff_files[file,"species"] %in% c("pm25","tpm10","tsp")) {
        colRamp <- colorRampPalette(colors=c("white","gray","yellow","red","black"))(42)
        labelcol="blue"
        wetlandcol="purple"
      } else {
        colRamp <- colorRampPalette(colors=c("white","gray","cyan","blue","purple"))(42)
        labelcol="black"
        wetlandcol="red" }
      
      plumeBreaks <- plumeBreaks * k
      
      al <- seq(0,k,sigfloor(k/5))
      axislabels = list(at=al,labels=al)
      
      require(rasterVis)
      parSets = rasterTheme(region=colRamp)
      parSets$layout.widths = list(axis.key.padding = 0, ylab.right = 2)
      parSets$layout.widths$ylab.right = 2
      parSets$fontsize$text = 12*1.8; parSets$fontsize$points = 8*1.5
      parSets$axis.components$left$tck = 0
      parSets$axis.components$bottom$tck = 0
      parSets$axis.line=list(lwd=3)
      parSets$panel.background$col <- colRamp[length(colRamp)]
      
      outpng <- gsub("\\.csv|\\.tif",paste0("_levelplot",filename_suffix,".png"),files[file])
      png(filename =  outpng,
          width = 3000, height = 2000, units = "px",
          bg = "white",res=200)
      
      pl <- levelplot(crop(conc_R,plot_bb),
                      margin=F,cex=.8,at=plumeBreaks[-length(plumeBreaks)],
                      par.settings=parSets,
                      main=calpuff_files[file,"titletxt"],ylab.right=calpuff_files[file,"unit"]) +
        layer(sp.lines(adm_utm, lwd=3, col='darkgray'))
      
      if(!is.null(plants_plot)) {
        pl = pl + layer(sp.points(plants_plot, pch=24,lwd=1.5, col="white",fill="red",cex=.7))
      }
      
      pl = pl + layer(sp.points(cityPlot, pch=1,lwd=3, col=labelcol)) +
        layer(sp.text(sp::coordinates(cityPlot), txt = cityPlot$name,
                      pos = cityPlot$pos,col=labelcol,font=1, cex=.7))
      
      if(!is.null(plant_names)) {
        pl = pl + layer(sp.text(textbuffer(sp::coordinates(plants_plot),width=1.2,steps=16),
                                txt = plants_plot$Source %>% lapply(rep,16) %>% unlist, 
                                pos = 4,font=2,cex=.6,col=rgb(1,1,1,alpha=1))) +
          layer(sp.text(sp::coordinates(plants_plot), txt = plants_plot$Source, pos = 4,font=2,cex=.6,col="red"))
      }
      
      print(pl)
      
      dev.off()
    }
    
    if("kml" %in% outputs | "expPop" %in% outputs) {
      
      lvls = unique(signif(c(k/10,seq(k/5,k,k/5)),1))
      
      if(!is.na(thr) & exceed) { #if a threshold has been specified, ensure it is included in levels
        lvls <- lvls[abs(lvls / thr - 1) > .1] #eliminate levels close to the threshold
        lvls <- sort(unique(c(lvls,thr)))
      }
      
      contP_UTM <- raster2contourPolys(conc_R,levels=lvls)[-1,]
      
      if("expPop" %in% outputs)
        expPop[[calpuff_files[file,"name"]]] <- data.frame(contP_UTM@data,
                                                           levelname=paste0(contP_UTM$level,calpuff_files[file,"unit"]),
                                                           pop=raster::extract(popCP,contP_UTM,sum,na.rm=T),
                                                           area=area(contP_UTM))
      
      if("kml" %in% outputs) {
        contP <- spTransform(contP_UTM,CRS(proj4string(grids$gridLL)))
        outL <- paste0(gsub("\n"," ",calpuff_files[file,"titletxt"]),filename_suffix)
        
        colorRampPalette(c("steelblue","yellow","orange","red","darkred"))(length(lvls)) -> yorb
        
        
        png(file.path(dir, "label.png"),width=1000,height=100,pointsize=16,bg = "transparent")
        
        decimals <- lvls %>% subset(.>0) %>% min %>%
          log10 %>% -. %>% ceiling %>% max(0)
        legendlvls <- round(lvls, decimals)
        
        leg.title <- paste0(gsub(filename_suffix, "", outL),
                            " (", calpuff_files[file,"unit"], ")")
        
        par(mar = rep(.5, 4))
        plot(1, type="n", axes=FALSE, xlab="", ylab="")
        legend("topleft", legend = legendlvls, col=yorb, pch = 15,
               xjust=0.5, yjust=0,horiz=T,
               title = leg.title,
               bg="white",
               text.width = 1.2*max(strwidth(legendlvls[-length(legendlvls)])))
        dev.off()
        
        kml_file <- file.path(dir, paste0(outL,".kml"))
        kmz_file <- file.path(dir, paste0(outL,".kmz"))
        colour <- rank(contP@data$max)
        kml_open(kml_file)
        kml_layer(obj=contP,
                  subfolder.name=calpuff_files[file,"unit"],
                  # colour=colour, #TODO NOT SURE WHY IT DOESN'T WORK
                  colour_scale=yorb,
                  alpha=0.5,
                  altitude=0,
                  plot.labpt=F,
                  labels=level,
                  LabelScale=0.5)
        
        kml_layer(obj=plants_plot, subfolder.name="Modeled sources",
                  size=1,
                  alpha=1,altitude=0,
                  labels=Source,
                  LabelScale=0.5,sname="labels", shape="factory.png")
        
        kml_screen(image.file="label.png",position="UL",sname="Label")
        kml_close(kml_file)
        
        zipping_function(kmz_file,
                         c(kml_file, file.path(dir,c("factory.png","label.png"))))
        
        if(!file.exists(kmz_file)) stop("creating kmz failed")
        file.remove(kml_file)
        file.remove(file.path(dir,"label.png"))
      }
    }
  }
  
  
  
  if("expPop" %in% outputs) {
    expPop %>% ldply(.id='name') %>% left_join(calpuff_files %>% sel(name, titletxt, threshold)) -> expPop2
    expPop2 %>% filter(min >= threshold) %>% group_by(name, titletxt, threshold) %>%
      summarise_at(c('pop', 'area'), sum, na.rm=T) -> pop_exceed
    
    write_csv(expPop2, file.path(dir, paste0("expPop_new_format",filename_suffix,".csv")))
    write_csv(pop_exceed, file.path(dir, paste0("threshold_exceedances",filename_suffix,".csv")))
    
    outF <- file(file.path(dir, paste0("expPop",filename_suffix,".csv")),"w")
    # names(expPop) <- gsub("\n"," ",calpuff_files[queue,"titletxt"])
    for(n in names(expPop)) {
      writeLines(n,outF)
      write.table(expPop[[n]],outF,append=T,row.names=F,quote=T,sep=",")
      writeLines("",outF)
    }
    close(outF)
  }
}
