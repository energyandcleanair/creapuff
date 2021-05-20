#' Title
#'
#' @param plants
#' @param plants_file
#' @param outputs
#' @param dir 
#' @param map_res in kilometers
#' @param utm_zone 
#' @param utm_hem 
#'
#' @return
#' @export
#'
#' @examples
plot_results <- function(dir,
                         map_res=10,
                         utm_zone=NULL,
                         utm_hem=NULL,
                         plants=NULL,
                         plants_file=NULL,
                         outputs=c("png", "kml", "expPop", "cityconcs")){
  
  # Get plants spatial data
  if(is.null(plants) & !is.null(plants_file)){
    plants <- read.csv(plants_file)
  }
  
  plants %<>%
    creahelpers::to_spdf()
  
  # Get grids
  utm_zone <- if(!is.null(utm_zone)) utm_zone else get_utm_zone(loc=plants)
  utm_hem <- if(!is.null(utm_hem)) utm_hem else get_utm_hem(loc=plants)
  calpuff_files <- get_calpuff_files(dir=dir)
  grids <- get_grids_calpuff(calpuff_files=calpuff_files, utm_zone=utm_zone, utm_hem=utm_hem, map_res=map_res)
  files <- calpuff_files$path
  
  # Get boundary box
  plants %<>% spTransform(crs(grids$gridR))
  plot_km <- 400
  plot_bb <- plants %>% extent %>% magrittr::add(plot_km)
  
  
  #
  #   CFPPs %<>% distinct(Lat, Long, .keep_all = T) %>%
  #     sel(Plant, Lat, Long, Status) %>% mutate(Source = Plant %>% gsub(' power station', '', ., ignore.case = T) %>%
  #                                                gsub(' Unit.*', '', .) %>%
  #                                                gsub(' U[0-9\\-]*', '', .)) %>%
  #     spdf %>% spTransform(crs(gridR))
  
  # Add cities
  cities <- creahelpers::get_boundaries_path('citiesDistToLarge.shp') %>% sf::read_sf() %>% as("Spatial")
  cities %>% raster::crop(grids$gridLL) %>% spTransform(crs(grids$gridR)) %>% raster::crop(plot_bb * .8) %>%
    (function(sp) sp[c(order(-sp$dstTLrg)[1:8], which(sp$name == 'Islamkot')), ]) -> cityPlot
  cityPlot$pos = ifelse(cityPlot@coords[, 1] < plot_bb@xmin + (plot_bb@xmax - plot_bb@xmin) * 1/3, 4, 2)
  cityPlot$pos[cityPlot$name == 'Badin'] <- 1
  
  # Output maps
  if("kml" %in% outputs) {
    #install.packages(c('plotrix', 'dismo', 'pixmap', 'RSAGA', 'colorRamps', 'aqp'))
    # library(plotKML)
    # if(!file.exists("zip.exe"))
    #   file.copy(paste0(HIApath, "zip.exe"),"zip.exe")
    label_file <- system.file("extdata", "factory.png", package="creapuff")
    file.copy(label_file, file.path(dir, basename(label_file)))
  }
  
  adm0_utm <- creahelpers::get_adm(0, res="coarse") %>% creahelpers::cropProj(grids$gridR)
  
  
  expPop <- list()
  popCP = makePop(grids=grids)
  
  calpuff_files$scenarioName <- calpuff_files$scenario
  # "Operating CFPPs"
  # calpuff_files$scenarioName[calpuff_files$scenario=='oprnew_a'] <- "Operating&Proposed CFPPs"
  calpuff_files$titletxt <- NA
  calpuff_files$titletxt <- paste0(ifelse(calpuff_files[["hr"]]<=24,paste0("Maximum ",calpuff_files[["hr"]],"-hour "),
                                         ifelse(calpuff_files[["type"]]=="concentration","Annual mean ","Annual total ")),
                                  calpuff_files[["speciesName"]]," ",calpuff_files[["type"]],
                                  "\nfrom ",calpuff_files[["scenarioName"]])
  
  queue <- 1:nrow(calpuff_files)
  # TODO Check we want to plot all of these?
  #which(calpuff_files$type=='concentration' &
   #             (calpuff_files$period == 'annual' | !is.na(calpuff_files$threshold)))  #1:nrow(calpuff_files) #which(calpuff_files$scenario=='ppmine') #
  test=F
  fn.ext <- "" #add an extension to end of filenames to avoid overwriting
  
  #set max value shown in graphs for each variable
  colorkeybasis <- calpuff_files$scenario[1] #"opr_all"  #'matar1' #set NULL to set colorkey separately for each scenario
  
  
  if(is.null(calpuff_files$k)) calpuff_files$k <- NA
  
  if(!is.null(colorkeybasis)){
    for(file in which(calpuff_files$scenario==colorkeybasis &
                      1:nrow(calpuff_files) %in% queue &
                      is.na(calpuff_files$k))){
      
      rfile <- gsub(".csv",".tif",files[file])
      
      if(!file.exists(rfile)){
        warning("Tif file doesn't exist. Recreating them now")
        suppressMessages(make_tifs(calpuff_files=calpuff_files, grids=grids))
      }
      
      if(!file.exists(rfile)){
       warning("File still does not exist: ", rfile, "[IGNORING]")
      }else{
        raster(rfile) -> conc_R
        prb = ifelse(calpuff_files$type[file] == 'deposition', .999625, 1)
        calpuff_files[calpuff_files$species == calpuff_files[file,"species"] &
                        calpuff_files$period == calpuff_files[file,"period"]
                      ,"k"] <- quantile(values(conc_R),probs=prb)
        print(files[file])  
      }
    }
  }
  
  
  #output maps
  for(file in queue[ifelse(test,1,T)]) {
    rfile <- gsub(".csv",".tif",files[file])
    if(file.exists(rfile)){
      raster(rfile) %>% #crop(plot_bb) %>%
        disaggregate(2, method='bilinear') -> conc_R
      max(values(conc_R)) -> maxVal
      
      #k=ifelse(calpuff_files[file,"type"]=="deposition",quantile(values(conc_R),probs=.9995),max(values(conc_R)))
      if(is.null(colorkeybasis)) {
        k=quantile(values(conc_R),probs=.9995)
        calpuff_files[file,"k"] <- k
      } else calpuff_files[file,"k"] -> k
      
      thr <- calpuff_files[file,"threshold"]
      exceed <- !is.na(thr) & max(values(conc_R)) >= thr
      if(!is.na(thr)) print(paste("threshold", ifelse(exceed,"","not"),"exceeded for",calpuff_files[file,"titletxt"]))
      
      plants -> plants_plot
      # CFPP_SP -> CFPPplot
      
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
        
        parSets = rasterTheme(region=colRamp)
        parSets$layout.widths = list(axis.key.padding = 0, ylab.right = 2)
        parSets$layout.widths$ylab.right = 2
        parSets$fontsize$text = 12*1.8; parSets$fontsize$points = 8*1.5
        parSets$axis.components$left$tck = 0
        parSets$axis.components$bottom$tck = 0
        parSets$axis.line=list(lwd=3)
        parSets$panel.background$col <- colRamp[length(colRamp)]
        
        outpng <- gsub("\\.csv|\\.tif",paste0("_levelplot",fn.ext,".png"),files[file])
        png(filename =  outpng,
            width = 3000, height = 2000, units = "px",
            bg = "white",res=200)
        
        pl <- levelplot(crop(conc_R,plot_bb),
                        margin=F,cex=.8,at=plumeBreaks[-length(plumeBreaks)],
                        par.settings=parSets,
                        main=calpuff_files[file,"titletxt"],ylab.right=calpuff_files[file,"unit"]) +
          layer(sp.lines(adm0_utm, lwd=3, col='darkgray')) +
          layer(sp.points(plants_plot, pch=24,lwd=1.5, col="white",fill="red",cex=.7)) +
          layer(sp.points(cityPlot, pch=1,lwd=3, col=labelcol)) +
          layer(sp.text(coordinates(cityPlot), txt = cityPlot$name,
                        pos = cityPlot$pos,col=labelcol,font=1, cex=.7))
        
        if(F) {
          pl = pl + layer(sp.text(textbuffer(coordinates(plants_plot),width=1.2,steps=16),
                                  txt = rep(CFPPplot$Source,16), pos = 4,font=2,cex=.6,col=rgb(1,1,1,alpha=1))) +
            # layer(sp.text(coordinates(plants_plot), txt = plants_plot$Source, pos = 4,font=2,cex=.6,col="red")) +
            layer(sp.text(admlab@coords,
                          admlab %>% row.names,
                          col='maroon3', font=3),
                  data=list(admlab=admlab))
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
          outL <- paste0(gsub("\n"," ",calpuff_files[file,"titletxt"]),fn.ext)
          
          colorRampPalette(c("steelblue","yellow","orange","red","darkred"))(length(lvls)) -> yorb
          
          
          png(file.path(dir, "label.png"),width=1000,height=100,pointsize=16,bg = "transparent")
          
          decimals <- lvls %>% subset(.>0) %>% min %>%
            log10 %>% -. %>% ceiling %>% max(0)
          legendlvls <- round(lvls, decimals)
          
          leg.title <- paste0(gsub(fn.ext, "", outL),
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
          
          zip(kmz_file,
              c(kml_file, file.path(dir,c("factory.png","label.png"))))
          
          if(!file.exists(kmz_file)) stop("creating kmz failed")
          file.remove(kml_file)
          file.remove(file.path(dir,"label.png"))
        }
      }
    }
  }
  
  
  
  if("expPop" %in% outputs) {
    expPop %>% ldply(.id='name') %>% left_join(calpuff_files %>% sel(name, titletxt, threshold)) -> expPop2
    expPop2 %>% filter(min >= threshold) %>% group_by(name, titletxt, threshold) %>%
      summarise_at(c('pop', 'area'), sum, na.rm=T) -> pop_exceed
    
    write_csv(expPop2, file.path(dir, paste0("expPop_new_format",fn.ext,".csv")))
    write_csv(pop_exceed, file.path(dir, paste0("threshold_exceedances",fn.ext,".csv")))
    
    outF <- file(file.path(dir, paste0("expPop",fn.ext,".csv")),"w")
    # names(expPop) <- gsub("\n"," ",calpuff_files[queue,"titletxt"])
    for(n in names(expPop)) {
      writeLines(n,outF)
      write.table(expPop[[n]],outF,append=T,row.names=F,quote=T,sep=",")
      writeLines("",outF)
    }
    close(outF)
  }
}
