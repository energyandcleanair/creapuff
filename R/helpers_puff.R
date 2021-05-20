

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
get_calpuff_files <- function(ext=".csv", gasunit="ug", dir=".") {
  
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
  
  calpuff_files[calpuff_files$species == 'hg','scale'] <- calpuff_files[calpuff_files$species == 'hg','scale'] * 1e3
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
  
  #exceedance thresholds - these will be recorded and included as a threshold level in contour plots
  calpuff_files$threshold <- NA
  calpuff_files[calpuff_files$speciesName=="SO2" & calpuff_files$hr==24,"threshold"] <- 20 #WHO
  calpuff_files[calpuff_files$speciesName=="NO2" & calpuff_files$hr==1,"threshold"] <- 200 #WHO
  calpuff_files[calpuff_files$speciesName=="PM2.5" & calpuff_files$hr==24,"threshold"] <- 25 #WHO
  calpuff_files[calpuff_files$speciesName=="PM10" & calpuff_files$hr==24,"threshold"] <- 50 #WHO
  calpuff_files[calpuff_files$speciesName=="mercury" & calpuff_files$type=="deposition","threshold"] <- 125 #Great lakes study
  calpuff_files[calpuff_files$speciesName=="SO2" & calpuff_files$hr==1,"threshold"] <- 75/0.355 #U.S. NAAQS
  calpuff_files$threshold.plotunit <- calpuff_files$threshold * calpuff_files$plotscale
  
  return(calpuff_files)
}


get_grids_calpuff <- function(calpuff_files,
                              runName=NULL,
                              utm_zone=NULL,
                              utm_hem=NULL,
                              map_res=NULL,
                              filepath=NULL) {
  
  if(is.null(runName)) runName <- calpuff_files[1,'scenario']
  
  if(is.null(filepath))
    filepath <- calpuff_files[calpuff_files$species=="pm25" &
                                calpuff_files$hr>24 & 
                                calpuff_files$scenario %in% runName, "path"][1]
  
  
  if(grepl('\\.tif$', filepath)) {
    gridR <- filepath %>% raster %>% raster %>% fixproj()
    gridSP <- NULL
  } else {
    if(is.null(utm_zone)) utm_zone=get('utm_zone', envir=.GlobalEnv)
    if(is.null(utm_hem)) utm_hem=get('utm_hem', envir=.GlobalEnv)
    if(is.null(map_res)) map_res=get('map_res', envir=.GlobalEnv)
    
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
    
    r <- raster(domain, resolution=map_res, crs=crs(pollSP))
    
    gridSP <- as(r, 'SpatialPixels') #CHECK We removed global variable
    gridR <- raster(gridSP)
  }
  
  gridLL <- projectRaster(gridR, crs = proj4string(rworldmap::countriesLow))
  gridLL <- extend(gridLL, c(40,40))
  
  return(list("gridR"=gridR,
              "gridSP"=gridSP,
              "gridLL"=gridLL))
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
getOutFiles=function(runDir, out_file_ext, batch_subset=NULL) {
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


#make bat file to generate .geo and .outa files with TAPM utilities
make_outa = function(out_files, tapm_dir, only_make_additional_files) {
  for(batch_name in out_files$run_name) {
    for(i in which(out_files$run_name == batch_name)) {
      run.outa <- !file.exists(gsub(".out",".geo",out_files[i, "file_out"])) | !only_make_additional_files 
      run.geo <- !file.exists(gsub(".out",".outa",out_files[i, "file_out"])) | !only_make_additional_files
      
      
      if(run.outa | run.geo) {
        file_bat_con <- file(paste0(tapm_dir,out_files[i,"grid_name"],"_TAPMtoCALTAPM.bat"),open="wt")
        if(run.outa) {
          query.xy <- round(unlist(out_files[i,c("GridNX","GridNY")])/2,0)
          writeLines(paste0(
            "echo     ",query.xy[1],"     ",query.xy[2],
            "     1     7         0.         0.        ",
            round(out_files[i,"Lon"],3),
            "     ",
            round(out_files[i,"Lat"],3),
            "  ",
            gsub("/","\\\\",gsub(".out","",out_files[i,"file_out"])),
            "  | C:\\tapm\\tapm2ts.exe"),
            con = file_bat_con)
        }
        
        if(run.geo) {
          writeLines(paste0(
            "echo ",out_files[i,"StartDate"]," ",out_files[i,"EndDate"]," ",
            gsub("/","\\\\",gsub(".out","",out_files[i,"file_out"])),
            " | C:\\tapm\\tapm2outa.exe"),con = file_bat_con) #TODO CLEAN THIS
        }
        
        #writeLines("pause",con=file_bat_con)
        close(file_bat_con) 
      }
    }
    
    #make .inp and .bat files to run CALTAPM
    
    for(i in which(out_files$run_name == batch_name)) {
      if(!file.exists(gsub(out_file_ext,".M3D",out_files[i, "file_out"])) | 
         !only_make_additional_files) {
        file_bat_con <- file(file.path(tapm_dir,paste0(out_files[i,"grid_name"],"_CALTAPM.bat")),open="wt")
        writeLines(paste0("caltapm_v7.0.0 ",out_files[i,"grid_name"],"_CALTAPM.inp"),con = file_bat_con)
        #writeLines("pause",con=file_bat_con)
        close(file_bat_con)
        
        
        inpCon <- file(file.path(tapm_dir, paste0(out_files[i,"grid_name"],"_CALTAPM.inp")), open="wt")
        writeLines(paste0(gsub("/","\\\\",out_files[i, "file_out"]),"a"),con = inpCon)             
        writeLines(gsub("/","\\\\",out_files[i, "file_def"]),con = inpCon)                 
        writeLines(paste0(gsub(".out","",gsub("/","\\\\",out_files[i, "file_out"])),".top"),con = inpCon)                  
        writeLines(paste0(out_files[i,"grid_name"],"_CALTAPM.LST"),con = inpCon)              
        writeLines(paste0(gsub(".out","",gsub("/","\\\\",out_files[i, "file_out"])),".M3D"),con = inpCon)              
        writeLines(paste0(out_files[i,"StartDate"],"01     : beginning date for processing (LST time - YYYYMMDDHH)"),
                   con = inpCon)
        writeLines(paste0(out_files[i,"EndDate"],"24     : ending    date for processing (LST time - YYYYMMDDHH)"),
                   con = inpCon)
        close(inpCon)
      }
    }
  }
}


#read data from  .geo files
read_TAPM_GEO = function(out_files, queue=1:nrow(out_files), backup=T) {
  out_files %<>% mutate(GridX=NA,GridY=NA,GridD=NA,QA3D=NA)
  
  for(i in queue) {
    bupGeo <- paste0(out_files[i, "dir"],"backup/",out_files[i, "grid_name"],".geo")
    geo <- gsub(out_file_ext,".geo",out_files[i, "file_out"])
    
    if(backup) {
      #make backup dir
      dir.create(file.path(out_files[i, "dir"], "backup"))
      #make backup file
      if(!file.exists(bupGeo))
        file.copy(geo, bupGeo,overwrite = F)
    } else bupGeo <- file.path(out_files[i, "dir"], paste0(out_files[i, "grid_name"], ".geo"))
    
    #read .geo file, save grid params 
    inGeoCon <- file(geo,open="rt")
    
    geo_data <- readLines(inGeoCon)
    inGeoCon <- file(geo,open="rt")
    
    #read grid extents and UTM zone from geo file
    gridData <- scan(inGeoCon,nmax=6,what=numeric(),skip=5, nlines=1)
    out_files[i, grepl("Grid",colnames(out_files))] <- gridData[1:5]  
    
    out_files[i, "UTMZ"] <- as.numeric(gsub("[A-Z]","",geo_data[4]))
    out_files[i, "UTMH"] <- gsub(" ","",gsub("[0-9]+{1-2}","",geo_data[4]))
    
    
    
    close(inGeoCon)
    
  }
  
  return(out_files)
}


#overwrite the original .geo files with grid params from out_files dataframe and with correct spacing
makeGeo = function(out_files, queue=1:nrow(out_files)) {
  for(i in queue) {
    inGeoCon <- file(paste0(out_files[i, "dir"],"backup/",out_files[i, "grid_name"],".geo"),open="rt")
    outGeoCon <- file(gsub(".out",".geo",out_files[i, "file_out"]), open="wt")
    
    geo_data <- readLines(inGeoCon)
    
    geo_data[5] <- "WGS-84   10-10-2002  "
    geo_data[6] <- paste0(
      paste(format(out_files[i,c("GridNX","GridNY")],digits=0,width=8),collapse=""),
      paste(format(out_files[i,c("GridX","GridY","GridD","GridD")],nsmall=3,width=12),collapse=""))
    
    #check for missing land use (lu) values and interpolate as needed
    luCon <- textConnection(geo_data[9:(9+out_files[i,"GridNY"]-1)])
    lu <- as.matrix(read.table(luCon))
    if(sum(lu==0)>0)
    {
      luR <- raster(lu)
      luR[luR==0]<-NA
      window=1
      luFill<-luR
      while(sum(is.na(values(luFill)))>0 & window<out_files[i,"GridNY"]/2)
      {
        luFill <- focal(luR,w=matrix(1,1+2*window,1+2*window),NAonly=T,fun=mean,na.rm=T,pad=T)
        window=window+1
      }
      
      lu<-round(as.matrix(luFill),-1)
      
      for(l in 1:out_files[i,"GridNY"])
        geo_data[9+l-1] <- paste0(" ",paste(lu[l,],collapse = " "))
    }
    
    
    writeLines(geo_data, con=outGeoCon)
    
    close(inGeoCon)
    close(outGeoCon)
    
  }
}


#function to set parameters in a CALPUFF input file
setPuff <- function(inpfile, paramdf, set.all=T) {
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


writeInp <- function(file_template, file_out, params, ...) {
  readLines(file_template) -> pu.inp
  setPuff(pu.inp, params, ...) -> pu.inp
  outcon <- file(file_out, "w")
  writeLines(pu.inp, outcon)
  close(outcon)
}


makeParams <- function(out_files) {
  #define parameters to set in CALPUFF.INP
  
  #start and end times
  paramlist <- 
    list(
      #times
      IBYR = substr(out_files$StartDate, 1, 4),
      IBMO = substr(out_files$StartDate, 5, 6),
      IBDY=substr(out_files$StartDate, 7, 8),
      IEYR=substr(out_files$EndDate, 1, 4),
      IEMO=substr(out_files$EndDate, 5, 6),
      IEDY=substr(out_files$EndDate, 7, 8),
  
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
  
  setParam(name=paramlist)
}


setParam <- function(df=NULL, name, value=NULL) {
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


getParamVal <- function(parname, inp, max.number=1) {
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
  
  inparams %>% lapply(getParamVal, inp, max.number=Inf) %>% set_names(inparams) -> d
  
  d[['datadir']] <- dirname(d[['CONDAT']]) %>% paste0('/')
  
  if(get_emissions) {
    nspec <- length(d[["CSPEC"]])
    npt1 <- getParamVal("NPT1", inp)
    getParamVal("X", inp, max.number=npt1) -> d[["X"]]
    d[["X"]] %>% lapply(function(x) x %>% strsplit(',') %>% unlist %>% as.numeric()) -> emis
    emis %>% lapply(function(e) e[(length(e) - nspec + 1):length(e)]) %>% 
      lapply(function(e) data.frame(species = d[["CSPEC"]],
                                    modeled = e)) -> d[["emissions"]]
    names(d[["emissions"]]) <- d[['SRCNAM']]
  }    
  
  return(d)
}


#remove discrete receptors and emissions sources from calpuff INP
cleanInp <- function(calpuff_inp) {
  grep('^Subgroup \\(13b\\)', calpuff_inp) -> psst
  grep('^Subgroup \\(13c\\)', calpuff_inp) -> psend
  grep('!', calpuff_inp) %>% subset(. %in% psst:psend) -> psln
  if(length(psln)>0) calpuff_inp[-psln] -> calpuff_inp
  
  grep('^Subgroup \\(20c\\)', calpuff_inp) -> drheader
  grep('!', calpuff_inp) %>% subset(. > drheader) -> drln
  if(length(drln)>0) calpuff_inp[-drln] -> calpuff_inp
  return(calpuff_inp)
}


makeCalpuffInp <- function(files_met,
                             calpuff_template,
                             output_dir,
                             puffrun=NULL,
                             sourceLines=NULL,
                             receptors=NULL,
                             OZONE.DAT=NULL,
                             bgconcs=lapply(list(O3=rep(25, 12),NH3=rep(10, 12),H2O2=rep(1, 12)), paste, collapse=','),
                             addparams=list(),
                             addsubgroups=list()) {
  
  
  
  if(is.null(puffrun)) puffrun=files_met$run_name[1]
  
  #read CALPUFF.INP
  calpuff_inp <- readLines(calpuff_template)
  
  metdir=files_met$dir[1] %>% as.character()
  file_out <- file.path(metdir, puffrun)
  
  files_met %<>% arrange(desc(GridD))
  metrun=files_met$run_name[1] %>% as.character()
  
    
  params <- files_met %>% head(1) %>% makeParams()
  
  #additional parameters
  addparams$NREC = length(receptors)
  addparams$NPT1 = length(sourceLines)/4
  
  #met domains and data files
  print(paste0("Number of domains is ", nrow(files_met)))
  addparams$NMETDOM = nrow(files_met)
  addparams$NMETDAT = nrow(files_met)
  
  if(nrow(files_met) == 1)
    addparams$METDAT = file.path(metdir, paste0(files_met$grid_name,"_CALMET.DAT"))

  if(is.null(files_met$grid_name)) stop('grid_name cannot be NULL')
  for(r in 1:5) {
    gridLevelAvailable <- (!is.na(files_met[r,"grid_name"]) & nrow(files_met)>1)
    addparams[[paste0('DOMAIN',r)]] <-
      ifelse(gridLevelAvailable,
             as.character(files_met[r,"grid_name"]),
             "not set")
    
    addparams[[paste0('METDAT',r)]] <- 
      ifelse(gridLevelAvailable,
             file.path(files_met[r, "dir"],
                       paste0(files_met[r,"grid_name"],"_CALMET.DAT")),
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
  addparams$MOZ = ifelse(!is.null(OZONE.DAT), 1, 0)
  addparams$MNH3 = 0
  addparams$MH2O2 = 0
  addparams$BCKO3 = ifelse(!is.null(OZONE.DAT), "not set", bgconcs$O3)
  addparams$BCKNH3 = bgconcs[["NH3"]]
  if(is.null(bgconcs[["H2O2"]])) bgconcs[["H2O2"]]=bgconcs[["H2O_"]]
  addparams$BCKH2O2 = bgconcs[["H2O2"]]
  addparams$OZDAT = ifelse(!is.null(OZONE.DAT), file.path(metdir, OZONE.DAT), "not set")
  
  #set parameter values
  setParam(params, addparams) -> params
  setPuff(calpuff_inp,params) -> calpuff_inp
  
  #remove discrete receptors and emissions sources from calpuff INP
  calpuff_inp %<>% cleanInp()
  
  if(!is.null(sourceLines)) addsubgroups %<>% c(list(X13b = sourceLines))
  if(!is.null(receptors))   addsubgroups %<>% c(list(X20c = receptors))
  
  for(sg in names(addsubgroups))
    calpuff_inp %<>% addSubgroupLines(addsubgroups[[sg]], subgroup=grm(sg, "^X"))
  
  #write into file
  outF <- file.path(output_dir, paste0(puffrun,"_CALPUFF_7.0.inp"))
  writeLines(calpuff_inp, outF)  
  
  return(outF)
}


readGEO <- function(geoPath) {
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


getPlantElev <- function(sources.sp, dir, out_files) {
  out_files %>% arrange(GridD) %>%
    mutate(path = file.path(dir, paste0(grid_name,".geo"))) %>%
    magrittr::use_series(path) %>% 
    lapply(readGEO) -> topoR
  sources.sp %<>% spTransform(crs(topoR[[1]]))
  topoR %>% lapply(extract, sources.sp) %>% data.frame -> elevs
  elevs %>% apply(1, function(x) x[!is.na(x)][1])
}


addSubgroupLines = function(inp, lines, subgroup, skip_lines=NULL) {
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


getSourceElev = function(sources, run_name) {
  sources$base.elevation..msl <- 
    getPlantElev(sources, out_files_all %>% filter(run_name == city_sp$run_name))
  sources.out
}


getRecep <- function(casecity,
                      nesfactL=c(4, 16, 40),
                      output_dir,
                      calpuff_exe,
                      calpuff_template,
                      out_files_all,
                      target_crs) {
  # setwd(calpuffDir)
  
  run=casecity$run_name
  files_met = out_files_all %>% subset(run_name == run) %>% arrange(desc(GridD))
  inpDir <- files_met$dir[1]
  calmetRes <- files_met$GridD[1] #resolution of the CALMET grid, in km
  calmetXY <- c(X=files_met$GridX[1],Y=files_met$GridY[1]) #origin (LL corner) of CALMET grid
  GridNX <- files_met$GridNX[1]
  GridNY <- files_met$GridNY[1]
  
  source.id <- casecity$source.name #city_short # LC
  cluster.center <- casecity %>% coordinates %>% data.frame %>% set_names(c("X", "Y"))
  
  for(nesfact in nesfactL) {
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
      makeCalpuffInp(puffrun=source.id,
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
                   ITEST=1, #STOPS program after SETUP phase
                   NPT1=0) #no emission sources
    
    #remove discrete receptors and emissions sources from calpuff INP
    grep('^Subgroup \\(13b\\)', calpuff_inp) -> psst
    grep('^Subgroup \\(13c\\)', calpuff_inp) -> psend
    grep('!', calpuff_inp) %>% subset(. %in% psst:psend) -> psln
    if(length(psln) > 0) calpuff_inp[-psln] -> calpuff_inp
    
    grep('^Subgroup \\(20c\\)', calpuff_inp) -> drheader
    grep('!', calpuff_inp) %>% subset(. > drheader) -> drln
    if(length(drln) > 0) calpuff_inp[-drln] -> calpuff_inp
    
    setPuff(calpuff_inp, params) -> calpuff_inp
    
    #write into file
    outinp <- file.path(output_dir, paste0(source.id,"_elevgen_CALPUFF_7.0.inp"))
    writeLines(calpuff_inp, outinp)  
    
    org_dir <- getwd()
    # calpuff_dir <- dirname(calpuff_exe)
    setwd(output_dir)
    
    #run CALPUFF setup to generate receptor grid  
    # file.rename("qarecg.dat","qarecg_backup.dat")
    system2(calpuff_exe, args=outinp)
    
    #rename receptor file and move to input directory
    file.rename("qarecg.dat", paste(source.id,'nesfact',nesfact,"qarecg.dat",sep="_"))
    
    setwd(org_dir)
  }
  
  
  ##read the receptor file written by CALPUFF and generate nested grids of discrete receptors
  # setwd(inpDir)
  
  #read in and merge files generated above
  nesfactL %>% 
    lapply(function(nf) {
      file.path(inpDir, paste(source.id,'nesfact',nf,"qarecg.dat",sep="_")) %>%
        read.table(stringsAsFactors = F,header=T) %>% 
        mutate(nesfact=nf)
    }) %>% bind_rows %>% to_spdf(crs=target_crs)
}


makeToporows <- function(topoXYZ) {
  paste0("Disc",1:nrow(topoXYZ),
         "  !  X = ",
         topoXYZ$Xkm,", ",
         topoXYZ$Ykm,", ",
         format(round(topoXYZ$Elev,1),nsmall=1, scientific=F), 
         " ! ! END !")
}


getBgconcs = function(sources, mod_dir) {

  sources %<>% to_spdf
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
    
    raster::extract(r, sources) -> conc
    
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
      stack %>% raster::extract(sources[miss, ]) %>% 
      data.frame(sources@data[miss, ], .) %>% 
      gather(var, val, contains("_M")) %>% 
      separate('var', c('spec', 'M')) %>% 
      spread(M, val) -> globconcs
    
    globconcs %<>% full_join(data.frame(spec=spec, scaling=scaling))
    
    
    globconcs[, paste0('M', 1:12)] %>% 
      multiply_by(globconcs$scaling) %>% 
      round(3) %>% 
      apply(1, paste, collapse=", ") -> globconcs$str
    
    globconcs %<>% dlply(.(spec))
    
    for(s in spec) {
      ind <- match(sources$city[miss], globconcs[[s]]$city)
      concs[[s]][miss] <- globconcs[[s]]$str[ind]
    } 
  }
  
  
  concs %>% set_names(spec) %>% 
    lapply(data.frame) %>% lapply(set_names, 'str') %>% 
    lapply(data.frame, sources@data) %>% 
    ldply(.id='spec') %>% 
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