#read coords and timezones from .def files
read_def = function(defFiles) {
  cbind(defFiles,Lat=NA,Lon=NA,TZ=NA,GridNX=NA,GridNY=NA,
        UTMZ=NA,UTMH=NA,StartDate=NA,EndDate=NA) -> defFiles
  
  for(i in 1:nrow(defFiles)) {
    defCon <- file(as.character(defFiles[i,"defFilePath"]), open="rt")
    readLines(defCon) -> indef
    defFiles[i,c("Lat","Lon")] <- as.numeric(indef[c(4,5)])
    defFiles[i,c("StartDate","EndDate")] <- indef[c(8,9)]
    defFiles[i,c("TZ")] <- round(as.numeric(indef[10]),0)
    defFiles[i,c("GridNX","GridNY")] <- as.numeric(indef[c(25,26)])
    close(defCon)
  }
}


#find all .def files and .out files in the run directory and subdirectories
get_outFiles=function(runDir, outFileExt, batchSubset=NULL) {
  outFiles <- data.frame(outFilePath = gsub("//","/",list.files(path=runDir,pattern=paste0("*",outFileExt,"$"),recursive=T,full.names=T,include.dirs=T)))
  defFiles <- data.frame(defFilePath = gsub("//","/",list.files(path=runDir,pattern="*.def",recursive=T,full.names=T,include.dirs=T)))
  
  outFiles <- cbind(outFiles,
                    runName = outFiles$outFilePath %>% 
                      gsub("[a-zA-Z0-9_:]*/+","", .) %>% 
                      get_runName,
                    gridName = gsub(outFileExt,"",gsub("[a-zA-Z0-9_:]*/+","",outFiles$outFilePath)),
                    dir = gsub(paste0("[a-zA-Z0-9_]*",outFileExt),"",outFiles$outFilePath))
  defFiles$runName = defFiles$defFilePath %>% 
    gsub("[a-zA-Z0-9_:]*/+","", .) %>% get_runName
  
  defFiles %<>% subset(!duplicated(runName))
  
  if(!is.null(batchSubset)) outFiles %<>% subset(runName %in% batchSubset)
  
  #read coords and timezones from .def files
  defFiles %<>% read_def
  outFiles %>% join(defFiles,by="runName",match="first")
}


#make bat file to generate .geo and .outa files with TAPM utilities
make_outa = function(outFiles, onlyMakeAdditionalFiles) {
  for(batchName in outFiles$runName) {
    for(i in which(outFiles$runName == batchName)) {
      run.outa <- !file.exists(gsub(".out",".geo",outFiles[i, "outFilePath"])) | !onlyMakeAdditionalFiles 
      run.geo <- !file.exists(gsub(".out",".outa",outFiles[i, "outFilePath"])) | !onlyMakeAdditionalFiles
      
      
      if(run.outa | run.geo) {
        batCon <- file(paste0(tapmDir,outFiles[i,"gridName"],"_TAPMtoCALTAPM.bat"),open="wt")
        if(run.outa) {
          query.xy <- round(unlist(outFiles[i,c("GridNX","GridNY")])/2,0)
          writeLines(paste0(
            "echo     ",query.xy[1],"     ",query.xy[2],
            "     1     7         0.         0.        ",
            round(outFiles[i,"Lon"],3),
            "     ",
            round(outFiles[i,"Lat"],3),
            "  ",
            gsub("/","\\\\",gsub(".out","",outFiles[i,"outFilePath"])),
            "  | C:\\tapm\\tapm2ts.exe"),
            con = batCon)
        }
        
        if(run.geo) {
          writeLines(paste0(
            "echo ",outFiles[i,"StartDate"]," ",outFiles[i,"EndDate"]," ",
            gsub("/","\\\\",gsub(".out","",outFiles[i,"outFilePath"])),
            " | C:\\tapm\\tapm2outa.exe"),con = batCon) #TODO CLEAN THIS
        }
        
        #writeLines("pause",con=batCon)
        close(batCon) 
      }
    }
    
    #make .inp and .bat files to run CALTAPM
    
    for(i in which(outFiles$runName == batchName)) {
      if(!file.exists(gsub(outFileExt,".M3D",outFiles[i, "outFilePath"])) | 
         !onlyMakeAdditionalFiles) {
        batCon <- file(paste0(caltapmDir,outFiles[i,"gridName"],"_CALTAPM.bat"),open="wt")
        writeLines(paste0("caltapm_v7.0.0 ",outFiles[i,"gridName"],"_CALTAPM.inp"),con = batCon)
        #writeLines("pause",con=batCon)
        close(batCon)
        
        
        inpCon <- file(paste0(caltapmDir,outFiles[i,"gridName"],"_CALTAPM.inp"),open="wt")
        writeLines(paste0(gsub("/","\\\\",outFiles[i, "outFilePath"]),"a"),con = inpCon)             
        writeLines(gsub("/","\\\\",outFiles[i, "defFilePath"]),con = inpCon)                 
        writeLines(paste0(gsub(".out","",gsub("/","\\\\",outFiles[i, "outFilePath"])),".top"),con = inpCon)                  
        writeLines(paste0(outFiles[i,"gridName"],"_CALTAPM.LST"),con = inpCon)              
        writeLines(paste0(gsub(".out","",gsub("/","\\\\",outFiles[i, "outFilePath"])),".M3D"),con = inpCon)              
        writeLines(paste0(outFiles[i,"StartDate"],"01     : beginning date for processing (LST time - YYYYMMDDHH)"),
                   con = inpCon)
        writeLines(paste0(outFiles[i,"EndDate"],"24     : ending    date for processing (LST time - YYYYMMDDHH)"),
                   con = inpCon)
        close(inpCon)
      }
    }
  }
}


#read data from  .geo files
read_TAPM_GEO = function(outFiles, queue=1:nrow(outFiles), backup=T) {
  outFiles %<>% mutate(GridX=NA,GridY=NA,GridD=NA,QA3D=NA)
  
  for(i in queue) {
    bupGeo <- paste0(outFiles[i, "dir"],"backup/",outFiles[i, "gridName"],".geo")
    geo <- gsub(outFileExt,".geo",outFiles[i, "outFilePath"])
    
    if(backup) {
      #make backup dir
      dir.create(paste0(outFiles[i, "dir"],"backup"))
      #make backup file
      if(!file.exists(bupGeo))
        file.copy(geo, bupGeo,overwrite = F)
    } else bupGeo <- paste0(outFiles[i, "dir"],outFiles[i, "gridName"],".geo")
    
    #read .geo file, save grid params 
    inGeoCon <- file(geo,open="rt")
    
    geoData <- readLines(inGeoCon)
    inGeoCon <- file(geo,open="rt")
    
    #read grid extents and UTM zone from geo file
    gridData <- scan(inGeoCon,nmax=6,what=numeric(),skip=5, nlines=1)
    outFiles[i,grepl("Grid",colnames(outFiles))] <- gridData[1:5]  
    
    outFiles[i, "UTMZ"] <- as.numeric(gsub("[A-Z]","",geoData[4]))
    outFiles[i, "UTMH"] <- gsub(" ","",gsub("[0-9]+{1-2}","",geoData[4]))
    
    
    
    close(inGeoCon)
    
  }
  
  return(outFiles)
}


#overwrite the original .geo files with grid params from outFiles dataframe and with correct spacing
make_geo = function(outFiles, queue=1:nrow(outFiles)) {
  for(i in queue) {
    inGeoCon <- file(paste0(outFiles[i, "dir"],"backup/",outFiles[i, "gridName"],".geo"),open="rt")
    outGeoCon <- file(gsub(".out",".geo",outFiles[i, "outFilePath"]),open="wt")
    
    geoData <- readLines(inGeoCon)
    
    geoData[5] <- "WGS-84   10-10-2002  "
    geoData[6] <- paste0(
      paste(format(outFiles[i,c("GridNX","GridNY")],digits=0,width=8),collapse=""),
      paste(format(outFiles[i,c("GridX","GridY","GridD","GridD")],nsmall=3,width=12),collapse=""))
    
    #check for missing land use (lu) values and interpolate as needed
    luCon <- textConnection(geoData[9:(9+outFiles[i,"GridNY"]-1)])
    lu <- as.matrix(read.table(luCon))
    if(sum(lu==0)>0)
    {
      luR <- raster(lu)
      luR[luR==0]<-NA
      window=1
      luFill<-luR
      while(sum(is.na(values(luFill)))>0 & window<outFiles[i,"GridNY"]/2)
      {
        luFill <- focal(luR,w=matrix(1,1+2*window,1+2*window),NAonly=T,fun=mean,na.rm=T,pad=T)
        window=window+1
      }
      
      lu<-round(as.matrix(luFill),-1)
      
      for(l in 1:outFiles[i,"GridNY"])
        geoData[9+l-1] <- paste0(" ",paste(lu[l,],collapse = " "))
    }
    
    
    writeLines(geoData,con=outGeoCon)
    
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


write.inp <- function(template.file, out.file, params, ...) {
  readLines(template.file) -> pu.inp
  setPuff(pu.inp, params, ...) -> pu.inp
  outcon <- file(out.file, "w")
  writeLines(pu.inp, outcon)
  close(outcon)
}


makeparams <- function(outFiles) {
  #define parameters to set in CALPUFF.INP
  
  #start and end times
  paramlist <- 
    list(
      #times
      IBYR = substr(outFiles$StartDate, 1, 4),
      IBMO = substr(outFiles$StartDate, 5, 6),
      IBDY=substr(outFiles$StartDate, 7, 8),
      IEYR=substr(outFiles$EndDate, 1, 4),
      IEMO=substr(outFiles$EndDate, 5, 6),
      IEDY=substr(outFiles$EndDate, 7, 8),
  
      #timezone
      ABTZ=TZstring(outFiles$TZ),
      
      #grid settings
      IUTMZN=outFiles$UTMZ,
      UTMHEM=outFiles$UTMH,
      DGRIDKM=outFiles$GridD,
      NX=outFiles$GridNX,
      NY=outFiles$GridNY,
      IECOMP=outFiles$GridNX,
      JECOMP=outFiles$GridNY,
      IESAMP=outFiles$GridNX,
      JESAMP=outFiles$GridNY,
      XORIGKM=outFiles$GridX,
      YORIGKM=outFiles$GridY)
  
  setparam(name=paramlist)
}


setparam <- function(df=NULL, name, value=NULL) {
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
read_puffinp <- function(file,
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
clean_inp <- function(calpuffInp) {
  grep('^Subgroup \\(13b\\)', calpuffInp) -> psst
  grep('^Subgroup \\(13c\\)', calpuffInp) -> psend
  grep('!', calpuffInp) %>% subset(. %in% psst:psend) -> psln
  if(length(psln)>0) calpuffInp[-psln] -> calpuffInp
  
  grep('^Subgroup \\(20c\\)', calpuffInp) -> drheader
  grep('!', calpuffInp) %>% subset(. > drheader) -> drln
  if(length(drln)>0) calpuffInp[-drln] -> calpuffInp
  return(calpuffInp)
}


make_calpuff_inp <- function(metfiles,
                             calpuff_template,
                             output_dir,
                             puffrun=NULL,
                             sourceLines=NULL,
                             receptors=NULL,
                             OZONE.DAT=NULL,
                             bgconcs=lapply(list(O3=rep(25, 12),NH3=rep(10, 12),H2O2=rep(1, 12)), paste, collapse=','),
                             addparams=list(),
                             addsubgroups=list()) {
  
  
  
  if(is.null(puffrun)) puffrun=metfiles$runName[1]
  
  #read CALPUFF.INP
  calpuffInp <- readLines(calpuff_template)
  
  metdir=metfiles$dir[1] %>% as.character()
  outFilePath <- file.path(metdir, puffrun)
  
  metfiles %<>% arrange(desc(GridD))
  metrun=metfiles$runName[1] %>% as.character()
  
    
  params <- metfiles %>% head(1) %>% makeparams()
  
  #additional parameters
  addparams$NREC = length(receptors)
  addparams$NPT1 = length(sourceLines)/4
  
  #met domains and data files
  print(paste0("Number of domains is ", nrow(metfiles)))
  addparams$NMETDOM = nrow(metfiles)
  addparams$NMETDAT = nrow(metfiles)
  
  if(nrow(metfiles) == 1)
    addparams$METDAT = file.path(metdir, paste0(metfiles$gridName,"_CALMET.DAT"))

  if(is.null(metfiles$gridName)) stop('gridName cannot be NULL')
  for(r in 1:5) {
    gridLevelAvailable <- (!is.na(metfiles[r,"gridName"]) & nrow(metfiles)>1)
    addparams[[paste0('DOMAIN',r)]] <-
      ifelse(gridLevelAvailable,
             as.character(metfiles[r,"gridName"]),
             "not set")
    
    addparams[[paste0('METDAT',r)]] <- 
      ifelse(gridLevelAvailable,
             file.path(metfiles[r, "dir"],
                       paste0(metfiles[r,"gridName"],"_CALMET.DAT")),
             "not set")
  }
  
  #output file names
  addparams$CONDAT = paste0(outFilePath,".CON")
  addparams$DFDAT = paste0(outFilePath,".DRY")
  addparams$WFDAT = paste0(outFilePath,".WET")
  addparams$VISDAT = paste0(outFilePath,".VIS")
  addparams$BALDAT = paste0(outFilePath,".BAL")
  addparams$PUFLST = paste0(outFilePath,"_CALPUFF.LST")
  
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
  setparam(params, addparams) -> params
  setPuff(calpuffInp,params) -> calpuffInp
  
  #remove discrete receptors and emissions sources from calpuff INP
  calpuffInp %<>% clean_inp()
  
  if(!is.null(sourceLines)) addsubgroups %<>% c(list(X13b = sourceLines))
  if(!is.null(receptors))   addsubgroups %<>% c(list(X20c = receptors))
  
  for(sg in names(addsubgroups))
    calpuffInp %<>% add_subgroup_lines(addsubgroups[[sg]], subgroup=grm(sg, "^X"))
  
  #write into file
  outF <- file.path(output_dir, paste0(puffrun,"_CALPUFF_7.0.inp"))
  writeLines(calpuffInp, outF)  
  
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


getPlantElev <- function(sources.sp, dir, outFiles) {
  outFiles %>% arrange(GridD) %>%
    mutate(path = file.path(dir, paste0(gridName,".geo"))) %>%
    magrittr::use_series(path) %>% 
    lapply(readGEO) -> topoR
  sources.sp %<>% spTransform(crs(topoR[[1]]))
  topoR %>% lapply(extract, sources.sp) %>% data.frame -> elevs
  elevs %>% apply(1, function(x) x[!is.na(x)][1])
}


add_subgroup_lines = function(inp, lines, subgroup, skip_lines=NULL) {
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


get_source_elev = function(sources, runName) {
  sources$base.elevation..msl <- 
    getPlantElev(sources, outFilesAll %>% filter(runName == city_sp$runName))
  sources.out
}


get_recep <- function(casecity,
                      nesfactL=c(4, 16, 40),
                      output_dir,
                      calpuff_exe,
                      calpuff_template,
                      outFilesAll,
                      target_crs) {
  # setwd(calpuffDir)
  
  run=casecity$runName
  metfiles = outFilesAll %>% subset(runName == run) %>% arrange(desc(GridD))
  inpDir <- metfiles$dir[1]
  calmetRes <- metfiles$GridD[1] #resolution of the CALMET grid, in km
  calmetXY <- c(X=metfiles$GridX[1],Y=metfiles$GridY[1]) #origin (LL corner) of CALMET grid
  GridNX <- metfiles$GridNX[1]
  GridNY <- metfiles$GridNY[1]
  
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
    metfiles %>% 
      make_calpuff_inp(puffrun=source.id,
                       calpuff_template=calpuff_template,
                       output_dir=output_dir) %>% 
      readLines -> calpuffInp
    
    
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
    grep('^Subgroup \\(13b\\)', calpuffInp) -> psst
    grep('^Subgroup \\(13c\\)', calpuffInp) -> psend
    grep('!', calpuffInp) %>% subset(. %in% psst:psend) -> psln
    if(length(psln) > 0) calpuffInp[-psln] -> calpuffInp
    
    grep('^Subgroup \\(20c\\)', calpuffInp) -> drheader
    grep('!', calpuffInp) %>% subset(. > drheader) -> drln
    if(length(drln) > 0) calpuffInp[-drln] -> calpuffInp
    
    setPuff(calpuffInp, params) -> calpuffInp
    
    #write into file
    outinp <- file.path(output_dir, paste0(source.id,"_elevgen_CALPUFF_7.0.inp"))
    writeLines(calpuffInp, outinp)  
    
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
    }) %>% bind_rows %>% spdf(crs=target_crs)
}


make_toporows <- function(topoXYZ) {
  paste0("Disc",1:nrow(topoXYZ),
         "  !  X = ",
         topoXYZ$Xkm,", ",
         topoXYZ$Ykm,", ",
         format(round(topoXYZ$Elev,1),nsmall=1, scientific=F), 
         " ! ! END !")
}


get_bgconcs = function(sources, mod_dir) {

  sources %<>% spdf
  #retrieve concentrations from Asia nested runs
  #file names
  bgFiles <- list("Max_8-hour_ozone" = "present_mda8.nc",
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
  
  for(i in 1:length(bgFiles)) {
    nc_open(file.path(mod_dir, bgFiles[[i]])) -> nc
    ncvar_get(nc, names(bgFiles)[i]) -> d
    aperm(d, c(2, 3, 1)) -> d
    datamonths = as.Date("2011-01-01") %>% 
      seq.Date(by="day",length.out = 365) %>% 
      month %>% list
    apply(d, c(1, 2), 
          function(x) aggregate(x, by=datamonths, mean)$x) -> d.m
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
      concs[[names(bgFiles)[i]]]
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