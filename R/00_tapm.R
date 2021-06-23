
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
read_tapm_geo = function(out_files, queue=1:nrow(out_files), backup=T) {
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
make_geo <- function(out_files, queue=1:nrow(out_files)) {
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