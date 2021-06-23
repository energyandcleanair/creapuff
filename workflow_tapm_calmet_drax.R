library(creapuff)
require(tidyverse)
require(magrittr)
require(pbapply)

load('~/CALPUFF/Drax.RData')
tapmDir <- "C:/tapm/"
caltapmDir <- "C:/Calpuff/CALTAPM_v7.0.0_L150330/"
calmetDir <- "C:/Calpuff/CALMET_v6.5.0_L150223/"
runDir <- "F:/TAPM/Drax/"
calmetTemplate <- list(noobs = "CALMET_template.INP", surfobs = "CALMET_surfObs_template.inp")

#function to extract the runName (common name of all the domains in a TAPM run) from the .out and .def file names
get_runName <- function(x) { x %>% gsub("_?[0-9]*","", .) %>% 
    gsub(paste0(outFileExt, "|\\.def"), "", .) %>% 
    gsub('H$', 'H2', .) }

#find all .def files and .out files in the run directory and subdirectories
outFiles = get_outFiles(runDir, outFileExt="\\.out", batchSubset=NULL)

#make bat file to generate .geo and .outa files with TAPM utilities
make_outa(outFiles, onlyMakeAdditionalFiles=T)

setwd(caltapmDir)
queue = !file.exists(gsub('\\.out', '.M3D', outFiles$outFilePath))
paste0("caltapm_v7.0.0 ",outFiles$gridName[queue],"_CALTAPM.inp") %>% 
  c('pause') %>% writeLines('Drax_CALTAPM.bat')

#check .pr1 files for crashed runs
setwd(runDir)
outFiles$outFilePath %>% gsub(".*/", "", .) %>% gsub("\\.out", "", .) %>% 
  paste0(".*\\.pr1") %>% sapply(function(x) list.files(pattern=x, recursive=T)[1]) %>% 
  subset(!is.na(.)) %>% 
  pbapply::pblapply(function(f) f %>% data.table::fread() %>% tail(1))


#read data from  .geo files
outFiles %<>% read_TAPM_GEO(queue = 1:nrow(outFiles), backup=T)

#overwrite the original .geo files with grid params from outFiles dataframe and with correct spacing
outFiles %>% make_geo

#generate SURF.DAT files
outFiles %>% arrange(GridD) %>% distinct(runName, .keep_all=T) %>% 
  mutate_at(vars(contains('Date')), ymd) -> cases
cases_to_process=1:nrow(cases) #numeric(0)

source('~/CALPUFF/PuffR/TAPM sur data to PuffR csv.R')
source('~/CALPUFF/PuffR/calmet_surface_met-generator-TAPM.R')
cases$surf.dat = lapply(cases$dir, 
                        list.files, 
                        pattern = "surf--.*\\.txt$", 
                        recursive = T, 
                        full.names = T) %>% 
  sapply(function(x) {if(length(x)==0) x=NA; x})

for(case in cases_to_process) {
  runDir = cases$outFilePath[case] %>% gsub('[^/]*$', '', .)
  list.files(path=runDir, pattern=paste0(cases$gridName[case], '.*\\.sur$'),
             full.names=T) %>% 
    sur2csv(lat=cases$Lat[case],
            lon=cases$Lon[case])
  
  cases$surf.dat[case] = 
    calmet_surface_met(location_name=cases$runName[case],
                       start_time <- ymd_h(paste(cases$StartDate[case], 0), tz = "GMT"),
                       end_time <- ymd_h(paste(cases$EndDate[case], 23), tz = "GMT"),
                       lat_dec_deg = cases$Lat[case],
                       lon_dec_deg = cases$Lon[case],
                       domain_height_m = 300e3,
                       domain_width_m = 300e3,
                       time_offset=cases$TZ[case],
                       local_archive_dir=runDir %>% paste0('NCDCsurf/'),
                       use_CSV_files = T,
                       min_hours = 2000)
}

cases %>% sel(runName, surf.dat) %>% left_join(outFiles, .) -> outFiles

  
#read template CALMET file and generate inp files for all grids with info from outFiles dataframe
make_calmet_inp(outFiles)

    
#generate QA3D data by running CALMET with incorrect grid settings
outFiles %<>% generate_QA3D(queue)

#analyse QA3D
outFiles %<>% analyse_QA3D(queue)

#update CALMET.INP and GEO.DAT
outFiles %>% make_geo()
outFiles %>% make_calmet_inp()


#create .bat file to run CALMET
setwd(calmetDir)
for(i in queue)
  writeLines(paste0("calmet_v6.5.0 ",outFiles[i, "gridName"],"_CALMET.inp"), 
             paste0(calmetDir,outFiles$gridName[i],"_CALMET.bat"))

#group into batches to run in parallel
batches = 2
batchsize = ceiling(length(queue) / batches)
for(batch in 1:batches) {
  startN <- (batchsize*(batch-1)+1)
  source.subset <- startN:(startN+batchsize-1)
  source.subset <- source.subset[source.subset %in% 1:length(queue)]
  
  if(length(source.subset) > 0) {
    outFiles$gridName[queue][source.subset] %>% paste0("_CALMET.bat") %>% 
      sapply(readLines) %>% 
      c("pause") %>% 
      writeLines(paste0("batch_", batch, ".bat"))
    
  }
}

#make domain polygons
grids = list()

outFilesAll %>% group_by(gridName) %>% 
  group_map(function(df, gridName) {
    df %>% mutate(xmx=GridX+GridD*GridNX,
                  ymx=GridY+GridD*GridNY) %>% 
      sel(GridX, xmx, GridY, ymx) %>% unlist %>% 
      extent %>% as('SpatialPolygons') -> utmdomain
    crs(utmdomain) = getUTMproj(df$UTMZ, df$UTMH)
    utmdomain@polygons[[1]]@ID = as.character(gridName)
    utmdomain %>% spTransform(llproj)
  }) %>% Reduce(rbind, .) -> domPols

