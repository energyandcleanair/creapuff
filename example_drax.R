library(creapuff)
require(tidyverse)
require(magrittr)
require(pbapply)

load('~/CALPUFF/Drax.RData')

################################
#CALPUFF
################################
setwd('~/CALPUFF/')
calpuffDir <- "C:/CALPUFF/CALPUFF_v7.2.1_L150618"
calpuffTemplate <- "~/CALPUFF/CALPUFF_7.0_template_Hg.INP"
runinfodir='~/CALPUFF/Vietnam2021/'
dir.create(runinfodir)
if(!exists('outFilesAll')) outFilesAll <- outFiles

outFilesAll$include = outFilesAll$gridName != 'Nam_Dinh_3'

sources = read_xlsx('VN_PDP8_HIA Data v4.xlsx', sheet='CALPUFF input', .name_repair = make.names) %>% 
  mutate_at(c('Lat','Long'), as.numeric) %>% rename(Plant=Plants) %>% filter(!is.na(Plant))

domain_centers = outFilesAll %>% distinct(runName, Lat, Lon)

monthscaling=NULL
OZONE.DAT=NULL

dists = distm(spdf(sources), spdf(domain_centers))
dists %>% apply(1, function(x) domain_centers$runName[which(x==min(x))]) -> sources$runName
dists %>% apply(1, min) -> sources$dist_to_domain_center
sources$dist_to_domain_center %>% summary

adm = getadm(0, 'coarse')
ggplot() + annotation_spatial(adm) + layer_spatial(domPols, fill=NA) + layer_spatial(spdf(sources))

domain_centers %>% get_bgconcs() -> bgconcs_all

sources %<>% rename(Stack.height=contains("stack.height"), 
                    velocity=contains("velocity"),
                    diameter = contains("Diameter"),
                    exit.temp = contains("Temperature"))
sources %<>% 
  mutate(FGD=T,
         CALPUFF.name = Plant %>% gsub(' ', '', .) %>% gsub('[^a-zA-Z0-9]', 'a', .) %>% 
           gsub('extend', 'E', .)) %>% 
  mutate(short.name = CALPUFF.name %>% gsub('Trach', 'TRACH', .) %>% gsub('Tri', 'TRI', .) %>% 
           gsub('[a-z]', '', .) %>% recode('B'='BD')) %>% 
  fill(Province) %>% 
  mutate(Prov_short = Province %>% gsub('[^A-Z]', '', .) %>% recode('N'='ND'))

"FGD	HG0	RGM	Hgp
F	43.9	54	2.1
T	74.2	24	1.8" %>% textConnection %>% read.table(header=T) %>% 
  mutate_if(is.numeric, divide_by, 100) ->
  hg_species

#get receptors for all
nesfactL = c(3, 8, 24)
nesfact_range = c(150, 30, 10)
if(!exists('topoAll')) topoAll = list()

queue = unique(sources$runName)
for(run in queue) {
  outF = outFilesAll %>% filter(runName == run, include) %>% head(1)
  
  targetcrs = getUTMproj(outF$UTMZ, outF$UTMH)
  
  loc = outF %>% spdf %>% spTransform(targetcrs)
  
  #get discrete receptors with 400x400 dim and 1km, 2.5km, 10km resos
  get_recep(loc, nesfactL = nesfactL) -> topoAll[[run]]
  print(run)
}

saveRDS(topoAll, '~/CALPUFF/Vietnam topoAll v2.RDS')
topoAll = readRDS('~/CALPUFF/Vietnam topoAll v2.RDS')

emitted.polls = c('SO2','NO','NO2','PM15','PM10','PPM25','HG0','RGM')

inpfiles_created <- list()
runsources_out=list()
pm10fraction=list()

sources$done=F
queue = sources$runName
for(metrun in queue) {
  sources %>% filter(runName == metrun, !done) %>% spdf -> runsources
  metfiles <- outFilesAll %>% filter(runName == runsources$runName, include)
  
  targetcrs = getUTMproj(metfiles$UTMZ[1], metfiles$UTMH[1])
  runsources %<>% spTransform(targetcrs)
  runsources %>% coordinates() %>% data.frame() %>% 
    set_names('UTMx', 'UTMy') %>% data.frame(runsources@data, .) -> runsources@data
  runsources$base.elevation..msl <- getPlantElev(runsources, metfiles)
  
  runsources@data %<>% mutate(SO2 = SO2_tpa,
                              SO4 = 0,
                              NO  = NOx_tpa * .95 * 30/46,
                              NO2 = NOx_tpa * .05,
                              HNO3 = 0,
                              NO3 = 0,
                              PM15 = PM_tpa * 26/80,
                              PM10 = PM_tpa * 30/80,
                              PPM25 = PM_tpa * 24/80,
                              HG0 = Hg_kgpa * hg_species$HG0[match(runsources$FGD, hg_species$FGD)], 
                              RGM = Hg_kgpa * hg_species$RGM[match(runsources$FGD, hg_species$FGD)],
                              Hgp = Hg_kgpa * hg_species$Hgp[match(runsources$FGD, hg_species$FGD)],
                              downwash = 0)
  
  runsources$exit.temp %<>% (function(x) x+ifelse(x < 273, 273.15, 0))
  
  runsources@data %>% ungroup %>% 
    sel(UTMx, UTMy, Stack.height, base.elevation..msl, diameter, velocity, exit.temp, downwash,
        SO2,SO4,NO,NO2,HNO3,NO3,PM15,PM10,PPM25, HG0, RGM) -> runsources2
  
  runsources2 %>% 
    mutate_all(signif, 6) %>% mutate_all(format, nsmall=1) %>% 
    apply(1, paste, collapse=", ") -> emislines

  #  Source       X         Y       Stack    Base     Stack    Exit  Exit    Bldg.  Emission
  #No.     Coordinate Coordinate Height Elevation Diameter  Vel.  Temp.   Dwash   Rates
  #(km)      (km)       (m)      (m)       (m)  (m/s) (deg. K)         
  #------   ---------- ---------- ------  ------   -------- ----- -------- ----- --------
  
  sourceLines <- list()
  for(i in 1:nrow(runsources))
    sourceLines[[runsources$short.name[i]]] <- c(paste0("! SRCNAM =",runsources$short.name[i],"!"),
                                                 paste("! X =", emislines[i], "!"),
                                                 "! ZPLTFM  =       .0 !",
                                                 "! FMFAC  =      1.0 !   !END!")
  
  topoAll[[metfiles$runName[1]]] -> intopo
  r=raster(extent(intopo), res=1, crs=crs(intopo))
  
  runsources$flag=1
  sourcesR=rasterize(runsources, r, 'flag')
  dist_to_source=distance(sourcesR)
  extract(dist_to_source, intopo) -> intopo$dist_to_source
  
  intopo$include=F
  
  for(i in seq_along(nesfactL))
    intopo$include[intopo$dist_to_source<nesfact_range[i] & intopo$nesfact==nesfactL[i]] <- T
  
  print(paste(metrun, sum(intopo$include), 'receptors'))
  if(sum(intopo$include)+metfiles$GridNX[1]*metfiles$GridNY[1]>=10000) stop('too many receptors!')
  
  #plotadm = getadm(0, 'coarse') %>% cropProj(r)
  
  quickpng(paste0(runinfodir,metrun, ' receptors.png'))
  #plotadm %>% plot(add=T, border='steelblue')
  intopo %>% subset(include) %>% plot(col='gray', cex=.5)
  runsources %>% plot(add=T)
  dev.off()
  
  addparams = list(DATUM="WGS-84")
  
  for(puffrun in runsources$short.name) {
    make_calpuff_inp(metfiles,
                     puffrun=puffrun,
                     bgconcs = bgconcs_all %>% filter(runName == metfiles$runName[1]),
                     OZONE.DAT = OZONE.DAT,
                     sourceLines=sourceLines[[puffrun]],
                     receptors=intopo %>% subset(include) %>% make_toporows,
                     addparams = addparams,
                     addsubgroups = monthscaling) ->
      inpfiles_created[[puffrun]]
    
    pm10fraction[[puffrun]] = sum(runsources$Hgp) / sum(runsources$PM10)
  }
}

inpfiles_created %>% split(1:6) -> batches
setwd(calpuffDir)
for(i in seq_along(batches)) {
  batches[[i]] %>% paste('calpuff_v7.2.1', .) %>% c('pause') %>% 
    writeLines(paste0('VNMv2_', i, '.bat'))
}


#############################
#POST PROCESSING
#############################

#generate POSTUTIL and CALPOST .inp files
pu.dir <- "C:\\CALPUFF\\POSTUTIL_v7.0.0_L150207\\"
pu.t <- c("Mintia_postutilRepartition.inp",
          "Mintia_postutil_depo.inp",
          "Mintia_postutil_PM10.inp")
pu.exe <- 'postutil_v7.0.0'
pu.inp.out <- gsub("^[^_]*", "", pu.t)

#make.time.series = "T"
#pm10fraction = sources$Hg_kgpa*1.8e-2/(sources$PM_tpa*30/80)
#nper = 8760
#start_hour <- 1

cp.dir <- "C:\\Calpuff\\CALPOST_v7.1.0_L141010/"
cp.t<- c("Mintia_AllOutput_calpost.inp",
         "Mintia_depo_calpost.inp")
cp.exe <- 'calpost_v7.1.0'
cp.inp.out <- gsub("^[^_]*", "", cp.t)

get_cp_period = function(params) {
  runyr = as.numeric(params$val[params$name=='ISYR']) + ifelse(params$val[params$name=='ISMO']==12, 1, 0)
  list(start = paste0(runyr, '-01-01 0') %>% ymd_h,
       end = paste0(runyr+1, '-01-01 0') %>% ymd_h)
}

METRUN = 0 #set 1 to run CALPOST on all periods in file
discrete_receptors=T

#generate POSTUTIL and CALPOST .inp files
sources$COD %>% force.numeric %>% is_greater_than(2019) %>% 
  ifelse('NEW', 'OPR') -> sources$status

runNames <- c(names(inpfiles_created), 
              unique(paste0(sources$status[!sources$done], '_', sources$Prov_short[!sources$done])))

for(s in runNames) {
  runName = s %>% gsub(' ', '', .) %>% substr(1,8)
  province = s %>% gsub('.*_', '', .)

  cp.runName <- runName #%>% (function(x) { paste0(substr(x, 1, 6), ifelse(grepl("_sens", x), "_s", "")) })
  is.cluster = s %notin% sources$short.name
  make.time.series = is.cluster
  
  metrun = sources$runName[sources$short.name==s]
  if(is.cluster) metrun = unique(sources$runName[sources$Prov_short==province])
  metFiles = outFilesAll %>% filter(runName == metrun)
  runDir = metFiles$dir[1]
  
  setwd(pu.dir)
  
  params <- data.frame(name=NA,val=NA)
  inparams <- data.frame(cpuname=NA,name=NA)
  
  #read CALPUFF.INP
  puffrun = s
  if(is.cluster) puffrun = sources$short.name[sources$runName==metrun][1]
  puffInp <- paste0(calpuffDir, "/", puffrun,"_CALPUFF_7.0.inp") %>% readLines()
  
  #set params read from CALPUFF.INP
  inparams[1,c('name', 'cpuname')] <- c("ISYR","IBYR")
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("ISMO","IBMO")
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("ISDY","IBDY")
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("ISHR","IBHR")
  
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("IEYR","IEYR")
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("IEMO","IEMO")
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("IEDY","IEDY")
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("IEHR","IEHR")
  
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("ABTZ","ABTZ")
  
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("BCKNH3","BCKNH3")
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("MODDAT","CONDAT")
  inparams[nrow(inparams)+1,c('name', 'cpuname')] <- c("UTLMET","METDAT1")
  
  params <- inparams
  params$val <- NA
  
  for(p in 1:nrow(inparams)) {
    puffInp %>% gsub(" ", "", .) %>% 
      getParamVal(params$cpuname[p], .) -> params$val[p]
  }
  
  params$val[params$name == 'ISHR'] %<>% as.numeric %>% add(2)
  
  nper <- difftime(paste(params$val[match(c('ISYR', 'ISMO', 'ISDY', 'ISHR'), params$name)], collapse = ' ') %>% ymd_h,
    paste(params$val[match(c('IEYR', 'IEMO', 'IEDY', 'IEHR'), params$name)], collapse = ' ') %>% ymd_h, units='hours') %>% 
    as.numeric() %>% abs %>% subtract(3)
  
  params %>% dplyr::select(-cpuname) %>% filter(!grepl("^IE", name)) -> params
  conF <- paste0(runDir, runName, '.CON')
  
  #set params determined by program
  params[nrow(params)+1,] <- c('UTLLST', gsub("\\.CON", "_POSTUTIL_REPART.LST", conF))
  params[nrow(params)+1,] <- c('UTLDAT', gsub("\\.CON", "_repart.CON", conF))
  params[nrow(params)+1,] <- c('NPER', nper)
  
  if(is.cluster) {
    include_status = 'OPR'
    if(grepl('NEW', s)) include_status = 'NEW'
    runsources = sources %>% filter(Prov_short == province, status %in% include_status)
    
    #create POSTUTIL file to sum up concentrations for different runs
    params %>% dplyr::filter(name %notin% c('BCKNH3')) -> sumparams
    sumparams$val[sumparams$name == 'UTLMET'] <- 'not set'
    sumparams[nrow(sumparams)+1,] <- c('NMET', 0)
    sumparams[nrow(sumparams)+1,] <- c('NFILES', nrow(runsources))
    
    sumparams$val[sumparams$name == 'UTLDAT'] <- conF
    sumparams$val[sumparams$name == 'UTLLST'] <- gsub('\\.CON', '_sumRuns.LST', conF)
    
    paste0(runDir, runsources$short.name, '.CON') -> MODDAT
    
    pu.inp.sums = 'Mintia_postutilSumruns.inp'
    pu.inp.out.sums = paste0(runName, '_postutilSumruns.inp')
    write.inp(pu.inp.sums, 
              pu.inp.out.sums,
              sumparams)
    
    pu.inp.out.sums %>% readLines -> inp
    grep('! *MODDAT', inp) -> ln
    modlines  = paste0('! MODDAT = ', MODDAT, ' ! !END!')
    c(inp[1:(ln-1)], modlines, inp[-1:-ln]) %>% writeLines(pu.inp.out.sums)
    
    params[params$name == 'MODDAT', 'val'] <- sumparams[sumparams$name == 'UTLDAT', 'val']
  }
  
  
  #write repartitioning INP file
  write.inp(pu.t[1], 
            paste0(runName, pu.inp.out[1]),
            params)
  
  #write INP file to calculate total PM
  params[params$name == 'MODDAT', 'val'] <- params[params$name == 'UTLDAT', 'val']
  params[params$name == 'UTLDAT', 'val'] <- gsub("\\.CON", "_TotalPM.CON", conF)
  params[params$name == 'UTLLST', 'val'] %<>% gsub("REPART", "TotalPM", .)
  params[!(params$name %in% c('BCKNH3', 'UTLMET')), ] -> params

  write.inp(pu.t[3], 
            paste0(runName, pu.inp.out[3]),
            params)
  
  #write deposition INP file
  pu.depo.out = paste0(runName, pu.inp.out[2])
  params[params$name == 'UTLDAT', 'val'] <- gsub("\\.CON", "_Depo.FLX", conF)
  params[params$name == 'MODDAT', 'val'] <- gsub("\\.CON", ".WET", conF)
  params[params$name == 'UTLLST', 'val'] %<>% gsub("TotalPM", "Depo", .)
  
  params[nrow(params)+1,] <- c('MODDAT', gsub("\\.CON", ".DRY", conF))
  write.inp(pu.t[2], 
            pu.depo.out,
            params)
  
  #add the Hg fraction in PM
  if(!is.cluster) {
    pu.depo.out %>% readLines -> pu.depo
    pu.depo %>% gsub(' ', '', .) %>% grep("!CSPECCMP=Hg!", .) -> hgcmp_startline
    pu.depo %>% gsub(' ', '', .) %>% grep("!PM10=", .) %>% 
      subset(.>hgcmp_startline) %>% head(1) -> hgcmp_pm10line
    pu.depo[hgcmp_pm10line] = paste("!    PM10  =     ",pm10fraction[[s]]," !")
    writeLines(pu.depo, pu.depo.out)
  }
  
  #make CALPOST INP files
  setwd(cp.dir)
  
  cp.period = get_cp_period(params)
  
  params %<>% subset(name == 'ABTZ')
  params[nrow(params)+1,] <- c('METRUN', METRUN)
  
  if(METRUN == 0) {
    params[nrow(params)+1,] <- c('ISYR', year(cp.period$start))
    params[nrow(params)+1,] <- c('ISMO', month(cp.period$start))
    params[nrow(params)+1,] <- c('ISDY', day(cp.period$start))
    params[nrow(params)+1,] <- c('ISHR', hour(cp.period$start))
    
    params[nrow(params)+1,] <- c('IEYR', year(cp.period$end))
    params[nrow(params)+1,] <- c('IEMO', month(cp.period$end))
    params[nrow(params)+1,] <- c('IEDY', day(cp.period$end))
    params[nrow(params)+1,] <- c('IEHR', hour(cp.period$end))
  }
  
  params[nrow(params)+1,] <- c('MODDAT', gsub("\\.CON", "_TotalPM.CON", conF))
  params[nrow(params)+1,] <- c('TSUNAM', cp.runName)
  params[nrow(params)+1,] <- c('TUNAM', cp.runName)
  params[nrow(params)+1,] <- c('LTIME', ifelse(make.time.series, 'T', 'F'))
  params[nrow(params)+1,] <- c('TSPATH', ifelse(make.time.series, gsub("[^/]*$","", conF),"not set"))
  params[nrow(params)+1,] <- c("L1HR", "T, F, T, T")
  params[nrow(params)+1,] <- c("L24HR", "T, T, T, T")
  params[nrow(params)+1,] <- c("LD", discrete_receptors %>% as.character %>% substr(1,1))
  
  #write INP file to get all concentration outputs
  write.inp(cp.t[1], 
            paste0(runName, cp.inp.out[1]),
            params,
            set.all=F)
  
  params$val[params$name=="L1HR"] <- "F, F, F"
  params$val[params$name=="L24HR"] <- "F, F, F"
  
  #write INP file to get all deposition outputs
  if(!is.cluster) {
    params[params$name == 'MODDAT', 'val'] <- gsub("\\.CON", "_Depo.FLX", conF)
    write.inp(cp.t[2], 
              paste0(runName, cp.inp.out[2]),
              params,
              set.all=F)
  }
}



#make bat files to run POSTUTIL and CALPOST
sumRunNames = runNames %>% grep('_', ., value=T)

batches = 6
batchsize = ceiling(length(runNames) / batches)
jobname <- "VNMv2"

for(batch in 1:batches) {
  startN <- (batchsize*(batch-1)+1)
  source.subset <- startN:(startN+batchsize-1)
  source.subset <- source.subset[source.subset %in% 1:length(runNames)]
  sum.subset = sumRunNames %whichin% runNames[source.subset]
  
  if(length(source.subset)>0) {
    setwd(pu.dir)
    
    pu.runs=NULL
    if(length(sum.subset)>0) sum.subset %>% paste0('_postutilSumruns.inp') -> pu.runs
    expand.grid(runNames[source.subset], pu.inp.out) %>% apply(1, paste, collapse="") %>% c(pu.runs, .) -> pu.runs
    writeLines(c(paste0(pu.exe, " ", pu.runs), "pause"), 
               paste0(jobname, "_batch_", batch, ".bat"))
    
    setwd(cp.dir)
    expand.grid(runNames[source.subset], cp.inp.out) %>% apply(1, paste, collapse="") -> cp.runs
    writeLines(c(paste0(cp.exe, " ", cp.runs), "pause"), 
               paste0(jobname, "_batch_", batch, ".bat"))
  }
}

list.files(path=cp.dir, pattern='rank.*\\.csv') %>% gsub('\\.csv', '', .) %>% 
  gsub('.*(conc|tflx)_', '', .) -> allpuffs
tolower(runNames) %whichnotin% allpuffs

sources %>% left_join(outFilesAll %>% distinct(runName, UTMH, UTMZ)) %>% 
  write_csv('~/CALPUFF/Vietnam 2021 modeled sources v3.csv')
