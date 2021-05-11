todo <- function(){
  #deposition totals
  file_species %>%
    subset(type=='deposition') -> depodf
  depodf %>%
    subset(select='name',drop=T) %>%
    gsub('.csv','.tif',.) %>% stack -> depoR
  crs(depoR) <- crs(gridR)
  
  depoR * area(depoR) * 10^2 -> depoR #hectares to km2
  names(depoR) <- paste0(depodf$species,'_',depodf$scenario)
  
  #get deposition by land use type
  raster(file.path(hia_raster_dir,'ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif')) -> lc
  projectRaster(crop(lc,gridLL), gridR, method='ngb') -> lc.utm
  read.csv(file.path(hia_raster_dir,'ESACCI-LC-Legend.csv'),sep=';',stringsAsFactors = F) -> lc.leg
  
  admUTM$ID <- admUTM$GID_0 %>% as.factor %>% as.numeric
  landmask <- rasterize(admUTM, depoR, "ID")
  lc.utm[lc.utm==210 & is.na(landmask)] <- 230
  
  zonal(depoR, lc.utm, sum) -> depo.lc
  depo.lc %>% as.data.frame %>%
    dplyr::rename(NB_LAB = zone) %>%
    left_join(lc.leg) -> depo.lc
  
  depo.lc %>% write_csv('deposition by land use, detailed breakdown.csv')
  
  depo.lc$broad.cat <- 'other'
  depo.lc[depo.lc$NB_LAB %in% 10:30,'broad.cat'] <- 'cropland'
  depo.lc[depo.lc$NB_LAB %in% c(50:100, 160:170),'broad.cat'] <- 'forest'
  depo.lc[depo.lc$NB_LAB == 210,'broad.cat'] <- 'fresh water'
  depo.lc[depo.lc$NB_LAB == 230,'broad.cat'] <- 'ocean'
  depo.lc %>%
    group_by(broad.cat) %>%
    summarise_if(is.double,sum, na.rm=T) %>%
    dplyr::select(-NB_LAB) %>%
    gather(var, deposition, -broad.cat) %>%
    separate(var, c("pollutant", "scenario"), "_") %>%
    mutate(deposition = deposition / ifelse(pollutant=='hg',1e6, 1e3)) %>%
    mutate(unit = ifelse(pollutant=='hg', 'kg/yr', 't/yr')) -> deposums
  
  deposums %>% write.csv('deposition by broad land use category.csv')
  
  deposums %>% ungroup %>%
    group_by(ocean = (broad.cat == 'ocean'),
             pollutant, scenario, unit) %>%
    summarize_at('deposition', sum)
  
  depoR %>% subset(grep('hg_', names(.))) %>% divide_by(100) -> hgdepo
  hgdepo %>% as.list -> hgdepolist
  names(hgdepolist) <- names(hgdepo)
  hgdepolist %>%
    lapply(function(r) area(r)[r>125] %>% sum) %>% data.frame %>%
    write_csv('hg above 125, km2.csv')
  
  #WDPA database extract
  '~/GIS/WDPA/WDPA_Mar2020-Philippines.RDS' %>% readRDS() -> prot
  prot %<>% spTransform(crs(gridR))
  
  extract <- raster::extract
  units = ifelse(grepl('hg', names(depoR)), 'mg', 'kg')
  extract(depoR, prot, sum) %>% data.frame -> protdepo
  names(protdepo) <- names(depoR) %>% paste0('_', units, '_total')
  depoR %>% divide_by(area(.)) %>% extract(prot, mean) %>% data.frame -> protdepo_per
  names(protdepo_per) <- names(depoR) %>% paste0('_', units, '_per.km2')
  depoR %>% divide_by(area(.)) %>% extract(prot, max) %>% data.frame -> protdepo_maxper
  names(protdepo_maxper) <- names(depoR) %>% paste0('_', units, '_maxper.km2')
  protdepo %<>% bind_cols(protdepo_per, protdepo_maxper)
  prot$NAME -> protdepo$name
  protdepo %>% arrange(desc(hg_oprnew_a_mg_total)) %>% head
  
  protdepo %>% write_csv('WDPA areas deposition.csv')
}



