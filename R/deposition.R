get_deposition_results <- function(calpuff_files, dir, add_wdpa_areas=T){
  #deposition totals
  calpuff_files %>% subset(type=='deposition') -> depodf
  depodf$path %>% stack %>% fixproj -> depoR
  
  depoR * area(depoR) * 10^2 -> depoR #hectares to km2
  names(depoR) <- paste0(depodf$species,'_',depodf$scenario)
  
  #get deposition by land use type
  raster(file.path(creahelpers::get_gis_dir(),'hia/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif')) %>% 
    cropProj(depoR, method='ngb') -> lc.utm
  read.csv(file.path(creahelpers::get_gis_dir(),'hia/ESACCI-LC-Legend.csv'),sep=';',stringsAsFactors = F) -> lc.leg
  
  adm_utm <- creahelpers::get_adm(0, res="low") %>% cropProj(grids$gridR)
  adm_utm$ID <- adm_utm$GID_0 %>% as.factor %>% as.numeric
  landmask <- rasterize(adm_utm, depoR, "ID")
  lc.utm[lc.utm==210 & is.na(landmask)] <- 230
  
  zonal(depoR, lc.utm, sum) -> depo.lc
  depo.lc %>% as.data.frame %>%
    dplyr::rename(NB_LAB = zone) %>%
    left_join(lc.leg) -> depo.lc
  
  depo.lc %>% write_csv(file.path(dir, 'deposition by land use, detailed breakdown.csv'))
  
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
  
  deposums %>% write.csv(file.path(dir, 'deposition by broad land use category.csv'))
  
  protdepo=NULL
  if(add_wdpa_areas) {
    
    grids <- get_grids_calpuff(calpuff_files)
    get_wdpa_areas(grids)
    
    #WDPA database extract
    units = ifelse(grepl('hg', names(depoR)), 'mg', 'kg')
    raster::extract(depoR, wdpa_areas, sum) %>% data.frame -> protdepo
    names(protdepo) <- names(depoR) %>% paste0('_', units, '_total')

    depoR %>% magrittr::divide_by(area(.)) %>% raster::extract(wdpa_areas, mean) %>% data.frame -> protdepo_per
    names(protdepo_per) <- names(depoR) %>% paste0('_', units, '_per.km2')
    depoR %>% magrittr::divide_by(area(.)) %>% raster::extract(wdpa_areas, max) %>% data.frame -> protdepo_maxper
    names(protdepo_maxper) <- names(depoR) %>% paste0('_', units, '_maxper.km2')
    protdepo %<>% bind_cols(protdepo_per, protdepo_maxper)
    wdpa_areas$NAME -> protdepo$name
    
    protdepo %>% write_csv(file.path(dir, 'WDPA areas deposition.csv'))
  }
  
  return(list(by_landuse = deposums,
              into_protected_areas = protdepo))
}



