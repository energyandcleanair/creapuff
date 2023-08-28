get_deposition_results <- function(calpuff_files, dir, wdpa_areas=NULL, long_format=T){
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
  
  if(is.logical(wdpa_areas)) {
    if(wdpa_areas) {
      grids <- get_grids_calpuff(calpuff_files)
      wdpa_areas <- get_wdpa_areas(grids)
    } else  wdpa_areas <- NULL
  }
  
  if(!is.null(wdpa_areas)) {
    
    #WDPA database extract
    raster::extract(depoR, wdpa_areas, sum) %>% data.frame -> protdepo
    depoR %>% magrittr::divide_by(area(.)) %>% set_names(names(depoR)) %>% raster::extract(wdpa_areas, mean) %>% data.frame -> protdepo_per
    depoR %>% magrittr::divide_by(area(.)) %>% set_names(names(depoR)) %>% raster::extract(wdpa_areas, max) %>% data.frame -> protdepo_maxper
    
    if(long_format) {
      bind_rows(protdepo %>% mutate(wdpa_area=wdpa_areas$NAME, variable='total deposition'),
                protdepo_per %>% mutate(wdpa_area=wdpa_areas$NAME, variable='average deposition'),
                protdepo_maxper %>% mutate(wdpa_area=wdpa_areas$NAME, variable='maximum local deposition')) %>% 
        pivot_longer(where(is.numeric)) %>% 
        separate(name, c('species', 'scenario'), sep='_') %>% 
        mutate(unit = case_when(species=='hg' & variable == 'total deposition' ~ 'kg',
                                variable=='total deposition'~'t',
                                species=='hg'~'mg/km2',
                                T~'kg/km2'),
               value = value * case_when(species=='hg' & variable == 'total deposition'~1e-6,
                                         variable == 'total deposition'~1e-3,
                                         T~1)) ->
        protdepo
    } else {
      names(protdepo) <- names(depoR) %>% paste0('_', units, '_total')
      names(protdepo_per) <- names(depoR) %>% paste0('_', units, '_per.km2')
      names(protdepo_maxper) <- names(depoR) %>% paste0('_', units, '_maxper.km2')
      wdpa_areas$NAME -> protdepo$name
      
      protdepo %<>% bind_cols(protdepo_per, protdepo_maxper)
    }
    
    protdepo %>% write_csv(file.path(dir, 'WDPA areas deposition.csv'))
  }
  
  return(list(by_landuse = deposums,
              into_protected_areas = protdepo))
}



