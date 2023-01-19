
plot_contours <- function(calpuff_files,
                          plot_bb,
                          titletxt = calpuff_files %>% make_titletxt %>% gsub('\n', ' ', .),
                          contour_type='both',
                          point_sources=NULL,
                          area_sources=NULL,
                          basemap=get_basemap(plot_bb),
                          color_scale=c(crea_palettes$change[4:7]),
                          fill_alpha_function = (function(x) x^.25*.4),
                          contour_breaks_function=make_contour_breaks,
                          include_threshold_as_break=T,
                          output_dir='.') {
  
  plot_bb_3857 = plot_bb %>% raster(crs='+init=EPSG:4326') %>% projectExtent(crs='+init=EPSG:3857') %>% 
    extent
  
  for(i in seq_along(calpuff_files$path)) {
    #prepare the raster to plot
    calpuff_files$path[i] %>% raster %>% multiply_by(calpuff_files$plotscale) -> r
    plotunit = calpuff_files$plotunit %>% gsub('ug', 'µg', .)
    
    max_val <- r[is.na(area_sources_raster)] %>% max
    
    if(!is.null(area_sources)) r[!is.na(area_sources_raster)] %<>% pmin(max_val)
    
    r %<>% projectRaster(crs = '+init=EPSG:3857') %>% crop(plot_bb_3857*1.2)
    
    r %>% as('SpatialGridDataFrame') %>% as_tibble -> concs_df
    names(concs_df) <- c('value', 'lon', 'lat')
    
    #prepare contour levels and color scales
    levels_to_include <- NULL
    if(include_threshold_as_break) levels_to_include <- calpuff_files$threshold.plotunit[i]
    brks <- contour_breaks_function(r, levels_to_include=levels_to_include)
    colRamp <- colorRampPalette(color_scale)(length(brks))
    alphas <- seq(0,1,length.out=length(brks)+1)[-1] %>% fill_alpha_function
    fillRamp <- colRamp %>% col.a(alphas)
    
    #initialize source data.frames
    area_sources_df <- tibble()
    point_sources_df <- tibble()
    
    #make the plot
    message(titletxt[i])
    
    map_plot <- ggmap(basemap)
    
    if(!is.null(area_sources)) {
      area_sources_df <- area_sources %>% st_centroid() %>% 
        bind_cols(st_coordinates(.)) %>% 
        select(-any_of(c('lon', 'lat'))) %>% st_drop_geometry() %>% rename(lon=X, lat=Y)
      
      map_plot = map_plot + annotation_spatial(area_sources, fill='darkgray')
    }
    
    if(contour_type %in% c('filled', 'both')) map_plot = map_plot + 
      geom_contour_filled(data=concs_df, aes(lon, lat, z=value), breaks=c(brks, max_val))
    if(contour_type %in% c('lines', 'both')) map_plot = map_plot + 
      metR::geom_contour2(data=concs_df, aes(lon, lat, z=value, col=as.factor(..level..), label=..level..), breaks=brks, show.legend = T, label_color='white')
    
    if(!is.null(point_sources)) {
      point_sources_df <- point_sources %>% bind_cols(st_coordinates(.)) %>% 
        select(-lon, -lat) %>% st_drop_geometry() %>% rename(lon=X, lat=Y, source=plant)
      map_plot = map_plot + annotation_spatial(point_sources, col='red')
    }
    
    sources_to_label <- bind_rows(point_sources_df, area_sources_df)
    if(nrow(sources_to_label)>0) {
      map_plot = map_plot + 
        geom_label_repel(data=bind_rows(point_sources_df, area_sources_df), mapping=aes(label=source, label = source),
                         box.padding = 2)
    }
    
    map_plot +
      coord_sf(xlim=c(plot_bb_3857@xmin, plot_bb_3857@xmax),
               ylim=c(plot_bb_3857@ymin, plot_bb_3857@ymax)) +
      theme(panel.border = element_rect(fill=NA, color='black')) +
      labs(title=str_wrap(titletxt, 45), x='', y='') +
      scale_color_manual(values=colRamp, name=plotunit) +
      scale_fill_manual(values=fillRamp, guide=F) +
      labs(title=str_wrap(titletxt[i], 45), x='', y='') +
      theme_crea() ->
      map_plot
    
    quicksave(file.path(output_dir, paste0(titletxt[i],'.png')), 
              plot=map_plot, width = 10, height = 8) 
    print(map_plot)
  }
}


get_basemap <- function(plot_bb, maptype = 'hybrid', source='google', ...) {
  plot_bb %>% as.matrix() %>% as.vector() %>% get_map(maptype = maptype, source=source, ...) %>% ggmap_bbox
}


make_contour_breaks <- function(r, levels_to_include=NULL) {
  sections <- r[] %>% quantile(p=c(0, .5,.9,.985))
  
  c(pretty(sections[1:2], n=3), 
    pretty(sections[2:3], n=3), 
    pretty(sections[3:4], n=3)) %>% 
    subset(.<=max(r[]) & .>sections[1]) %>% c(levels_to_include) %>% unique %>% sort
}

ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}
