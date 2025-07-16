#' Output contour plots of CALPUFF results
#' @param calpuff_files Data.frame of tif rasters of CALPUFF results, returned by creapuff::get_calpuff_files. The additional columns "title" and "subtitle" are used for the plots, if provided. If "title" column is not present, creapuff::make_titletxt is used to generate titles, and plot_filename to specify the name . Can be grouped, to control faceting.
#' @param plot_bb Latitude-longitude bounding box for the plot area. An Extent class object or other valid first argument to raster::raster().
#' @param contour_type Either 'lines', 'filled' or 'both' (default).
#' @param point_sources Point sources to show on the map. An sf points object or a function that returns one when given a subset of the calpuff_files data.frame as an argument. The function option is convenient when different scenarios have a different subset of modeled sources included. The column "source" is used to the sources if label_sources==TRUE.
#' @param area_sources An sf polygons object. The area inside the polygons (e.g. a coal mine) is excluded from raster values when determining the contour levels. The column "source" is used to label area_sources if label_sources==TRUE.
#' @param basemap Basemap for the plot. A ggmap object. If not provided, retrieved as creapuff::get_basemap(plot_bb).
#' @param facet_by='' A column name in calpuff_files to facet the plots by. E.g. facet_by='scenario_description' will put all the maps for the same pollutant and averaging period into one plot, with different facets for the different scenarios.
#' @param facet_ncol Number of columns in faceted plots.
#' @param contour_breaks Either a numeric vector specifying the contour breaks, or a function that returns the breaks when provided a raster object, and accepts the arguments color_scale_basis_raster, levels_to_include and probs. Default is the creapuff::make_contour_breaks() function.
#' @param contour_break_probs Quantiles to use when generating contour breaks. Numeric vector as in stats::quantile.
#' @param label_contours Should contour levels be labeled on the map? Default TRUE
#' @param label.placer=label_placer_flattest() Function to determine the places of contour level labels, passed on to metR::geom_contour2()
#' @param skip_labels=1 number of contours to skip for labelling. E.g. skip = 1 (default) will skip 1 contour line between labels. Passed onto metR::geom_contour2().
#' @param color_scale Color scale to use for the contour lines and fill. A vector of colors passed to colorRampPalette(). Default: rcrea::crea_palettes$change[4:7].
#' @param color_scale_basis_scenario Name of scenario used to set contour levels for all rasters with the same pollutant and averaging period. Typically, the scenario with the highest modeled emissions. Refers to the column calpuff_files$scenario.
#' @param fill_alpha_function Function giving the relationship between the contour variable (concentration or deposition) and the alpha of the contours. Should return the desired alpha values (0 to 1) when given a sequence of values from 0 to 1 as input, with 0 corresponding to variable value 0 and 1 to the maximum value. Default: (function(x) x^.25*.4).
#' @param include_threshold_as_break Should the "threshold" in calpuff_files be included as a contour break? Default TRUE.
#' @param label_sources Should labels be plotted for the point_sources and area_sources.
#' @param source_marker_linewidth, source_marker_size, source_marker_alpha, source_label_color='orange' Parameters controlling the appearance of the source markers and labels.
#' @param title_width_characters Maximum number of characters after which title is wrapped to multiple lines
#' @param output_dir Output directory path.
#' @param quicksave_options Named list of parameters controlling the appearance of the plot. Passed onto rcrea::quicksave().
#'
#' @details
#' 
#' @returns Nothing. Image files are written into output_dir
#' 
#' @export
#'
#' @examples
#' 
plot_contours <- function(calpuff_files,
                          plot_bb,
                          contour_type='both',
                          point_sources=NULL,
                          area_sources=NULL,
                          basemap=get_basemap(plot_bb),
                          facet_by='',
                          facet_ncol=NULL,
                          contour_breaks=make_contour_breaks,
                          contour_break_probs=c(0, .33,.8,.995),
                          label_contours=T,
                          label.placer=label_placer_flattest(),
                          skip_labels=1,
                          color_scale=c(crea_palettes$change[4:7]),
                          color_scale_basis_scenario=NULL,
                          fill_alpha_function = (function(x) x^.25*.4),
                          include_threshold_as_break=T,
                          label_sources=T,
                          source_marker_linewidth=1, source_marker_size=1, source_marker_alpha=1,
                          source_label_color='orange',
                          title_width_characters=60,
                          contour_guide=guide_legend(),
                          output_dir='.',
                          ggplot_theme=NULL,
                          quicksave_options=list(width=8, height=6, scale=1.33, logo=T)) {
  plot_crs=3857
  plot_bb_3857 = plot_bb %>% raster(crs='+init=EPSG:4326') %>% projectExtent(crs='+init=EPSG:3857') %>% 
    extent
  
  if(is.null(calpuff_files$title)) {
    include_scenario_in_title = T
    if(facet_by %in% c('scenario', 'scenario_description')) include_scenario_in_title = F
    calpuff_files$title <- calpuff_files %>% make_titletxt(include_scenario=include_scenario_in_title, 
                                                           line_break = F)
  }
  
  if(is.null(calpuff_files$subtitle)) calpuff_files$subtitle <- ''
  if(!is.null(point_sources) & !is.function(point_sources)) point_sources %<>% st_transform(plot_crs)
  
  point_sources_fun <- NULL
  contour_breaks_fun <- NULL
  if(!is.null(point_sources) & is.function(point_sources)) point_sources_fun <- point_sources
  if(is.function(contour_breaks)) contour_breaks_fun <- contour_breaks
  
  if(!is.null(area_sources)) {
    gridR <- calpuff_files$path[1] %>% raster %>% raster
    area_sources %<>% st_transform(plot_crs)
    area_sources_raster <- area_sources %>% st_zm() %>% st_transform(crs(gridR)) %>% raster::rasterize(gridR)
  }
  
  if(!is_grouped_df(calpuff_files))  {
    vars_to_group_by <- c('species', 'type', 'period', 'scenario')
    if(facet_by != '') vars_to_group_by %<>% subset(. != facet_by)
    if(facet_by %in% c('scenario', 'scenario_description')) vars_to_group_by <- c('species', 'type', 'period')
    
    calpuff_files %<>% group_by(across(any_of(vars_to_group_by)))
  }
  
  calpuff_files %<>% group_split
  plot_filenames <- character()
  
  for(i in seq_along(calpuff_files)) {
    plot_filename <- unique(calpuff_files[[i]]$plot_filename)
    plot_title <- unique(calpuff_files[[i]]$title)
    plot_subtitle <- unique(calpuff_files[[i]]$subtitle)
    
    if(is.null(plot_filename)) {
      if(unique(plot_subtitle) != '') {
        plot_filename <- paste(plot_title, plot_subtitle)
      } else {
        plot_filename <- plot_title
      }
      plot_filename %<>% gsub('\\.[a-zA-Z0-9]+$', '', .) %>% paste0('.png')
    }
    
    #load the raster to plot
    calpuff_files[[i]]$path %>% stack %>% multiply_by(calpuff_files[[i]]$plotscale) -> r
    plotunit = unique(calpuff_files[[i]]$plotunit) %>% gsub('ug', 'µg', .)
    
    max_val <- r[] %>% max(na.rm=T)
    
    if(!is.null(point_sources_fun)) {
      point_sources <- point_sources_fun(calpuff_files[[i]])
      if(!is.null(point_sources)) point_sources %<>% st_transform(plot_crs)
    }
      
    
    #exclude the insides of area sources from color range
    if(!is.null(area_sources)) {
      max_val <- r[is.na(area_sources_raster)] %>% max
      r[!is.na(area_sources_raster)] %<>% pmin(max_val)
    }
    
    #prepare contour levels and color scales
    levels_to_include <- NULL
    threshold <- unique(calpuff_files[[i]]$threshold.plotunit)
    if(include_threshold_as_break & !is.na(threshold)) levels_to_include <- threshold
    if(!is.null(contour_breaks_fun)) {
      color_scale_basis_raster <- r
      if(!is.null(color_scale_basis_scenario)) {
        color_scale_basis_raster <- calpuff_files %>% bind_rows %>% 
          filter(species==unique(calpuff_files[[i]]$species),
                 period==unique(calpuff_files[[i]]$period),
                 scenario==color_scale_basis_scenario) %>% 
          use_series(path) %>% raster
      }
      
      color_scale_basis_raster %<>% projectRaster(crs='+init=EPSG:4326') %>%  crop(plot_bb)
      contour_breaks <- contour_breaks_fun(color_scale_basis_raster, levels_to_include=levels_to_include,
                                           probs=contour_break_probs)
    }
    
    #prepare color scales
    colRamp <- colorRampPalette(color_scale)(length(contour_breaks))
    alphas <- seq(0,1,length.out=length(contour_breaks)+1)[-1] %>% fill_alpha_function
    fillRamp <- colRamp %>% col.a(alphas)
    
    #prepare dataframe with raster data for plotting
    r %<>% projectRaster(crs = '+init=EPSG:3857') %>% crop(plot_bb_3857*1.2)
    
    r %>% as('SpatialGridDataFrame') %>% as_tibble %>% pivot_longer(matches('^layer|^rank')) -> concs_df
    names(concs_df)[1:2] <- c('lon', 'lat')
    if(facet_by != '') concs_df %<>% full_join(tibble(name=names(r), faceting_name=calpuff_files[[i]][[facet_by]]))
    
    #initialize source data.frames
    area_sources_df <- tibble()
    point_sources_df <- tibble()
    
    #make the plot
    message(unique(calpuff_files[[i]]$title), unique(calpuff_files[[i]]$subtitle))
    
    map_plot <- ggplot() + basemap
    
    if(facet_by != '') map_plot = map_plot + facet_wrap(~faceting_name, ncol=facet_ncol)
    
    if(!is.null(area_sources)) {
      if(facet_by %in% names(area_sources)) area_sources$faceting_name <- area_sources[[facet_by]]
      area_sources_df <- area_sources %>% st_centroid() %>% select(-any_of(c('lon', 'lat')))
      
      area_sources_df %<>% st_coordinates() %>% as_tibble() %>% set_names(c('lon', 'lat')) %>% 
        bind_cols(area_sources_df, .) %>% st_drop_geometry()
      
      map_plot = map_plot + annotation_spatial(area_sources, fill=source_label_color)
    }
    
    if(contour_type %in% c('filled', 'both')) map_plot = map_plot + 
      geom_contour_filled(data=concs_df, aes(lon, lat, z=value), breaks=c(contour_breaks, max_val))
    
    if(contour_type %in% c('lines', 'both') & label_contours) map_plot = map_plot + 
      metR::geom_contour2(data=concs_df, aes(lon, lat, z=value, col=as.factor(..level..), label=..level..), 
                          breaks=contour_breaks, show.legend = T, label_color='white',
                          skip=skip_labels, label.placer = label.placer)
    
    if(contour_type %in% c('lines', 'both') & !label_contours) map_plot = map_plot + 
      geom_contour(data=concs_df, aes(lon, lat, z=value, col=as.factor(..level..)), breaks=contour_breaks)
    
    if(!is.null(point_sources)) {
      if(facet_by %in% names(point_sources)) point_sources$faceting_name <- point_sources[[facet_by]]
      
      point_source_coords <- point_sources %>% st_coordinates()  %>% as_tibble %>% '['(T,1:2) %>% set_names(c('lon', 'lat'))
      point_sources_df <- point_sources %>% select(-any_of(c('lon', 'lat'))) %>% 
        bind_cols(point_source_coords) %>% 
        st_drop_geometry()
      map_plot = map_plot + annotation_spatial(point_sources, mapping=aes(shape='modeled sources'), 
                                               col=source_label_color, stroke=source_marker_linewidth, size=source_marker_size, alpha=source_marker_alpha) +
        scale_shape_manual(values=2, name='', guide=guide_legend(override.aes = list(linetype = 0)))
    }
    
    sources_to_label <- bind_rows(point_sources_df, area_sources_df)
    
    if(nrow(sources_to_label)>0 & label_sources) {
      map_plot = map_plot + 
        geom_text_repel(data=sources_to_label, mapping=aes(label=source), max.overlaps = 20, color=source_label_color, face='bold')
    }
    
    if(contour_type=='filled') {
      contour_guide_fill=contour_guide
      contour_guide_color='none'
    } else {
      contour_guide_fill='none'
      contour_guide_color=contour_guide
    }
    
    map_plot +
      coord_sf(xlim=c(plot_bb_3857@xmin, plot_bb_3857@xmax),
               ylim=c(plot_bb_3857@ymin, plot_bb_3857@ymax)) +
      theme(panel.border = element_rect(fill=NA, color='black')) +
      scale_color_manual(values=colRamp, drop=F, name=plotunit, guide=contour_guide_color,
                         label=contour_breaks) +
      scale_fill_manual(values=fillRamp, drop=F, name=plotunit, guide=contour_guide_fill,
                        label=contour_breaks) +
      labs(title=str_wrap(plot_title, title_width_characters), 
           subtitle=unique(plot_subtitle),
           x='', y='') +
      theme_crea() + ggplot_theme ->
      map_plot
    
    quicksave_options %>% c(list(file=file.path(output_dir, paste0(plot_filename,'.png')), 
                                 plot=map_plot)) ->
      quicksave_args
    do.call(rcrea::quicksave, quicksave_args)
    
    plot_filenames[i] <- plot_filename
  }
  return(plot_filenames)
}


get_basemap <- function(plot_bb, maptype = 'hybrid', source='google', ...) {
  plot_bb %>% as.matrix() %>% as.vector() %>% get_map(maptype = maptype, source=source, ...) %>% ggmap_bbox -> basemap
}


make_contour_breaks <- function(r, levels_to_include=NULL, probs=c(0, .5,.9,.985)) {
  sections <- r[] %>% quantile(p=probs)
  
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
