readLines('~/google_api_key.txt') %>% ggmap::register_google()

plot_bb = point_sources_to_plot %>% st_transform(crs=4326) %>% extent() %>% add(6)
basemap <- get_basemap(plot_bb, zoom=6, color='bw', maptype='roadmap')

plot_bb %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(4326) %>% st_transform(3857) -> plot_bb_polygon
ggmap(basemap) + layer_spatial(plot_bb_polygon, fill=NA, linewidth=2, color='red')




calpuff_files_all %>% filter(species=='pm25') %>% head(1) %>% 
  filter(year==2022) %>% 
  mutate(subtitle='in 2022') %>% 
  plot_contours(plot_bb=plot_bb,
                contour_type='filled',
                #color_scale_basis_scenario='noimpr_31',
                point_sources=point_sources_to_plot,
                basemap=basemap,
                label_sources=F,
                color_scale=paletteer::paletteer_c("grDevices::Lajolla", 7),
                source_label_color='brown1',
                fill_alpha_function = (function(x) x),
                output_dir='~')
