# Set up the environment
# remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
# devtools::reload(pkgload::inst("creapuff"))
require(raster)
require(sf)
require(tidyverse)
require(magrittr)
require(lubridate)
library(readxl)
library(creapuff) 
list.files(path='R', full.names=T) %>% sapply(source)


# Parameters ###################################################################
# ============================= Project specific ===============================
#project_dir="I:/SouthAfrica"       # calpuff_external_data-2 persistent disk (project data)
project_dir="C:/Users/lauri/Desktop/My Drive/air pollution/TAPM/2017cases/SouthAfrica2022"
input_dir <- file.path(project_dir,"calpuff_suite") # Where to read all CALPUFF generated files
output_dir <- file.path(project_dir,"plots") ; if (!dir.exists(output_dir)) dir.create(output_dir) # Where to write all HIA files
emissions_dir <- file.path(project_dir,"emissions")

#load grid parameters
calmet_result <- readRDS(file.path(input_dir,"calmet_result.RDS" ))
UTMZ <- calmet_result$params[[01]]$IUTMZN
UTMH <- calmet_result$params[[01]]$UTMHEM

# List generated csv files
input_dir <- file.path(project_dir,"calpuff_suite/Lephalale") # Where to read all CALPUFF generated files
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ppb', dir=input_dir, hg_scaling=1e-3)
grids = get_grids_calpuff(calpuff_files, UTMZ, UTMH, map_res=1)

#make tifs
calpuff_files %>% make_tifs(grids = grids)

# Select tif data 
calpuff_files_all <- get_calpuff_files(ext=".tif", gasunit = 'ppb', dir=input_dir, hg_scaling=1e-3) %>% 
  mutate(scenario_description = paste0(case_when(grepl('mnpp', scenario)~'Lephalale mine and IPP power plant',
                                                 #grepl('pp', scenario)~'Lephalale IPP power plant',
                                                 grepl('mine|mn', scenario)~'Lephalale mine'),
                                       ifelse(grepl('.bg', scenario), ' combined with major background sources', ''))) %>% 
  filter(grepl('pp', scenario) | grepl('pm|tsp|hg', species), scenario != 'lcppipp')

calpuff_files_all %>% write_csv(file.path(input_dir, 'file_info.csv'))



# ================================ General =====================================
gis_dir <- "~/GIS"                         # The folder where we store general GIS data

# creahia::set_env('gis_dir',"~/GIS/")
# Sys.setenv(gis_dir="~/GIS/")
#
# setwd(get_gis_dir())
# system("gsutil rsync -r gs://crea-data/gis .")


# Plots ########################################################################
target_crs <- crs(grids$gridR)

source('project_workflows/emissions_processing_SA.R')

plot_crs=3857
point_sources %<>% st_transform(plot_crs) %>% filter(grepl('IPP|Medupi|Matimba', plant))
point_sources_df <- point_sources %>% bind_cols(st_coordinates(.)) %>% 
  select(-lon, -lat) %>% st_drop_geometry() %>% rename(lon=X, lat=Y, source=plant)

lmine <- file.path(emissions_dir, 'Lephalale infrastructure area.kml') %>% rgdal::readOGR() %>% subset(Name=='Development Area') %>% 
  st_as_sf() %>% st_transform(plot_crs)

gtmine <- file.path(emissions_dir, 'Grootgeluk.kml') %>% rgdal::readOGR() %>% 
  st_as_sf() %>% st_transform(plot_crs)

area_sources <- bind_rows(lmine %>% mutate(source='Lephalale mine'),
                          gtmine %>% mutate(source='Grootegeluk mine'))

area_sources_df <- area_sources %>% st_centroid() %>% 
  bind_cols(st_coordinates(.)) %>% 
  select(-any_of(c('lon', 'lat'))) %>% st_drop_geometry() %>% rename(lon=X, lat=Y)

area_sources_raster <- area_sources %>% st_transform(target_crs) %>% raster::rasterize(grids$gridR)

require(ggspatial); require(ggmap); require(ggrepel)


plot_bb = point_sources %>% st_transform(crs=4326) %>% extent() %>% add(c(-.1,.3,-.1,.35))
plot_bb_3857 = plot_bb %>% raster(crs='+init=EPSG:4326') %>% projectExtent(crs='+init=EPSG:3857') %>% 
  extent

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


basemap <- plot_bb %>% as.matrix() %>% as.vector() %>% get_map(maptype = 'hybrid', source='google', zoom=10) %>% ggmap_bbox

ggmap(basemap) + 
  annotation_spatial(st_transform(point_sources, crs=3857), col='red') +
  annotation_spatial(st_transform(area_sources, crs=3857), col='red')

queue <- which(calpuff_files_all$scenario %notin% c('bg', 'bgmx') &
                 calpuff_files_all$species %in% c('so2', 'no2') &
                 !is.na(calpuff_files_all$threshold))

plot_contours <- function(calpuff_files,
                          titletxt = calpuff_file %>% make_titletxt %>% gsub('\n', ' ', .),
                          plot_bb=NULL,
                          basemap,
                          contour_type='lines',
                          point_sources=NULL,
                          area_sources=NULL,
                          color_scale=c(crea_palettes$change[5:7], 'black'),
                          contour_breaks_function=make_contour_breaks,
                          include_threshold_as_break=T,
                          output_dir='',
                          filename = file.path(output_dir, paste0(titletxt,'.png'))) {
  for(i in seq_along(calpuff_files$path)) {
    calpuff_files$path[i] %>% raster -> r
    
    max_val <- r[is.na(area_sources_raster)] %>% max
    r[!is.na(area_sources_raster)] %<>% pmin(max_val)
    
    r %<>% projectRaster(crs = '+init=EPSG:3857') %>% crop(plot_bb_3857*1.2)
    
    r %>% as('SpatialGridDataFrame') %>% as_tibble -> concs_df
    names(concs_df) <- c('value', 'lon', 'lat')
    
    levels_to_include <- NULL
    if(include_threshold_as_break) levels_to_include <- calpuff_files$threshold[i]
    brks <- contour_breaks_function(r, levels_to_include=levels_to_include)
    colRamp <- colorRampPalette(color_scale)(length(brks))
    
    map_plot <- ggmap(basemap)
    
    if(!is.null(area_sources)) map_plot = map_plot + annotation_spatial(area_sources, fill='darkgray')
    if(contour_type=='filled') map_plot = map_plot + 
      metR::geom_contour2(data=concs_df, aes(lon, lat, z=value, col=as.factor(..level..), label=..level..), breaks=brks)
    if(contour_type=='lines') map_plot = map_plot + 
      metR::geom_contour2(data=concs_df, aes(lon, lat, z=value, col=as.factor(..level..), label=..level..), breaks=brks, show.legend = T, label_color='white')
    
      
      annotation_spatial(point_sources, col='red') +
      geom_label_repel(data=bind_rows(point_sources_df, area_sources_df), mapping=aes(label=source, label = source),
                       box.padding = 2) +
      coord_sf(xlim=c(plot_bb_3857@xmin, plot_bb_3857@xmax),
               ylim=c(plot_bb_3857@ymin, plot_bb_3857@ymax)) +
      scale_x_continuous(limits=c(plot_bb_3857@xmin, plot_bb_3857@xmax), expand=expansion()) +
      scale_y_continuous(limits=c(plot_bb_3857@ymin-2e5, plot_bb_3857@ymax), expand=expansion()) +
      theme(panel.border = element_rect(fill=NA, color='black')) +
      labs(title=str_wrap(titletxt, 45), x='', y='') +
      scale_color_manual(values=colRamp, name='µg/m3') +
      scale_size_continuous(range=c(.2,2)) +
      theme_crea() ->
      map_plot
    
    quicksave(filename, plot=map_plot, width = 10, height = 8) 
  }
}



make_contour_breaks <- function(r, levels_to_include=NULL) {
  sections <- r %>% values %>% quantile(p=c(0, .5,.9,.985))
  
  c(pretty(sections[1:2], n=3), 
    pretty(sections[2:3], n=3), 
    pretty(sections[3:4], n=3)) %>% 
    unique %>% subset(.<=max_val & .>sections[1]) %>% c(levels_to_include) %>% sort
}



raster('~/../Downloads/V5GL03.HybridPM25c_0p10.Global.201901-201912.nc') -> pm25
get_adm(3, 'low') %>% subset(NAME_3=='Lephalale') -> lepha_adm
point_sources %>% st_drop_geometry() %>% to_spdf %>% raster::extract(pm25, .)



# ==============================================================================
# ... Moving ...
print('... moving output files (png, exceedances, tseries,...)')
# PNG files
png_dir <- file.path(project_dir,"png")
if (!dir.exists(png_dir)) dir.create(png_dir)
file.path(output_dir,list.files(output_dir,"\\.png$" )) -> png_files
file.copy(png_files,png_dir, overwrite = TRUE)
file.remove(png_files)
# KML files
kmz_dir <- file.path(project_dir,"kmz") ; 
if (!dir.exists(kmz_dir)) dir.create(kmz_dir)
file.path(output_dir,list.files(output_dir,"\\.kmz$" )) -> kmz_files
file.copy(kmz_files,kmz_dir, overwrite = TRUE)
file.remove(kmz_files)
# Exeedances
exceedances_dir <- file.path(project_dir,"exceedances")
if (!dir.exists(exceedances_dir)) dir.create(exceedances_dir)
file.path(output_dir,list.files(output_dir,"expPop|threshold_exceedances" )) -> exceedances_files
file.copy(exceedances_files,exceedances_dir, overwrite = TRUE)
file.remove(exceedances_files)
# tseries
tseries_dir <- file.path(project_dir,"tseries")
if (!dir.exists(tseries_dir)) dir.create(tseries_dir)
file.path(output_dir,list.files(output_dir,"tseries" )) -> tseries_files
file.copy(tseries_files,tseries_dir, overwrite = TRUE)
file.remove(tseries_files)

browser()

# ==============================================================================
#get WDPA protected areas
grids_wdpa <- grids
grids_wdpa$gridR %<>% (function(x) {crop(x, extent(x)*.33)})
get_wdpa_for_grid(grids_wdpa) -> wdpa_areas
saveRDS(file.path(output_dir, 'WDPA areas.RDS'))

#output deposition results
get_deposition_results(calpuff_files, dir=output_dir, wdpa_areas=wdpa_areas) -> depo

