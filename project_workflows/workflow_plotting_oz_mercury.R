#set up the environment
remotes::install_github("energyandcleanair/creapuff", ref="main", dependencies=T, update=T)
library(creapuff)
require(tidyverse)
require(magrittr)

output_dir <- "F:/TAPM/Oz" # Where to write all generated files
creahia::set_env('gis_dir',"~/GIS/")
Sys.setenv(gis_dir="~/GIS/")

setwd(get_gis_dir())
system("gsutil rsync -r gs://crea-data/gis .")

#list generated tif files
calpuff_files <- get_calpuff_files(ext=".csv", gasunit = 'ug', dir=output_dir)
grids <- get_grids_calpuff(calpuff_files, utm_zone = 55, utm_hem = 'S', map_res = 1)
calpuff_files %>% make_tifs(grids)

calpuff_files <- get_calpuff_files(ext=".tif", gasunit = 'ug', dir=output_dir) %>% 
  filter(species %in% c('hg', 'rgm', 'hg0'))

calpuff_files$path[grepl('trara\\.', calpuff_files$name)] %>% raster %>% 
  focal(matrix(1,3,3), mean) %>% aggregate(3, mean) -> csiro_vs

csiro_vs %>% values %>% sort(decreasing = T) %>% head(10) %>% multiply_by(1000*1e-4*720/8760)

UoM_loc = data.frame(lat="38o18'44”S", lon="146o25'42”E") %>% 
  mutate_all(lauR::dms_to_dec) %>% to_spdf() %>% spTransform(crs(grids$gridR))

calpuff_files$path %>% stack %>% 
  raster::extract(UoM_loc) #%>% multiply_by(1000*1e-4*720/8760)

read.table('clipboard', header=T) -> pp
pp %<>% mutate_all(lauR::dms_to_dec) %>% mutate_at('Latitude', multiply_by, -1) %>% 
  to_spdf %>% spTransform(crs(grids$gridR))
dist = distanceFromPoints(grids$gridR, pp)

calpuff_files$path[grepl('hg_.*trara\\.', calpuff_files$name)] %>% raster %>% 
  (function(r) r[dist>9 & dist<11]) %>% mean %>% multiply_by(1000*1e-4)

dist_3km = distanceFromPoints(csiro_vs, pp)
csiro_vs %>% (function(r) r[dist_3km>5 & dist_3km<10]) %>% max %>% multiply_by(1000*1e-4*720/8760)
#mg/ha/yr -> ug/m2/month

