require(raster)
require(tidyverse)
require(magrittr)

scens %>% group_by(across(everything())) %>% 
  group_walk(function(df, group) {
    message(group)
    outfile <- file.path(output_dir, paste0('rank(0)_', group$species, '_',group$hr,'hr_conc_', group$scenario, 'y', group$year, '.tif'))
    if(!file.exists(outfile) | overwrite) {
      emis %>% filter(scenario==group$emission_scenario, year==group$year) %>% arrange(cluster) %>% get_scaled_raster(group$species) %>% 
        writeRaster(outfile, overwrite=overwrite)
    }
  })