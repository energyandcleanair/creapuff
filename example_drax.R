library(creapuff)
require(tidyverse)
require(magrittr)
require(pbapply)

gis_dir = "C:\\Users\\lauri\\Documents\\GIS"

load('~/CALPUFF/Drax.RData')

################################
#CALPUFF
################################
outFiles %<>% rename(run_name=runName,
                     grid_name=gridName,
                     METDAT=outFilePath)

outFiles %>% 
  rename(IUTMZN=UTMZ,
         UTMHEM=UTMH,
         DGRIDKM=GridD,
         NX=GridNX,
         NY=GridNY,
         XORIGKM=GridX,
         YORIGKM=GridY) %>% 
  mutate(IBYR=StartDate %>% substr(1,4),
         IBMO=StartDate %>% substr(5,6),
         IBDY=StartDate %>% substr(7,8),
         IEYR=EndDate %>% substr(1,4),
         IEMO=EndDate %>% substr(5,6),
         IEDY=EndDate %>% substr(7,8),
         ABTZ=paste0('UTC',ifelse(TZ>=0, '+', '-'), stringr::str_pad(TZ, 2, 'left', '0'), '00')) ->
  params_allgrids

domain_centers = params_allgrids %>% distinct(run_name, Lat, Lon)

bgconcs = get_bg_concs(domain_centers)

sources = read_csv('F:/TAPM/Drax/stations.txt')
target_crs = get_utm_proj(30, 'N', units = 'km')
sources %<>% rename(x=easting, y=northing) %>% to_spdf(crs=get_utm_proj(30, 'N', units = 'm')) %>% 
  spTransform(target_crs)

#get receptors for all
nesting_factors = c(3, 6, 10, 30)
nesfact_range = c(300, 60, 30, 10)

receptors = get_recep(sources,
                      run_name = 'Drax',
                      nesting_factors=nesting_factors,
                      files_met=outFiles,
                      target_crs=target_crs)

receptors %<>% select_receptors(sources=sources, 
                                run_name = 'Drax',
                                nesting_factors=nesting_factors, 
                                nesfact_range=nesfact_range, 
                                files_met=outFiles)

make_calpuff_inp(files_met=outFiles,
                 calpuff_template = system.file("extdata", "CALPUFF_7.0_template_Hg.INP", package="creapuff"),
                 puffrun='Drax',
                 bgconcs = bgconcs,
                 receptors=receptors %>% subset(include) %>% make_topo_rows,
                 addparams = list(NPTDAT = 1,
                                  PTDAT = 'F:/TAPM/Drax/ptemarb_2019_drax.DAT',
                                  NPT2 = 1)) -> inpfiles_created

read_csv('~/CALPUFF/202105_drax/data/emissions_year.csv') -> emis

emis %>% filter(grepl("HG|PM10", pollutantCode)) %>% group_by(reportingYear) %>% 
  rename(emissions_kg = totalPollutantQuantityKg) %>% 
  group_modify(function(df, year) {
    pm10fraction = df$emissions_kg[df$pollutantCode=='HGANDCOMPOUNDS']*1.8e-2 / 
      (df$emissions_kg[df$pollutantCode=='PM10']*30/54)
    tibble(year=unique(year$reportingYear), value=pm10fraction)
  }) -> pm10fraction

runPostprocessing(calpuff_inp = inpfiles_created,
                  run_name = 'Drax',
                  pm10fraction = pm10fraction$value[pm10fraction$year==2019],
                  run_pu=F, run_calpost=F)

