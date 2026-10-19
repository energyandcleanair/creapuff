require(raster)
require(tidyverse)
require(magrittr)
require(lubridate)
require(readxl)
require(creahelpers)
require(rcrea)

#require(creapuff)
#require(ggmap)

read_xlsx("~/../Downloads/Thailand_Gas Power Plants_HIA.xlsx", sheet='Model Input',
          .name_repair=make.names) -> emis

emis