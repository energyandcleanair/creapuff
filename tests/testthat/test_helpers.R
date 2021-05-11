library(testthat)

test_data_dir <- function(){
  if(dir.exists("../test_data")) "../test_data" else "tests/test_data"
}


test_that("get_grids_calpuff works", {
  
  expect_true(dir.exists(test_data_dir()))
  
  for(example_dir in list.dirs(test_data_dir(), recursive = F)){
    print(example_dir)
    
    # Get list of ciles
    calpuff_files <- creapuff::get_calpuff_files(dir=example_dir)
    expect_gt(nrow(calpuff_files), 0)
  
    # Get UTM information from plants
    plants <- read.csv(file.path(example_dir, "plants.csv"))
    utm_zone <- get_utm_zone(plants)
    utm_hem <- get_utm_hem(plants)
    
    # Get grids
    grids <- creapuff::get_grids_calpuff(calpuff_files=calpuff_files,
                                         utm_zone=utm_zone,
                                         utm_hem=utm_hem,
                                         map_res=10)
    expect_true(all(c("gridR","gridLL","gridSP") %in% names(grids)))
  }
})


