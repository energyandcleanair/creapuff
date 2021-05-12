library(testthat)

test_data_dir <- function(){
  if(dir.exists("../test_data")) "../test_data" else "tests/test_data"
}


test_that("get_grids_calpuff works", {
  
  expect_true(dir.exists(test_data_dir()))
  
  for(example_dir in list.dirs(test_data_dir(), recursive = F)){
    print(example_dir)
    
    # Get list of files
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



test_that("make_tifs works", {
  
  expect_true(dir.exists(test_data_dir()))
  
  for(example_dir in list.dirs(test_data_dir(), recursive = F)){
    print(example_dir)
    
    # Remove preexisting tif files
    tif_files <- list.files(example_dir, ".tif", full.names = T)
    file.remove(tif_files)
    tif_files <- list.files(example_dir, ".tif", full.names = T)
    expect_equal(length(tif_files), 0) # Just to confirm...
    
    
    # Get list of files
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
    
    # Make tifs
    creapuff::make_tifs(calpuff_files=calpuff_files, grids=grids)
    
    # Check that they have been created
    tif_files <- list.files(example_dir, ".tif", full.names = T)
    expect_equal(nrow(calpuff_files), length(tif_files))
  }
})



test_that("plotting works", {
  
  expect_true(dir.exists(test_data_dir()))
  
  for(example_dir in list.dirs(test_data_dir(), recursive = F)){
    print(example_dir)
    
    # Get list of files
    calpuff_files <- creapuff::get_calpuff_files(dir=example_dir)
    
    # Get UTM information from plants
    plants <- read.csv(file.path(example_dir, "plants.csv"))
    utm_zone <- get_utm_zone(plants)
    utm_hem <- get_utm_hem(plants)
    
    # Get grids
    grids <- creapuff::get_grids_calpuff(calpuff_files=calpuff_files,
                                         utm_zone=utm_zone,
                                         utm_hem=utm_hem,
                                         map_res=10)
    
    # Make tifs
    creapuff::make_tifs(calpuff_files=calpuff_files, grids=grids)
    
    # Plot - PNG
    png_files <- list.files(example_dir, ".png", full.names = T)
    file.remove(png_files)
    creapuff::plot_results(dir=example_dir, plants=plants, outputs="png")
    png_files <- list.files(example_dir, ".png", full.names = T)
    expect_gt(length(png_files), 0)
    
    # Plot - KMZ
    kmz_files <- list.files(example_dir, ".kmz", full.names = T)
    file.remove(kmz_files)
    creapuff::plot_results(dir=example_dir, plants=plants, outputs="kml")
    kmz_files <- list.files(example_dir, ".kmz", full.names = T)
    expect_gt(length(kmz_files), 0)
    
    # Plot - exPop
    png_files <- list.files(example_dir, ".png", full.names = T)
    file.remove(png_files)
    creapuff::plot_results(dir=example_dir, plants=plants, outputs="expPop")
    png_files <- list.files(example_dir, ".png", full.names = T)
    expect_gt(length(png_files), 0)
    
  }
})


