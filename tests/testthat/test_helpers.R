library(testthat)

test_data_dir <- function(){
  if(dir.exists("../test_data")) "../test_data" else "tests/test_data"
}

#identify dirs with calpost outputs to work with
test_output_dirs <- function(){
  candidates <- list.dirs(test_data_dir(), recursive = F)
  has_plants <- file.exists(file.path(candidates, 'plants.csv'))
  candidates[has_plants]
}


test_that("get_grids_calpuff works", {
  
  expect_true(dir.exists(test_data_dir()))
  
  for(example_dir in test_output_dirs()){
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
  
  for(example_dir in test_output_dirs()){
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

  gis_dir <- "F:/gis"
  readRenviron("../.Renviron")

  expect_true(dir.exists(test_data_dir()))

  for(example_dir in test_output_dirs()){
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
    calpuff_files <- creapuff::get_calpuff_files(ext='\\.tif', dir=example_dir)

    # Plot - PNG
    png_files <- list.files(example_dir, ".png", full.names = T)
    file.remove(png_files)
    creapuff::plot_results(calpuff_files, dir=example_dir, plants=plants, outputs="png")
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


test_that("latlon_to_utm converts coordinates correctly", {

  # Known point: Korea source K10O from sources.csv template
  # lat=35.315199, lon=129.178375 in UTM 52N should give ~516.21 km E, ~3908.01 km N
  result <- latlon_to_utm(lat = 35.315199, lon = 129.178375, utm_zone = "52N", units = "km")

  expect_true(is.data.frame(result))
  expect_equal(names(result), c("easting", "northing"))
  expect_equal(result$easting,  516.214, tolerance = 0.01)
  expect_equal(result$northing, 3908.013, tolerance = 0.01)

  # Omitting units must error
  expect_error(latlon_to_utm(lat = 35.315199, lon = 129.178375, utm_zone = "52N"))
})


test_that("latlon_to_utm handles vectorised input", {

  result <- latlon_to_utm(
    lat = c(35.315199, 37.6543),
    lon = c(129.178375, 127.113),
    utm_zone = "52N",
    units = "km"
  )

  expect_equal(nrow(result), 2)
  # Second point: K11O — easting ~333.54, northing ~4169.13
  expect_equal(result$easting[2],  333.544, tolerance = 0.01)
  expect_equal(result$northing[2], 4169.135, tolerance = 0.01)
})


test_that("latlon_to_utm supports units='m'", {

  result_km <- latlon_to_utm(lat = 35.315199, lon = 129.178375, utm_zone = "52N", units = "km")
  result_m  <- latlon_to_utm(lat = 35.315199, lon = 129.178375, utm_zone = "52N", units = "m")

  expect_equal(result_m$easting,  result_km$easting * 1000, tolerance = 1)
  expect_equal(result_m$northing, result_km$northing * 1000, tolerance = 1)
})


