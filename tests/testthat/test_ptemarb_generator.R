library(testthat)


test_data_dir <- function(){
  if(dir.exists("../test_data")) "../test_data" else "tests/test_data"
}

test_that("ptemarb_generator works", {
  
  expect_true(dir.exists(test_data_dir()))

  # library(tidyverse)  
  sources <- readr::read_csv(file.path(test_data_dir(), "korea/source_characterics.csv"))
  emissions <- readr::read_csv(file.path(test_data_dir(), "korea/emissions_by_cluster.csv"))

  
  creapuff::generate_ptemarbs(folder="tmp",
                              sources=sources,
                              emissions=emissions,
                              species=list(SO2=32, NO=30, NO2=46, PPM25=1),
                              utc_zone="UTC+0100")
  
  
  
})

