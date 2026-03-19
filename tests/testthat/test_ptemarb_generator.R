library(testthat)


test_that("ptemarb.check_input validates sources and emissions", {

  template_dir <- system.file("templates", "generate_ptemarbs", package = "creapuff")
  sources   <- readr::read_csv(file.path(template_dir, "sources.csv"), show_col_types = FALSE)
  emissions <- readr::read_csv(file.path(template_dir, "emissions.csv"), show_col_types = FALSE)
  species   <- list(SO2 = 64, NO = 30, NO2 = 46, PPM25 = 1)

  # Should pass without error

  expect_silent(creapuff:::ptemarb.check_input(sources, emissions, species))

  # Should fail with missing column
  bad_sources <- sources[, -which(names(sources) == "stack_height_m")]
  expect_error(creapuff:::ptemarb.check_input(bad_sources, emissions, species),
               "Missing colnames in sources")
})


test_that("ptemarb_generator works end-to-end", {

  template_dir <- system.file("templates", "generate_ptemarbs", package = "creapuff")
  sources   <- readr::read_csv(file.path(template_dir, "sources.csv"), show_col_types = FALSE)
  emissions <- readr::read_csv(file.path(template_dir, "emissions.csv"), show_col_types = FALSE)

  output_dir <- file.path(tempdir(), "ptemarb_test")

  creapuff::generate_ptemarbs(
    folder   = output_dir,
    sources  = sources,
    emissions = emissions,
    species  = list(SO2 = 64, NO = 30, NO2 = 46, PPM25 = 1),
    utc_zone = "UTC+0900"
  )

  # Config file should have been created
  expect_true(file.exists(file.path(output_dir, "config.json")))
})

