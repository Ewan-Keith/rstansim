context("refit methods should function correctly")

test_that("refit.stansim_single fails correctly", {

  ## read in test stansim_single obj to refit
  stansim_obj <-
    readRDS("objects/test_stansim.rds")

  # datafiles must be character
  expect_error(refit(stansim_obj, datafiles = 5),
               "datafiles argument must be of type character")

  # all_warn must be logical
  expect_error(refit(stansim_obj, all_warn = 5),
               "all_warn argument must be of type logical")

  # all_warn must be logical
  expect_error(
    refit(stansim_obj,
          datafiles = "tests/testthat/data-raw/data/schoolsdat5.rds"),
    paste0(
      "datafiles argument \"",
      "tests/testthat/data-raw/data/schoolsdat5.rds",
      "\" not found in provided stansim_object"
    )
  )


})
