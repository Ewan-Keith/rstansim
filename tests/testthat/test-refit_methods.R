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

  ## all datafile must exist in the file structure
  expect_error(refit(stansim_obj,
                     datafiles =
                       "data-raw/data/schoolsdat5.rds"),
               paste0("datafile \"",
                      "data-raw/data/schoolsdat5.rds",
                      "\" could not be found. Check your file structure"
               ))

  ## all_datafiles must be found in the original stansim_single
  # remove schoolsdat4 data records for test
  df_not_found <- stansim_obj
  df_not_found$data <- stansim_obj$data[
    stansim_obj$data[, "data"] !=
      "data-raw/data/schoolsdat4.rds", ]

  expect_error(
    refit(df_not_found,
          datafiles = "data-raw/data/schoolsdat4.rds"),
    paste0(
      "datafiles argument \"",
      "data-raw/data/schoolsdat4.rds",
      "\" not found in provided stansim_object"
    )
  )
})

test_that("refit.stansim_single updates stansim_single obj correctly", {

  ## read in test stansim_single obj to refit
  stansim_obj <-
    readRDS("objects/test_stansim.rds")

  # refit
  new_stansim <-
    refit(stansim_obj,
          datafiles = c("data-raw/data/schoolsdat1.rds",
                        "data-raw/data/schoolsdat2.rds"))




})
