context("refit methods should function correctly")

test_that("refit.stansim_simulation fails correctly", {

  ## read in test stansim_simulation obj to refit
  stansim_obj <-
    readRDS("objects/test_stansim.rds")

  # datafiles must be character
  expect_error(refit(stansim_obj, datafiles = 5),
               "datafiles argument must be of type character")

  ## all datafile must exist in the file structure
  expect_error(refit(stansim_obj,
                     datafiles =
                       "data-raw/data/schoolsdat5.rds"),
               paste0("datafile \"",
                      "data-raw/data/schoolsdat5.rds",
                      "\" could not be found. Check your file structure"
               ))

  ## all_datafiles must be found in the original stansim_simulation
  # remove schoolsdat4 data records for test
  df_not_found <- stansim_obj
  df_not_found$data <- stansim_obj$data[
    stansim_obj$data[, "datafile"] !=
      "data-raw/data/schoolsdat4.rds", ]

  expect_error(
    refit(df_not_found,
          datafiles = "data-raw/data/schoolsdat4.rds"),
    paste0(
      "datafiles argument \"",
      "data-raw/data/schoolsdat4.rds",
      "\" not found in provided stansim_object data"
    )
  )
})



test_that("refit.stansim_simulation updates stansim_simulation obj correctly", {

  ## read in test stansim_simulation obj to refit
  stansim_obj <-
    readRDS("objects/test_stansim.rds")


  with_mock(
    `rstansim::stansim` = function(...){
      readRDS("objects/test_stansim_refit.rds")
    },

  # refit
  new_stansim <-
    refit(stansim_obj,
          datafiles = c("data-raw/data/schoolsdat1.rds",
                        "data-raw/data/schoolsdat3.rds")),

  ## general object tests

  # output should be a list
  expect_type(new_stansim, "list"),

  # list of length 10
  expect_equal(length(new_stansim), 10),

  # has class "stansim_simulation"
  expect_s3_class(new_stansim, "stansim_simulation"),

  # item names should be as expected
  expect_equal(names(new_stansim),
               c("sim_name", "start_time", "end_time", "model_name",
                 "model_code", "sim_seed", "instances", "data", "raw_call",
                 "refitted")),

  ## specific refit tests
  # the refitted instances should have the same run time (due to mocking)
  # character conversion avoids < 1 sec differences causing failures
  expect_equal(as.character(new_stansim$instances[[2]]$ran_at),
               as.character(new_stansim$instances[[4]]$ran_at)),

  # the non-refitted instances should have the same run time (due to mocking)
  expect_equal(as.character(new_stansim$instances[[1]]$ran_at),
               as.character(new_stansim$instances[[3]]$ran_at)),

  # refitted and original instances should have different run times
  expect_false(as.character(new_stansim$instances[[2]]$ran_at) ==
                 as.character(new_stansim$instances[[1]]$ran_at)),

  # full returned data should be different from original
  expect_false(identical(stansim_obj$data, new_stansim$data)),

  # non-refited subset data should be same as original
  old_subset <- stansim_obj$data[stansim_obj$data$datafile %in%
                                   c("data-raw/data/schoolsdat2.rds",
                                     "data-raw/data/schoolsdat4.rds"), ],

  old_subset <- old_subset[with(old_subset, order(datafile, parameter, estimate, value)), ],

  new_subset <- new_stansim$data[new_stansim$data$datafile %in%
                                  c("data-raw/data/schoolsdat2.rds",
                                    "data-raw/data/schoolsdat4.rds"), ],
  new_subset <- new_subset[with(new_subset, order(datafile, parameter, estimate, value)), ],

  expect_equal(sum(old_subset != new_subset), 0),

  # refitted slot should be correct
  expect_equal(new_stansim$refitted,
               c("data-raw/data/schoolsdat1.rds",
                 "data-raw/data/schoolsdat3.rds"))


  )

})



