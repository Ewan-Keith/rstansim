context("refit methods should function correctly")

test_that("refit.stansim_simulation fails correctly", {

  ## read in test stansim_simulation obj to refit
  stansim_obj <-
    readRDS("objects/test_stansim.rds")

  # datasets must be character
  expect_error(refit(stansim_obj, datasets = 5),
               "datasets argument must be of type character")

  ## all datasets must exist in the file structure
  expect_error(refit(stansim_obj,
                     datasets =
                       "data-raw/data/schoolsdat5.rds"),
               paste0("dataset \"",
                      "data-raw/data/schoolsdat5.rds",
                      "\" could not be found. Check your file structure"
               ))

  ## all_datasets must be found in the original stansim_simulation
  # remove schoolsdat4 data records for test
  df_not_found <- stansim_obj
  df_not_found$data <- stansim_obj$data[
    stansim_obj$data[, "dataset"] !=
      "data-raw/data/schoolsdat4.rds", ]

  expect_error(
    refit(df_not_found,
          datasets = "data-raw/data/schoolsdat4.rds"),
    paste0(
      "datasets argument \"",
      "data-raw/data/schoolsdat4.rds",
      "\" not found in provided stansim_simulation object data"
    )
  )
})



test_that("refit.stansim_simulation updates stansim_simulation obj correctly", {

  ## read in test stansim_simulation obj to refit
  stansim_obj <-
    readRDS("objects/test_stansim.rds")


  with_mock(
    `rstansim::fit_models` = function(...){
      readRDS("objects/test_stansim_refit.rds")
    },

  # refit
  new_stansim <-
    refit(stansim_obj,
          datasets = c("data-raw/data/schoolsdat1.rds",
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
  old_subset <- stansim_obj$data[stansim_obj$data$dataset %in%
                                   c("data-raw/data/schoolsdat2.rds",
                                     "data-raw/data/schoolsdat4.rds"), ],

  old_subset <- old_subset[with(old_subset, order(dataset, parameter, estimate, value)), ],

  new_subset <- new_stansim$data[new_stansim$data$dataset %in%
                                  c("data-raw/data/schoolsdat2.rds",
                                    "data-raw/data/schoolsdat4.rds"), ],
  new_subset <- new_subset[with(new_subset, order(dataset, parameter, estimate, value)), ],

  expect_equal(sum(old_subset != new_subset), 0),

  # refitted slot should be correct
  expect_equal(new_stansim$refitted,
               c("data-raw/data/schoolsdat1.rds",
                 "data-raw/data/schoolsdat3.rds"))


  )

})



