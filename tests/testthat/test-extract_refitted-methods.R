context("testing all extract_refitted methods")

test_that(
  paste("extract_refitted.stansim_simulation function should return",
        "expected results"),
  {

    ## read in test stansim obj to extract from
    extract_test_data <-
      readRDS("objects/refitted_for_collection_tests.rds")

    refitted_extract <- extract_refitted(extract_test_data)

    # is type character
    expect_type(refitted_extract, "character")

    # is length 2
    expect_equal(length(refitted_extract), 2)

    # correct values recorded
    expect_equal(refitted_extract,
                 c("data-raw/data/schoolsdat1.rds",
                   "data-raw/data/schoolsdat3.rds"))

  }
)

test_that(
  paste("extract_refitted.stansim_collection function should return",
        "expected results"),
  {

    ## read in test stansim obj to extract from
    extract_test_data <- readRDS("objects/collection_for_method_tests.rds")

    ## input errors should be as expected
    expect_error(extract_refitted(extract_test_data, datafiles = 5),
                 "datafiles argument must be of type character")

    expect_error(extract_refitted(extract_test_data, sim_names = 5),
                 "sim_names argument must be of type character")

    ## checks with default args
    default_test <- extract_refitted(extract_test_data)
    # should be dataframe
    expect_true(is.data.frame(default_test))

    # dims should be correct
    expect_equal(dim(default_test), c(2, 2))

    # dim names should be correct
    expect_named(default_test, c("sim_name", "datafile"))

    # coltypes should be character
    expect_type(default_test[, 1], "character")
    expect_type(default_test[, 2], "character")

    # sim names should be correct
    expect_equal(default_test$sim_name,
                 c("refitted test sim", "refitted test sim"))

    # datafiles should be correct
    expect_equal(default_test$datafile,
                 c("data-raw/data/schoolsdat1.rds",
                   "data-raw/data/schoolsdat3.rds"))

    ## filtering sim_name
    simname_test <- extract_refitted(extract_test_data,
                                     sim_names = "refitted test sim")
    # should be dataframe
    expect_true(is.data.frame(simname_test))

    # dims should be correct
    expect_equal(dim(simname_test), c(2, 2))

    # dim names should be correct
    expect_named(simname_test, c("sim_name", "datafile"))

    # coltypes should be character
    expect_type(simname_test[, 1], "character")
    expect_type(simname_test[, 2], "character")

    # sim names should be correct
    expect_equal(simname_test$sim_name,
                 c("refitted test sim", "refitted test sim"))

    # datafiles should be correct
    expect_equal(simname_test$datafile,
                 c("data-raw/data/schoolsdat1.rds",
                   "data-raw/data/schoolsdat3.rds"))

    # correct behaviour when sim_name isn't present
    missing_simname_test <- extract_refitted(extract_test_data,
                                     sim_names = "not there")

    # should be dataframe
    expect_true(is.data.frame(missing_simname_test))

    # dims should be correct
    expect_equal(dim(missing_simname_test), c(0, 2))

    # dim names should be correct
    expect_named(missing_simname_test, c("sim_name", "datafile"))

    # coltypes should be character
    expect_type(missing_simname_test[, 1], "character")
    expect_type(missing_simname_test[, 2], "character")


    ## filtering datafiles
    datafiles_test <- extract_refitted(extract_test_data,
                                     datafiles =
                                       "data-raw/data/schoolsdat1.rds")
    # should be dataframe
    expect_true(is.data.frame(datafiles_test))

    # dims should be correct
    expect_equal(dim(datafiles_test), c(1, 2))

    # dim names should be correct
    expect_named(datafiles_test, c("sim_name", "datafile"))

    # coltypes should be character
    expect_type(datafiles_test[, 1], "character")
    expect_type(datafiles_test[, 2], "character")

    # sim names should be correct
    expect_equal(datafiles_test$sim_name,
                 "refitted test sim")

    # datafiles should be correct
    expect_equal(datafiles_test$datafile,
                 "data-raw/data/schoolsdat1.rds")

    # correct behaviour when datafile isn't present
    missing_datafile_test <- extract_refitted(extract_test_data,
                                             datafiles = "not there")

    # should be dataframe
    expect_true(is.data.frame(missing_datafile_test))

    # dims should be correct
    expect_equal(dim(missing_datafile_test), c(0, 2))

    # dim names should be correct
    expect_named(missing_datafile_test, c("sim_name", "datafile"))

    # coltypes should be character
    expect_type(missing_datafile_test[, 1], "character")
    expect_type(missing_datafile_test[, 2], "character")

  })

test_that("extract_refitted.stansim_collection fails correctly", {

  coll1 <-
    readRDS("objects/collection_for_method_tests.rds")

  expect_error(extract_refitted(coll1, datafiles = c("all", "another")),
               paste("if datafiles argument contains \"any\",",
                     "length\\(datafiles\\) must be 1"))

  expect_error(extract_refitted(coll1, sim_names = c("all", "another")),
               paste("if sim_names argument contains \"any\",",
                     "length\\(sim_names\\) must be 1"))


})
