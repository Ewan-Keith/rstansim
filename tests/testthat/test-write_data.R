context("write_data functions as expected")

data_test <- readRDS("objects/stansim_data_for_method_tests.rds")

#-----------------------------------------------------------------
#### input verification ####

test_that("write_data fails as expected with bad input", {

  # object must be class stansim_data
  expect_error(write_data(object = "test"),
               "object must be of class stansim_data")

  # path must be character
  expect_error(write_data(data_test,
                          path = 5),
               "path must be of type character")

  # dataname must be character or NULL
  expect_error(write_data(data_test,
                          path = "testdir",
                          data_name = 5),
               "data_name must be NULL or of type character")

  # recursive must be logical
  expect_error(write_data(data_test,
                          path = "testdir",
                          recursive = "test"),
               "recursive must be of type logical")

  expect_error(write_data(data_test,
                          path = "testdir",
                          recursive = 55),
               "recursive must be of type logical")

})

#-----------------------------------------------------------------
#### test with non-existent directory ####

test_that("write_data creates directory and functions correctly", {
  # check that directory doesn't already exist
  expect_false(dir.exists("testdir"))

  # run write_data
  write_data(data_test, path = "testdir", data_name = "name_stem")

  # test that directory has been created
  expect_true(dir.exists("testdir"))

  # test that 100 files are written
  expect_equal(length(list.files("testdir")), 100)

  for (i in list.files("testdir")) {
    # test that all files have the correct stem
    expect_match(i, "^name_stem_\\d+.RDS$")

    # read in one of the files
    rds_test <- readRDS(paste0("testdir/", i))

    # check has length 3
    expect_length(rds_test, 3)

    # test names are correct
    expect_named(rds_test, c("x", "y", "N"))

    # test that N is correct
    expect_equal(rds_test$N, 100)
  }

  # delete the directory
  unlink("testdir", recursive = TRUE)

  # check that directory doesn't exist
  expect_false(dir.exists("testdir"))
})

#-----------------------------------------------------------------
#### test with existent directory ####

test_that("write_data functions correctly with pre-existing directory", {
  # check that directory doesn't already exist
  expect_false(dir.exists("testdir"))

  # create dir
  dir.create("testdir")

  # test that directory has been created
  expect_true(dir.exists("testdir"))

  # run write_data
  write_data(data_test, path = "testdir", data_name = "name_stem")

  # test that directory has been created
  expect_true(dir.exists("testdir"))

  # test that 100 files are written
  expect_equal(length(list.files("testdir")), 100)

  for (i in list.files("testdir")) {
    # test that all files have the correct stem
    expect_match(i, "^name_stem_\\d+.RDS$")

    # read in one of the files
    rds_test <- readRDS(paste0("testdir/", i))

    # check has length 3
    expect_length(rds_test, 3)

    # test names are correct
    expect_named(rds_test, c("x", "y", "N"))

    # test that N is correct
    expect_equal(rds_test$N, 100)
  }

  # delete the directory
  unlink("testdir", recursive = TRUE)

  # check that directory doesn't exist
  expect_false(dir.exists("testdir"))
})


