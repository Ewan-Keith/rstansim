context("stan_sim objects and methods should function correctly")

test_that("stan_sim object constructor should work correctly", {



})

test_that("extract_data.stansim function should return expected results", {

  ## read in test stansim obj to extract from
  extract_test_data <- readRDS("objects/test_stansim.rds")

  ## input errors should be as expected
  expect_error(extract_data(extract_test_data, values = 5),
               "value argument must be NULL or a function")

  expect_error(extract_data(extract_test_data, datasets = 5),
               "dataset argument must be of type character")

  expect_error(extract_data(extract_test_data, parameters = 5),
               "parameter argument must be of type character")

  expect_error(extract_data(extract_test_data, estimates = 5),
               "estimate argument must be of type character")

  ## extract should return the known correct dimensions of extracted data
  expect_equal(dim(extract_data(extract_test_data)),
               c(760, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 datasets = "data-raw/data/schoolsdat2.rds")
  ),
  c(190, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 datasets = c("data-raw/data/schoolsdat2.rds",
                             "data-raw/data/schoolsdat3.rds"))
  ),
  c(380, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 parameters = "mu")
  ),
  c(40, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 parameters = c("mu", "eta[1]"))
  ),
  c(80, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 parameters = c("mu", "eta"))
  ),
  c(360, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 parameters = c("eta[1]", "eta"))
  ),
  c(320, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 parameters = "mu",
                 param_expand = FALSE)
  ),
  c(40, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 parameters = "mu",
                 param_expand = FALSE)
  ),
  c(40, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 parameters = c("eta"),
                 param_expand = FALSE)
  ),
  c(0, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 parameters = c("eta[1]"),
                 param_expand = FALSE)
  ),
  c(40, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 parameters = c("eta[1]", "eta"),
                 param_expand = FALSE)
  ),
  c(40, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 estimates = "Rhat")
  ),
  c(76, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 estimates = c("Rhat", "97.5%"))
  ),
  c(152, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 estimates = c("Rhat", "97.5%", "test"))
  ),
  c(152, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 estimates = c("Rhat", "97.5%", "test"))
  ),
  c(152, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 values = function(x) x > 1.1)
  ),
  c(352, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 values = function(x) x < 0)
  ),
  c(157, 4))

  ## extract should return a dataframe
  expect_true(is.data.frame(extract_data(extract_test_data)))



})


