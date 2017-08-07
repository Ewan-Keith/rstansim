context("testing all extract_data methods")

test_that(paste("extract_data.stansim_simulation function should return",
                "expected results"), {

                  ## read in test stansim obj to extract from
                  extract_test_data <- readRDS("objects/test_stansim.rds")

                  ## input errors should be as expected
                  expect_error(extract_data(extract_test_data, values = 5),
                               "value argument must be NULL or a function")

                  expect_error(extract_data(extract_test_data, datasets = 5),
                               "datasets argument must be of type character")

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
                  c(351, 4))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 values = function(x) x < 0)
                  ),
                  c(160, 4))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 estimates = "Rhat",
                                 values = function(x) x > 1.1)
                  ),
                  c(0, 4))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 estimates = "Rhat",
                                 values = function(x) x < 1.1 & x > 1)
                  ),
                  c(45, 4))

                  ## if "all" is provided for an arg it must be alone
                  expect_error(extract_data(
                    extract_test_data,
                    datasets = c("all",
                                 "data-raw/data/schoolsdat2.rds")
                  ),
                  "if datasets argument contains \"any\", length\\(datasets\\) must be 1")

                  expect_error(extract_data(
                    extract_test_data,
                    parameters = c("all",
                                   "eta")
                  ),
                  "if parameters argument contains \"any\", length\\(parameters\\) must be 1")

                  expect_error(extract_data(
                    extract_test_data,
                    estimates = c("all",
                                  "Rhat")
                  ),
                  "if estimates argument contains \"any\", length\\(estimates\\) must be 1")

                  ## extract should return a dataframe
                  expect_true(is.data.frame(extract_data(extract_test_data)))

                })

test_that(paste("extract_data.stansim_collection function should return",
                "expected results"), {

                  ## read in test stansim obj to extract from
                  extract_test_data <-
                    readRDS("objects/collection_for_method_tests.rds")

                  ## input errors should be as expected
                  expect_error(extract_data(extract_test_data, values = 5),
                               "value argument must be NULL or a function")

                  expect_error(extract_data(extract_test_data, sim_names = 5),
                               "sim_names argument must be of type character")

                  expect_error(extract_data(extract_test_data, datasets = 5),
                               "datasets argument must be of type character")

                  expect_error(extract_data(extract_test_data, parameters = 5),
                               "parameter argument must be of type character")

                  expect_error(extract_data(extract_test_data, estimates = 5),
                               "estimate argument must be of type character")

                  ## extract should return the known correct dimensions of extracted data
                  expect_equal(dim(extract_data(extract_test_data)),
                               c(1520, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 sim_names = "refitted test sim")
                  ),
                  c(760, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 datasets = "data-raw/data/schoolsdat2.rds")
                  ),
                  c(380, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 datasets = c("data-raw/data/schoolsdat2.rds",
                                              "data-raw/data/schoolsdat3.rds"))
                  ),
                  c(760, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 parameters = "mu")
                  ),
                  c(80, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 parameters = c("mu", "eta[1]"))
                  ),
                  c(160, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 parameters = c("mu", "eta"))
                  ),
                  c(720, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 parameters = c("eta[1]", "eta"))
                  ),
                  c(640, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 parameters = "mu",
                                 param_expand = FALSE)
                  ),
                  c(80, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 parameters = "mu",
                                 param_expand = FALSE)
                  ),
                  c(80, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 parameters = c("eta"),
                                 param_expand = FALSE)
                  ),
                  c(0, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 parameters = c("eta[1]"),
                                 param_expand = FALSE)
                  ),
                  c(80, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 parameters = c("eta[1]", "eta"),
                                 param_expand = FALSE)
                  ),
                  c(80, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 estimates = "Rhat")
                  ),
                  c(152, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 estimates = c("Rhat", "97.5%"))
                  ),
                  c(304, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 estimates = c("Rhat", "97.5%", "test"))
                  ),
                  c(304, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 estimates = c("Rhat", "97.5%", "test"))
                  ),
                  c(304, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 values = function(x) x > 1.1)
                  ),
                  c(703, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 values = function(x) x < 0)
                  ),
                  c(324, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 estimates = "Rhat",
                                 values = function(x) x > 1.1)
                  ),
                  c(0, 5))

                  expect_equal(dim(
                    extract_data(extract_test_data,
                                 estimates = "Rhat",
                                 values = function(x) x < 1.1 & x > 1)
                  ),
                  c(81, 5))

                  ## if "all" is provided for an arg it must be alone
                  expect_error(extract_data(
                    extract_test_data,
                    sim_names = c("all",
                                 "refitted test sim")
                  ),
                  "if sim_names argument contains \"any\", length\\(sim_names\\) must be 1")

                  expect_error(extract_data(
                    extract_test_data,
                    datasets = c("all",
                                 "data-raw/data/schoolsdat2.rds")
                  ),
                  "if datasets argument contains \"any\", length\\(datasets\\) must be 1")

                  expect_error(extract_data(
                    extract_test_data,
                    parameters = c("all",
                                   "eta")
                  ),
                  "if parameters argument contains \"any\", length\\(parameters\\) must be 1")

                  expect_error(extract_data(
                    extract_test_data,
                    estimates = c("all",
                                  "Rhat")
                  ),
                  "if estimates argument contains \"any\", length\\(estimates\\) must be 1")

                  ## extract should return a dataframe
                  expect_true(is.data.frame(extract_data(extract_test_data)))

                })
