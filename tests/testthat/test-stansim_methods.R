context("stansim_single methods should function correctly")

test_that(paste("extract_data.stansim_single function should return",
          "expected results"), {

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
                 datasets = "tests/testthat/data-raw/data/schoolsdat2.rds")
  ),
  c(190, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 datasets = c("tests/testthat/data-raw/data/schoolsdat2.rds",
                              "tests/testthat/data-raw/data/schoolsdat3.rds"))
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
  c(349, 4))

  expect_equal(dim(
    extract_data(extract_test_data,
                 values = function(x) x < 0)
  ),
  c(161, 4))

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
  c(46, 4))

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

test_that(paste(
  "print.stansim_single function should print",
  "expected output"), {

    ## read in test stansim_single obj to print
    extract_test_data <- readRDS("objects/test_stansim.rds")

    caught_print <- utils::capture.output(print(extract_test_data))

    expect_true(grepl("Stan Simulation Title: .*", caught_print[1]))

    expect_true(grepl("Model Name: .*", caught_print[2]))

    expect_true(grepl("", caught_print[3]))

    expect_true(grepl("Started Running at: .*", caught_print[4]))

    expect_true(grepl("Finished Running at: .*", caught_print[5]))

    expect_true(grepl("", caught_print[6]))

    expect_true(grepl("Number of Models Fitted: \\d*", caught_print[7]))

    expect_true(grepl("", caught_print[8]))

    expect_true(grepl("Parameters Recorded: \\d*", caught_print[9]))

    expect_true(grepl(
      "eta\\[1\\]   eta\\[2\\]   eta\\[3\\]   eta\\[4\\]   eta\\[5\\]  ",
      caught_print[10]))

    expect_true(grepl(
      "eta\\[6\\]   eta\\[7\\]   eta\\[8\\]   lp__     mu      ",
      caught_print[11]))

    expect_true(grepl(
      "tau      theta\\[1\\] theta\\[2\\] theta\\[3\\] theta\\[4\\]",
      caught_print[12]))

    expect_true(grepl(
      "theta\\[5\\] theta\\[6\\] theta\\[7\\] theta\\[8\\]         ",
      caught_print[13]))

    expect_true(grepl("", caught_print[14]))

    expect_true(grepl("Estimates Recorded: \\d*", caught_print[15]))

    expect_true(grepl(
      "2.5%    25%     50%     75%     97.5%  ",
      caught_print[16]))

    expect_true(grepl(
      "mean    n_eff   Rhat    sd      se_mean",
      caught_print[17]))
})
