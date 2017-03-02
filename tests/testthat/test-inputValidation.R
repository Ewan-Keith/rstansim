context("stanSim input should be of correct types")

test_that("Invalid parameters return the correct error message", {

  ##-------------------------------------------------
  ## raw inputs must be of type list
  expect_error(stanSim(stanArgs = "1"),
               "stanArgs must be a list of parameters")

  expect_error(stanSim(simArgs = "1"),
               "simArgs must be a list of parameters")

  expect_error(stanSim(returnArgs = "1"),
               "returnArgs must be a list of parameters")

  ##-------------------------------------------------
  ## stanArgs checks

  expect_error(stanSim(stanArgs = list("file" = 555)),
               "stanArgs\\$file must be of type 'character' if specified")

  expect_error(stanSim(stanArgs = list("data" = 555)),
               "stanArgs\\$data must be of type 'list' if specified")

  expect_error(stanSim(stanArgs = list("iter" = -1)),
               "stanArgs\\$iter must be a positive integer")

  expect_error(stanSim(stanArgs = list("iter" = 500.5)),
               "stanArgs\\$iter must be a positive integer")

  expect_error(stanSim(stanArgs = list("chains" = -1)),
               "stanArgs\\$chains must be a positive integer")

  expect_error(stanSim(stanArgs = list("chains" = 500.5)),
               "stanArgs\\$chains must be a positive integer")

  ##-------------------------------------------------
  ## simArgs checks

  #expect_error(stanSim(),
  #             "simArgs\\$simData must be specified")

  expect_error(stanSim(simArgs = list("LOO" = 55)),
               "simArgs\\$LOO must be Boolean")

  expect_error(stanSim(simArgs = list("useCores" = -1)),
               "simArgs\\$useCores must be a positive integer")

  expect_error(stanSim(simArgs = list("useCores" = 4.5)),
               "simArgs\\$useCores must be a positive integer")

  expect_error(stanSim(simArgs = list("useCores" = 9999)),
               "UseCores parameter must be less thanthe number of detected cores")

  expect_error(stanSim(simArgs = list("maxFailures" = -1)),
               "simArgs\\$maxFailures must be a positive integer")

  expect_error(stanSim(simArgs = list("maxFailures" = 4.5)),
               "simArgs\\$maxFailures must be a positive integer")

  expect_error(stanSim(simArgs = list("maxRhat" = "hello")),
               "simArgs\\$maxRhat must be numeric")

  expect_error(stanSim(simArgs = list("maxRhat" = 0.8)),
               "simArgs\\$maxRhat must be >= 1")
})
