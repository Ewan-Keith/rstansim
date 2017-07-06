context("stan_sim input exceptions should be handled correctly")

sim_data_test <- "data-raw/data/schoolsdat1.rds"

test_that("stan_sim fails correctly with invalid parameters", {

  # sim_data must be provided
  expect_error(stan_sim(),
               "sim_data must be specified")

  expect_error(stan_sim(sim_data = NULL),
               "sim_data must be specified")

  expect_error(stan_sim(sim_data = list()),
               "sim_data must have length > 0")

  expect_error(stan_sim(sim_data = "not a real file.rds"),
               "sim_data arg \"not a real file.rds\" could not be found")

  expect_error(stan_sim(sim_data = "data-raw/8schools.stan"),
               "sim_data arg \"data-raw/8schools.stan\" is not a .rds file")

  # calc_loo must be boolean
  expect_error(stan_sim(sim_data = sim_data_test, calc_loo = 55),
               "calc_loo must be of type logical")

  expect_error(stan_sim(sim_data = sim_data_test, calc_loo = "TRUE"),
               "calc_loo must be of type logical")

  # calc_loo cant be missing
  expect_error(stan_sim(sim_data = sim_data_test, calc_loo = NA),
               "calc_loo cannot be NA")

  # use_cores must be a positive integer
  expect_error(stan_sim(sim_data = sim_data_test, use_cores = "test"),
               "use_cores must be a positive integer")

  expect_error(stan_sim(sim_data = sim_data_test, use_cores = NULL),
               "use_cores must be a positive integer")

  expect_error(stan_sim(sim_data = sim_data_test, use_cores = 2.5),
               "use_cores must be a positive integer")

  expect_error(stan_sim(sim_data = sim_data_test, use_cores = -1),
               "use_cores must be a positive integer")

  expect_error(stan_sim(sim_data = sim_data_test, use_cores = TRUE),
               "use_cores must be a positive integer")

  # stan args must be of type list
  expect_error(stan_sim(sim_data = sim_data_test, stan_args = "1"),
               "stan_args must be of type list")

  expect_error(stan_sim(sim_data = sim_data_test, stan_args = c("1", "abc")),
               "stan_args must be of type list")

  expect_error(stan_sim(sim_data = sim_data_test, stan_args = NA),
               "stan_args must be of type list")

  expect_error(stan_sim(sim_data = sim_data_test, stan_args = NULL),
               "stan_args must be of type list")

  expect_error(stan_sim(sim_data = sim_data_test, stan_args = TRUE),
               "stan_args must be of type list")

  # parameters must be characters
  expect_error(stan_sim(sim_data = sim_data_test, parameters =  55),
               "parameters must be of type character")

  expect_error(stan_sim(sim_data = sim_data_test,
                        parameters =  c(55, 45, 64)),
               "parameters must be of type character")

  expect_error(stan_sim(sim_data = sim_data_test, parameters =  list("test")),
               "parameters must be of type character")

  expect_error(stan_sim(sim_data = sim_data_test, parameters =  NULL),
               "parameters must be of type character")

  expect_error(stan_sim(sim_data = sim_data_test, parameters =  NA),
               "parameters must be of type character")

  expect_error(stan_sim(sim_data = sim_data_test, parameters =  TRUE),
               "parameters must be of type character")

  # if "all" provided to parameters it must be alone
  expect_error(
    stan_sim(sim_data = sim_data_test, parameters =  c("all",
                                                "eta")),
    "if parameters argument contains \"any\", length\\(parameters\\) must be 1"
  )

  # stan_warnings must be one of print, catch, suppress
  expect_error(stan_sim(sim_data = sim_data_test, stan_warnings = TRUE),
               paste0("stan_warnings must be one of \"print\", ",
                      "\"catch\", or \"suppress\""))

  expect_error(stan_sim(sim_data = sim_data_test, stan_warnings = "file"),
               paste0("stan_warnings must be one of \"print\", ",
                      "\"catch\", or \"suppress\""))

  expect_error(stan_sim(sim_data = sim_data_test, stan_warnings = 25),
               paste0("stan_warnings must be one of \"print\", ",
                      "\"catch\", or \"suppress\""))



  expect_error(stan_sim(sim_data = sim_data_test, calc_loo = 55),
               "loo must be of type logical")

  expect_error(stan_sim(sim_data = sim_data_test, use_cores = -1),
               "use_cores must be a positive integer")

  expect_error(stan_sim(sim_data = sim_data_test, use_cores = 4.5),
               "use_cores must be a positive integer")

  # stan_args$data must not be provided
  expect_error(stan_sim(sim_data = sim_data_test,
                        stan_args = list("data" = "test")),
               paste("stan_args\\$data cannot be directly specified,",
                     "sim_data should be used"))

  # cache must be Boolean
  expect_error(stan_sim(sim_data = sim_data_test, cache = 555),
               "cache must be of type logical")

  expect_error(stan_sim(sim_data = sim_data_test, cache = "TRUE"),
               "cache must be of type logical")

  expect_error(stan_sim(sim_data = sim_data_test, cache = NULL),
               "cache must be of type logical")

  # sample_file must be NULL
  expect_error(stan_sim(sim_data = sim_data_test, stan_args =
                          list("sample_file" = "test.txt")))

  # probs must be numeric between 0 and 1
  expect_error(stan_sim(sim_data = sim_data_test, probs = 1.1),
               "all probs arguments must be numbers between 0 and 1")

  expect_error(stan_sim(sim_data = sim_data_test, probs = -.5),
               "all probs arguments must be numbers between 0 and 1")

  expect_error(stan_sim(sim_data = sim_data_test, probs = "test"),
               "all probs arguments must be numbers between 0 and 1")

  expect_error(stan_sim(sim_data = sim_data_test, probs = c(.5, "test")),
               "all probs arguments must be numbers between 0 and 1")

  # estimates have to be one of the presepcified values
  expect_error(
    stan_sim(sim_data = sim_data_test, estimates = "median"),
    paste(
      "estimate arguments must be one of \"Rhat\",",
      "\"n_eff\", \"mean\", \"se_mean\", or \"sd\"."
    )
  )

  expect_error(
    stan_sim(sim_data = sim_data_test, estimates = 555),
    paste(
      "estimate arguments must be one of \"Rhat\",",
      "\"n_eff\", \"mean\", \"se_mean\", or \"sd\"."
    )
  )

  expect_error(
    stan_sim(sim_data = sim_data_test, estimates = c("mean", "n_eff", "test")),
    paste(
      "estimate arguments must be one of \"Rhat\",",
      "\"n_eff\", \"mean\", \"se_mean\", or \"sd\"."
    )
  )



})

test_that("where args are ignored return the correct warning message", {

  # stan_args$cores will just be overwritten, cache error used
  # to terminate the test in a controlled way
  expect_error(
  expect_warning(
    stan_sim(sim_data = sim_data_test, stan_args = list("cores" = 5),
             paste("stan_sim is parallel across stan instances,",
                   "not within. stan_arg$cores is fixed to 1")
             )
    ), "calc_loo must be of type logical")


})
