context("stan_sim input exceptions should be handled correctly")

test_that("Invalid parameters return the correct error message", {

  # sim_data must be provided
  expect_error(stan_sim(),
               "sim_data must be specified")

  expect_error(stan_sim(sim_data = NULL),
               "sim_data must be specified")

  # calc_loo must be boolean
  expect_error(stan_sim(sim_data = "test", calc_loo = 55),
               "calc_loo must be of type logical")

  expect_error(stan_sim(sim_data = "test", calc_loo = "TRUE"),
               "calc_loo must be of type logical")

  # calc_loo cant be missing
  expect_error(stan_sim(sim_data = "test", calc_loo = NA),
               "calc_loo cannot be NA")

  # use_cores must be a positive integer
  expect_error(stan_sim(sim_data = "test", use_cores = "test"),
               "use_cores must be a positive integer")

  expect_error(stan_sim(sim_data = "test", use_cores = NULL),
               "use_cores must be a positive integer")

  expect_error(stan_sim(sim_data = "test", use_cores = 2.5),
               "use_cores must be a positive integer")

  expect_error(stan_sim(sim_data = "test", use_cores = -1),
               "use_cores must be a positive integer")

  expect_error(stan_sim(sim_data = "test", use_cores = TRUE),
               "use_cores must be a positive integer")

  # stan args must be of type list
  expect_error(stan_sim(sim_data = "test", stan_args = "1"),
               "stan_args must be of type list")

  expect_error(stan_sim(sim_data = "test", stan_args = c("1", "abc")),
               "stan_args must be of type list")

  expect_error(stan_sim(sim_data = "test", stan_args = NA),
               "stan_args must be of type list")

  expect_error(stan_sim(sim_data = "test", stan_args = NULL),
               "stan_args must be of type list")

  expect_error(stan_sim(sim_data = "test", stan_args = TRUE),
               "stan_args must be of type list")

  # parameters must be characters
  expect_error(stan_sim(sim_data = "test", parameters =  55),
               "parameters must be of type character")

  expect_error(stan_sim(sim_data = "test", parameters =  c(55, 45, 64)),
               "parameters must be of type character")

  expect_error(stan_sim(sim_data = "test", parameters =  list("test")),
               "parameters must be of type character")

  expect_error(stan_sim(sim_data = "test", parameters =  NULL),
               "parameters must be of type character")

  expect_error(stan_sim(sim_data = "test", parameters =  NA),
               "parameters must be of type character")

  expect_error(stan_sim(sim_data = "test", parameters =  TRUE),
               "parameters must be of type character")

  # stan_warnings must be one of print, catch, suppress
  expect_error(stan_sim(sim_data = "test", stan_warnings = TRUE),
               paste0("stan_warnings must be one of \"print\", ",
                      "\"catch\", or \"suppress\""))

  expect_error(stan_sim(sim_data = "test", stan_warnings = "file"),
               paste0("stan_warnings must be one of \"print\", ",
                      "\"catch\", or \"suppress\""))

  expect_error(stan_sim(sim_data = "test", stan_warnings = 25),
               paste0("stan_warnings must be one of \"print\", ",
                      "\"catch\", or \"suppress\""))



  expect_error(stan_sim(sim_data = "test_only", loo = 55),
               "loo must be of type logical")

  expect_error(stan_sim(sim_data = "test_only", use_cores = -1),
               "use_cores must be a positive integer")

  expect_error(stan_sim(sim_data = "test_only", use_cores = 4.5),
               "use_cores must be a positive integer")

  # stan_args$data must not be provided
  expect_error(stan_sim(sim_data = "test_only", stan_args = list("data" = "test")),
               "stan_args$data cannot be directly specified, sim_data should be used")



})

test_that("where args are ignored return the correct warning message", {

  ##-------------------------------------------------
  ## stan args must be of type list
  # expect_error(stan_sim(stan_args = "1"),
  #              "stan_args must be a list of stan parameters")


})
