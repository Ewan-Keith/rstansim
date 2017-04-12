context("stan_sim input should be of correct types")

test_that("Invalid parameters return the correct error message", {

  ##-------------------------------------------------
  ## stan args must be of type list
  expect_error(stan_sim(stan_args = "1"),
               "stan_args must be a list of stan parameters")


  ##-------------------------------------------------
  ## simArgs checks

  expect_error(stan_sim(),
               "sim_data must be specified")

  expect_error(stan_sim(sim_data = "test_only", loo = 55),
               "loo must be of type logical")

  expect_error(stan_sim(sim_data = "test_only", use_cores = -1),
               "use_cores must be a positive integer")

  expect_error(stan_sim(sim_data = "test_only", use_cores = 4.5),
               "use_cores must be a positive integer")

})
