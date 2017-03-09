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

  expect_error(stan_sim(sim_data = "test_only", max_failures = -1),
               "max_failures must be a positive integer")

  expect_error(stan_sim(sim_data = "test_only", max_failures = 4.5),
               "max_failures must be a positive integer")

  expect_error(stan_sim(sim_data = "test_only", max_rhat = "test"),
               "max_rhat must be numeric")

  expect_error(stan_sim(sim_data = "test_only", max_rhat = 0.8),
               "max_rhat must be >= 1")
})
