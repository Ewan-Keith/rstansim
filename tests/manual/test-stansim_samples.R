library(rstansim)
library(testthat)

context("manual tests checking that stan manages to sample at all")

test_that("stan_sim returns a valid stansim object", {

  ## setup and run tiny example
  test_stan_args <- list(file = "tests/testthat/data-raw/8schools.stan",
                       iter = 1, chains = 1)

  output <- stansim(
    stan_args = test_stan_args,
    sim_data = dir("tests/testthat/data-raw/data", full.names = TRUE)[1],
    use_cores = 1
  )

  # basic s3 class check
  expect_s3_class(output, "stansim_single")

  # check s3 slots
  expect_equal(names(output), c("sim_name", "start_time", "end_time",
                               "model_name", "model_code", "sim_seed",
                               "instances", "data"))

  # check data dimensions
  expect_equal(dim(output$data), c(190, 4))

  ## instance checks
  # instance names
  expect_equal(names(output$instances[[1]]),
               c("data_name", "ran_at", "elapsed_time", "stan_inits",
                 "stan_args", "seed", "warnings"))

  # stan_args  names
  expect_equal(names(unlist(output$instances[[1]]$stan_args)),
               c("chain_id", "iter", "thin", "seed", "warmup", "init",
                 "algorithm", "method" ))

  # check that .cache has been deleted
  expect_false(dir.exists(".cache"))

})
