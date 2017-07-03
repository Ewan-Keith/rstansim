library(rstansim)
library(testthat)

context("basic runs to test that stan manages to sample at all, small and fast")

test_that("stan_sim returns a valid stansim object", {

  ## basic s3 class check
  testStanArgs <- list(file = 'tests/testthat/data-raw/8schools.stan',
                       iter = 1, chains = 1)

  expect_s3_class(stan_sim(
    stan_args = testStanArgs,
    sim_data = dir("tests/testthat/data-raw/data", full.names = TRUE)[1],
    use_cores = 1
  ), "stansim")

})
