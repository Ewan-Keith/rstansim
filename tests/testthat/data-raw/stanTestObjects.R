#### code that produces various objects (usually stan and stansim obs)
## to be used in unit testing

library(rstansim)
library(rstan)

## small scale 8schools example for basic stansim methods testing

test_stan_args <- list(file = "tests/testthat/data-raw/8schools.stan",
                     iter = 1000, chains = 4)

test_stansim <- stan_sim(stan_args = test_stan_args,
                         sim_data = dir("data-raw/data",
                                        full.names = TRUE), use_cores = 4)

# saveRDS(test_stansim, "tests/testthat/objects/test_stansim.rds")

## small scale 8schools example for testing that needs a stanfit object

test_stan_args <- list(file = "tests/testthat/data-raw/8schools.stan",
                       iter = 1000, chains = 4,
                       data = readRDS(dir("tests/testthat/data-raw/data",
                                          full.names = TRUE)[1]))

test_stanfit <- do.call(stan, test_stan_args)

# saveRDS(test_stanfit, "tests/testthat/objects/test_stanfit.rds")
