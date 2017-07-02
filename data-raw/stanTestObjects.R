#### code that produces various objects (usually stan and stansim obs)
## to be used in unit testing

library(rstansim)

## small scale 8schools example for basic stansim methods testing

test_stan_args <- list(file = 'data-raw/8schools.stan',
                     iter = 1000, chains = 4)

test_stansim <- stan_sim(stan_args = test_stan_args, sim_data = dir("data-raw/data", full.names = TRUE), use_cores = 4)

# saveRDS(test_stansim, "tests/testthat/objects/test_stansim.rds")
