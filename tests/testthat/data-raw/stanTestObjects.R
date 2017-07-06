#### code that produces various objects (usually stan and stansim obs)
## to be used in unit testing

library(rstansim)
library(rstan)

#-----------------------------------------------------------------
#### small scale 8schools example for basic stansim methods testing ####

test_stan_args <- list(file = "tests/testthat/data-raw/8schools.stan",
                     iter = 1000, chains = 4)

test_stansim <- stan_sim(stan_args = test_stan_args,
                         sim_data = dir("data-raw/data",
                                        full.names = TRUE), use_cores = 4)

# saveRDS(test_stansim, "tests/testthat/objects/test_stansim.rds")

#-----------------------------------------------------------------
#### small scale 8schools example for testing that needs a stanfit object ####

test_stan_args <- list(file = "tests/testthat/data-raw/8schools.stan",
                       iter = 1000, chains = 4,
                       data = readRDS(dir("tests/testthat/data-raw/data",
                                          full.names = TRUE)[1]))

test_stanfit <- do.call(stan, test_stan_args)

# saveRDS(test_stanfit, "tests/testthat/objects/test_stanfit.rds")

#-----------------------------------------------------------------
#### loo friendly small stanfit for extraction testing ####

# Prepare data
url <- "http://stat.columbia.edu/~gelman/arm/examples/arsenic/wells.dat"
wells <- read.table(url)
wells$dist100 <- with(wells, dist / 100)
X <- model.matrix(~ dist100 + arsenic, wells)

row_nums <- sample(nrow(X), size=50, replace=FALSE)
small_X <- X[row_nums, ]

standata <- list(y = wells$switch[row_nums], X = small_X, N = nrow(small_X), P = ncol(small_X))

# Fit model
fit_loo <- stan("tests/testthat/data-raw/logistic.stan", data = standata, iter = 500)

# saveRDS(fit_loo, "tests/testthat/objects/test_stanfit_loo.rds")

#-----------------------------------------------------------------
#### output of fit_stan_warnings with warnings treated in three ways

test_stan_args <- list(file = "tests/testthat/data-raw/8schools.stan",
                       iter = 100, chains = 4,
                       data = readRDS(dir("tests/testthat/data-raw/data",
                                          full.names = TRUE)[1]))

do.call(rstan::stan, test_stan_args)


fit_stan_warnings_catch <-
  rstansim:::fit_stan_warnings(test_stan_args, w_handler_catch)

