context("stansim_simulate functions as expected")

#### current workings ####

# reg_sim <- function(N = 100) {
#   list("N" = N, "x" = rep(0, N), "y" = rep(0, N))
# }
#
# reg_data <- reg_sim(100)
#
# test_vals <- list("alpha" = 100, "beta" = -5, "sigma" = 20)
#
# file <- 'data-raw/simtestreg.stan'
#
# fit <- stansim_simulate(file = file,
#                         holding_data = reg_data,
#                         datasets = 100,
#                         param_values = test_vals,
#                         sim_params = c("sim_x", "sim_y", "N"),
#                         use_cores = 4)
#
#
#
# plot(fit[[1]]$x, fit[[1]]$y)
# abline(lm(fit[[1]]$y ~ fit[[1]]$x))
#
# ## refit test
# fit_test <- rstan::stan(file, data = fit[[1]])

#-----------------------------------------------------------------
#### input verification ####

test_that("stansim_simulate fails as expected with bad input", {

  # file must be type character
  expect_error(stansim_simulate(file = 55),
               "file must be of type character")

  # data nam must be character
  expect_error(stansim_simulate(file = "test",
                                data_name = 55),
               "data_name must be of type character")

  # save_dir must be type character [1]
  expect_error(stansim_simulate(file = "test",
                                save_dir = 55),
               "save_dir must be NULL or of type character")

  # save_dir must be type character [2]
  expect_error(stansim_simulate(file = "test",
                                save_dir = NA),
               "save_dir must be NULL or of type character")

  # holding_data must be NULL or list [1]
  expect_error(stansim_simulate(file = "test",
                                save_dir = "test",
                                holding_data = 55),
               "holding_data must be NULL or of type list")

  # holding_data must be NULL or list [2]
  expect_error(stansim_simulate(file = "test",
                                save_dir = "test",
                                holding_data = "test"),
               "holding_data must be NULL or of type list")

  # param_values must be NULL or list [1]
  expect_error(stansim_simulate(file = "test",
                                save_dir = "test",
                                param_values = 55),
               "param_values must be NULL or of type list")

  # param_values must be NULL or list [2]
  expect_error(stansim_simulate(file = "test",
                                save_dir = "test",
                                param_values = "test"),
               "param_values must be NULL or of type list")

  # sim_drop must of type logical [1]
  expect_error(stansim_simulate(file = "test",
                                save_dir = "test",
                                sim_drop = 55),
               "sim_drop must be of type logical")

  # sim_drop must of type logical [2]
  expect_error(stansim_simulate(file = "test",
                                save_dir = "test",
                                sim_drop = "test"),
               "sim_drop must be of type logical")

})
