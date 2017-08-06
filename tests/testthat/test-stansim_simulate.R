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
#                         input_data = reg_data,
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

  # data name must be character
  expect_error(stansim_simulate(file = "test",
                                data_name = 55),
               "data_name must be of type character")

  # path must be type character [1]
  expect_error(stansim_simulate(file = "test",
                                path = 55),
               "path must be NULL or of type character")

  # path must be type character [2]
  expect_error(stansim_simulate(file = "test",
                                path = NA),
               "path must be NULL or of type character")

  # input_data must be NULL or list [1]
  expect_error(stansim_simulate(file = "test",
                                input_data = 55),
               "input_data must be NULL or of type list")

  # input_data must be NULL or list [2]
  expect_error(stansim_simulate(file = "test",
                                input_data = "test"),
               "input_data must be NULL or of type list")

  # vars must be type character [1]
  expect_error(stansim_simulate(file = "test",
                                vars = 55),
               "vars must be of type character")

  # datasets must be positive integer [1]
  expect_error(stansim_simulate(file = "test",
                                datasets = -1),
               "datasets must be a positive integer")

  # datasets must be positive integer [2]
  expect_error(stansim_simulate(file = "test",
                                datasets = 2.1),
               "datasets must be a positive integer")

  # vars must be type character [2]
  expect_error(stansim_simulate(file = "test",
                                vars = NA),
               "vars must be of type character")

  # if "all" provided to vars it must be alone
  expect_error(
    stansim_simulate(file = "test",
                     vars = c("all", "test")),
    "if vars argument contains \"all\", length\\(vars\\) must be 1"
  )

  # param_values must be NULL or list [1]
  expect_error(stansim_simulate(file = "test",
                                param_values = 55),
               "param_values must be NULL or of type list")

  # param_values must be NULL or list [2]
  expect_error(stansim_simulate(file = "test",
                                param_values = "test"),
               "param_values must be NULL or of type list")

  # return_object must of type logical [1]
  expect_error(stansim_simulate(file = "test",
                                return_object = 55),
               "return_object must be of type logical")

  # return_object must of type logical [2]
  expect_error(stansim_simulate(file = "test",
                                return_object = "test"),
               "return_object must be of type logical")

  # sim_drop must of type logical [1]
  expect_error(stansim_simulate(file = "test",
                                sim_drop = 55),
               "sim_drop must be of type logical")

  # sim_drop must of type logical [2]
  expect_error(stansim_simulate(file = "test",
                                sim_drop = "test"),
               "sim_drop must be of type logical")

})

#-----------------------------------------------------------------
#### output verification ####

test_that("stansim_simulate returns correct output", {

  # check that testdir doesn't already exist
  expect_false(dir.exists("testdir"))

  ## prep arguments
  reg_data <- list("N" = 100, "x" = rep(0, 100), "y" = rep(0, 100))
  test_vals <- list("alpha" = 100, "beta" = -5, "sigma" = 20)

  catch <-
    capture_output(
      output1 <- stansim_simulate(
        file = 'data-raw/simtestreg.stan',
        data_name = "test data",
        input_data = reg_data,
        vars = c("sim_x", "sim_y", "N"),
        path = "testdir",
        param_values = test_vals,
        datasets = 5
      )
    )

  # check that testdir now exists
  expect_true(dir.exists("testdir"))

  # expect class
  expect_s3_class(output1, "stansim_data")

  # expect list
  expect_type(output1, "list")

  # expect length
  expect_length(output1, 4)

  # expect dim names
  expect_named(output1, c("data_name", "datasets", "model_name", "model_code"))

  # expect data_name type
  expect_type(output1$data_name, "character")

  # expect data name value
  expect_equal(output1$data_name, "test data")

  # expect data is character
  expect_type(output1$datasets, "character")

  # expect datasets length
  expect_length(output1$datasets, 5)

  # expect data list names
  expect_equal(output1$datasets, c("test data_1.rds", "test data_2.rds", "test data_3.rds",
                               "test data_4.rds", "test data_5.rds"))

  ## for each data list
  for (dnames in dir("testdir", full.names = TRUE)) {

    d <- readRDS(dnames)

    # expect a list
    expect_type(d, "list")

    # expect length 3
    expect_length(d, 3)

    # expect names
    expect_named(d, c("x", "y", "N"))

    # for x and y
    for (i in c("x", "y")) {
      # expect dimension
      expect_length(d[[i]], 100)

      # expect numeric
      expect_type(d[[i]], "double")
    }

    # expect N numeric
    expect_type(d$N, "double")

    # expect N dimension
    expect_length(d$N, 1)

    # expect N value
    expect_equal(d$N, 100)
  }

  # expect model name is character
  expect_type(output1$model_name, "character")

  # expect model name value
  expect_equal(output1$model_name, "simtestreg")

  # expect model_code is character
  expect_type(output1$model_code, "character")

  # expect start of model code
  expect_match(
    output1$model_code,
    "data \\{\\nint<lower=0> N;\\nvector\\[N\\] x;\\nvector\\[N\\] y;\\n\\}")

  # check that testdir still exists
  expect_true(dir.exists("testdir"))

  # delete testdir
  unlink("testdir", recursive = TRUE)

  # check that testdir doesn't exist
  expect_false(dir.exists("testdir"))
  })
