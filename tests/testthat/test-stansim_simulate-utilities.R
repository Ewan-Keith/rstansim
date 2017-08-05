context("simulate_internal utility function works as expected")

# read in pre-compiled stan_model
compiled_model <- readRDS("objects/sim_compiled.rds")

## prep arguments
reg_data <- list("N" = 100, "x" = rep(0, 100), "y" = rep(0, 100))
test_vals <- list("alpha" = 100, "beta" = -5, "sigma" = 20)

test_that("simulate_internal returns correctly formatted output (sim_drop = T)", {

  # run simulate_internal with sim_drop = T
  catch <-
    capture_output(
      sim_out <- simulate_internal(
        compiled_model,
        reg_data,
        c("sim_x", "sim_y", "N"),
        test_vals,
        sim_drop = T
      )
    )

  # expect a list
  expect_type(sim_out, "list")

  # expect length 3
  expect_length(sim_out, 3)

  # expect names
  expect_named(sim_out, c("x", "y", "N"))

  # for x and y
  for(i in c("x", "y")){
  # expect dimension
    expect_length(sim_out[[i]], 100)

  # expect numeric
    expect_type(sim_out[[i]], "double")
  }

  # expect N numeric
  expect_type(sim_out$N, "double")

  # expect N dimension
  expect_length(sim_out$N, 1)

  # expect N value
  expect_equal(sim_out$N, 100)

})


test_that("simulate_internal returns correctly formatted output (sim_drop = F)", {

  # run simulate_internal with sim_drop = F
  catch <-
    capture_output(
      sim_out <- rstansim:::simulate_internal(
        compiled_model,
        reg_data,
        c("sim_x", "sim_y", "N"),
        test_vals,
        sim_drop = FALSE
      )
    )

  # expect a list
  expect_type(sim_out, "list")

  # expect length 3
  expect_length(sim_out, 3)

  # expect names
  expect_named(sim_out, c("sim_x", "sim_y", "N"))

  # for sim_x and sim_y
  for(i in c("sim_x", "sim_y")){
    # expect dimension
    expect_length(sim_out[[i]], 100)

    # expect numeric
    expect_type(sim_out[[i]], "double")
  }

  # expect N numeric
  expect_type(sim_out$N, "double")

  # expect N dimension
  expect_length(sim_out$N, 1)

  # expect N value
  expect_equal(sim_out$N, 100)

})
