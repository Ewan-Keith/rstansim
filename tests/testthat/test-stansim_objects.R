context("stan_sim objects should function correctly")

test_that("stan_sim_uni object constructor returns correct values", {

  # read in prepared stanfit object
  test_stanfit <- readRDS("objects/test_stanfit.rds")

  test_stansim_uni <-
    stansim_uni(
      test_stanfit,
      data_name = "data_name123",
      ran_at = Sys.time(),
      long_data = param_extract(
        test_stanfit,
        calc_loo = F,
        parameters = "all",
        probs = c(.025, .25, .5, .75, .975),
        estimates = c("mean",
                      "se_mean",
                      "sd",
                      "n_eff",
                      "Rhat"),
        data = "datafile location.rds"
      ),
      stan_warnings = "warning strings",
      cache = F
    )


})

test_that("stan_sim object constructor returns correct values", {



})

