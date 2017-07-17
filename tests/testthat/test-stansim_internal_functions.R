context("test internal, non-exposed functions")

# rstan needs loaded for loo
suppressPackageStartupMessages(library(rstan))


# read in prepared stanfit object
test_stanfit <- readRDS("objects/test_stanfit.rds")

# set up default args for space
def_probs <- c(.025, .25, .5, .75, .975)
def_estimates <- c("mean", "se_mean", "sd", "n_eff", "Rhat")

#-----------------------------------------------------------------
#### param_extract failing tests ####
test_that("param_extract should fail correctly with bad input", {

  # when no params recognised fail immediately and tell user
  expect_error(
    rstansim:::param_extract(test_stanfit, calc_loo = FALSE,
                  parameters = "notrealparam",
                  probs = def_probs,
                  estimates = def_estimates,
                  data = "datafile location.rds"),
    paste(
      "None of the provided parameters were identified in the model,",
      "check your stan code and arguments.")
  )

  expect_error(
    rstansim:::param_extract(test_stanfit, calc_loo = FALSE,
                  parameters = c("notrealparam", "notrealparam2"),
                  probs = def_probs,
                  estimates = def_estimates,
                  data = "datafile location.rds"),
    paste(
      "None of the provided parameters were identified in the model,",
      "check your stan code and arguments.")
  )

  expect_error(
    rstansim:::param_extract(test_stanfit, calc_loo = FALSE,
                  parameters = c("eta", "notrealparam", "notrealparam2"),
                  probs = def_probs,
                  estimates = def_estimates,
                  data = "datafile location.rds"),
    paste0("Following parameter\\(s\\) could not be found in the model, ",
           "check your stan code and arguments.\n\\[",
           "notrealparam, notrealparam2\\]")
  )

  expect_error(
    rstansim:::param_extract(test_stanfit, calc_loo = FALSE,
                  parameters = c("eta", "notrealparam", "notrealparam2",
                                 "eta[1]"),
                  probs = def_probs,
                  estimates = def_estimates,
                  data = "datafile location.rds"),
    paste0("Following parameter\\(s\\) could not be found in the model, ",
           "check your stan code and arguments.\n\\[",
           "notrealparam, notrealparam2, eta\\[1\\]\\]")
  )

})

#-----------------------------------------------------------------
#### param_extract correct testing (LOO=F) ####
test_that("param_extract should return a correct dataframe", {

  # prepare default param_extract output
  test_param_extract <- rstansim:::param_extract(
    test_stanfit,
    calc_loo = FALSE,
    parameters = "all",
    probs = def_probs,
    estimates = def_estimates,
    data = "datafile location.rds"
  )

  ## param_extract should return a dataframe
  expect_true(is.data.frame(test_param_extract))

  ## param_extract dimensions should be as expected
  expect_equal(dim(test_param_extract), c(190, 4))

  ## colnames should be as expected
  expect_equal(colnames(test_param_extract),
               c("data", "parameter", "estimate", "value"))

  # column types should be as expected
  expect_equal(sapply(test_param_extract, typeof),
               c("data" = "character",
                 "parameter" = "character",
                 "estimate" = "character",
                 "value" = "double"))

  # prepare parameter filter param_extract output
  test_param_pfilter_extract <- rstansim:::param_extract(
    test_stanfit,
    calc_loo = FALSE,
    parameters = "eta",
    probs = def_probs,
    estimates = def_estimates,
    data = "datafile location.rds"
  )

  ## pfilter dimensions should be as expected
  expect_equal(dim(test_param_pfilter_extract), c(80, 4))

  test_param_pfilter_extract2 <- rstansim:::param_extract(
    test_stanfit,
    calc_loo = FALSE,
    parameters = c("eta", "mu"),
    probs = def_probs,
    estimates = def_estimates,
    data = "datafile location.rds"
  )

  ## pfilter2 dimensions should be as expected
  expect_equal(dim(test_param_pfilter_extract2), c(90, 4))

  # prepare probs filter param_extract output
  test_param_probsfilter_extract <- rstansim:::param_extract(
    test_stanfit,
    calc_loo = FALSE,
    parameters = "all",
    probs = c(.1, .9),
    estimates = def_estimates,
    data = "datafile location.rds"
  )

  ## pfilter dimensions should be as expected
  expect_equal(dim(test_param_probsfilter_extract), c(133, 4))

  # prepare probs filter2 param_extract output
  test_param_probsfilter_extract2 <- rstansim:::param_extract(
    test_stanfit,
    calc_loo = FALSE,
    parameters = "all",
    probs = .5,
    estimates = def_estimates,
    data = "datafile location.rds"
  )

  ## pfilter dimensions should be as expected
  expect_equal(dim(test_param_probsfilter_extract2), c(114, 4))

  # prepare probs filter3 param_extract output
  test_param_probsfilter_extract3 <- rstansim:::param_extract(
    test_stanfit,
    calc_loo = FALSE,
    parameters = "all",
    probs = NA,
    estimates = def_estimates,
    data = "datafile location.rds"
  )

  ## efilter3 dimensions should be as expected
  expect_equal(dim(test_param_probsfilter_extract3), c(95, 4))

  # prepare estimates filter param_extract output
  test_param_efilter_extract <- rstansim:::param_extract(
    test_stanfit,
    calc_loo = FALSE,
    parameters = "all",
    probs = def_probs,
    estimates = "Rhat",
    data = "datafile location.rds"
  )

  ## efilter dimensions should be as expected
  expect_equal(dim(test_param_efilter_extract), c(114, 4))

  # prepare estimates filter2 param_extract output
  test_param_efilter_extract2 <- rstansim:::param_extract(
    test_stanfit,
    calc_loo = FALSE,
    parameters = "all",
    probs = def_probs,
    estimates = c("Rhat", "mean"),
    data = "datafile location.rds"
  )

  ## efilter2 dimensions should be as expected
  expect_equal(dim(test_param_efilter_extract2), c(133, 4))

  # prepare estimates filter3 param_extract output
  test_param_efilter_extract3 <- rstansim:::param_extract(
    test_stanfit,
    calc_loo = FALSE,
    parameters = "all",
    probs = def_probs,
    estimates = NA,
    data = "datafile location.rds"
  )

  ## efilter3 dimensions should be as expected
  expect_equal(dim(test_param_efilter_extract3), c(95, 4))

})

#-----------------------------------------------------------------
#### loo extract testing ####
test_that("param_extract with LOO mocked should return correct data", {

  test_stanfit_loo <- readRDS("objects/test_stanfit_loo.rds")

  # suppress warnings because of the unclsoed connection, not sure how to
  # close this if loo isn't
  suppressWarnings(
    loo_extract <- rstansim:::param_extract(
      test_stanfit_loo,
      calc_loo = TRUE,
      parameters = c("beta"),
      probs = def_probs,
      estimates = def_estimates,
      data = "datafile location.rds"
    )
  )

  ## param_extract should return a dataframe
  expect_true(is.data.frame(loo_extract))

  ## param_extract dimensions should be as expected
  expect_equal(dim(loo_extract), c(36, 4))

  ## colnames should be as expected
  expect_equal(colnames(loo_extract),
               c("data", "parameter", "estimate", "value"))

  # column types should be as expected
  expect_equal(sapply(loo_extract, typeof),
               c("data" = "character",
                 "parameter" = "character",
                 "estimate" = "character",
                 "value" = "double"))

  # only betas and loo params should be present
  expect_equal(unique(loo_extract$parameter),
               c("beta[1]", "beta[2]", "beta[3]",
                 "elpd_loo", "looic", "p_loo"))

  # estimate and se should both be estimates from loo

  expect_true("se" %in% unique(loo_extract$estimate) &
                "estimate" %in% unique(loo_extract$estimate))

})




