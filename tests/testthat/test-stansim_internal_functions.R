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
    param_extract(test_stanfit, calc_loo = FALSE,
                  parameters = "notrealparam",
                  probs = def_probs,
                  estimates = def_estimates,
                  data = "datafile location.rds"),
    paste(
      "None of the provided parameters were identified in the model,",
      "check your stan code and arguments.")
  )

  expect_error(
    param_extract(test_stanfit, calc_loo = FALSE,
                  parameters = c("notrealparam", "notrealparam2"),
                  probs = def_probs,
                  estimates = def_estimates,
                  data = "datafile location.rds"),
    paste(
      "None of the provided parameters were identified in the model,",
      "check your stan code and arguments.")
  )

  expect_error(
    param_extract(test_stanfit, calc_loo = FALSE,
                  parameters = c("eta", "notrealparam", "notrealparam2"),
                  probs = def_probs,
                  estimates = def_estimates,
                  data = "datafile location.rds"),
    paste0("Following parameter\\(s\\) could not be found in the model, ",
           "check your stan code and arguments.\n\\[",
           "notrealparam, notrealparam2\\]")
  )

  expect_error(
    param_extract(test_stanfit, calc_loo = FALSE,
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
  test_param_extract <- param_extract(
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
  test_param_pfilter_extract <- param_extract(
    test_stanfit,
    calc_loo = FALSE,
    parameters = "eta",
    probs = def_probs,
    estimates = def_estimates,
    data = "datafile location.rds"
  )

  ## pfilter dimensions should be as expected
  expect_equal(dim(test_param_pfilter_extract), c(80, 4))

  test_param_pfilter_extract2 <- param_extract(
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
  test_param_probsfilter_extract <- param_extract(
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
  test_param_probsfilter_extract2 <- param_extract(
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
  test_param_probsfilter_extract3 <- param_extract(
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
  test_param_efilter_extract <- param_extract(
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
  test_param_efilter_extract2 <- param_extract(
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
  test_param_efilter_extract3 <- param_extract(
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

  loo_extract <- rstansim:::param_extract(
    test_stanfit_loo,
    calc_loo = TRUE,
    parameters = c("beta"),
    probs = def_probs,
    estimates = def_estimates,
    data = "datafile location.rds"
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

#-----------------------------------------------------------------
#### single_sim testing ####
test_that("single_sim should return correct object (mocked stan fit)", {

  with_mock(
    withCallingHandlers = function(...) {
      test_stanfit
    },

    test_stan_args <-
      list(
        file = "data-raw/8schools.stan",
        iter = 500,
        chains = 4
      ),

    catch_out <- rstansim:::single_sim(
      datafile = dir("data-raw/data",
                     full.names = TRUE)[1],
      stan_args = test_stan_args,
      calc_loo = F,
      parameters = "all",
      probs = c(.025, .25, .5, .75, .975),
      estimates = c("mean", "se_mean",
                    "sd", "n_eff", "Rhat"),
      stan_warnings = "catch",
      cache = F
    ),

    # should be list
    expect_type(catch_out, "list"),

    # should be class stansim_uni
    expect_s3_class(catch_out, "stansim_uni"),

    # should have ten items
    expect_equal(length(catch_out), 10),

    # check list item names are correct
    expect_equal(names(catch_out),
                 c("data_name", "ran_at", "elapsed_time", "stan_inits",
                   "stan_args", "seed", "out_data", "model_name",
                   "model_code", "warnings")),

    # data name should be character
    expect_type(catch_out$data_name, "character"),

    is_date <- function(mydate, date.format = "%d/%m/%y") {
      tryCatch(!is.na(as.Date(mydate, date.format)),
               error = function(err) {FALSE})
    },

    # ran at should be date
    expect_true(is_date(catch_out$ran_at)),

    # elapsed_time should have c(chains, 2) dim
    expect_equal(dim(catch_out$elapsed_time),
                 c(test_stan_args$chains, 2)),

    # elapsed_time should correct colnames
    expect_equal(colnames(catch_out$elapsed_time),
                 c("warmup", "sample")),

    # stan inits should be list
    expect_type(catch_out$stan_inits, "list"),

    # stan inits should have record for each chain
    expect_equal(length(catch_out$stan_inits), 4),

    # stan args that should be same across chains are
    ident <- function(...){
      args <- c(...)
      if( length( args ) > 2L ){
        #  recursively call ident()
        out <- c( identical( args[1] , args[2] ) , ident(args[-1]))
      }else{
        out <- identical( args[1] , args[2] )
      }
      return( all( out ) )
    },

    # iter same
    expect_true(ident(catch_out$stan_args[[1]]$iter,
                      catch_out$stan_args[[2]]$iter,
                      catch_out$stan_args[[3]]$iter,
                      catch_out$stan_args[[4]]$iter)),

    # thin
    expect_true(ident(catch_out$stan_args[[1]]$thin,
                      catch_out$stan_args[[2]]$thin,
                      catch_out$stan_args[[3]]$thin,
                      catch_out$stan_args[[4]]$thin)),

    # warmup
    expect_true(ident(catch_out$stan_args[[1]]$warmup,
                      catch_out$stan_args[[2]]$warmup,
                      catch_out$stan_args[[3]]$warmup,
                      catch_out$stan_args[[4]]$warmup)),

    # init
    expect_true(ident(catch_out$stan_args[[1]]$init,
                      catch_out$stan_args[[2]]$init,
                      catch_out$stan_args[[3]]$init,
                      catch_out$stan_args[[4]]$init)),

    # algorithm
    expect_true(ident(catch_out$stan_args[[1]]$algorithm,
                      catch_out$stan_args[[2]]$algorithm,
                      catch_out$stan_args[[3]]$algorithm,
                      catch_out$stan_args[[4]]$algorithm)),

    # check_unknown_args
    expect_true(ident(catch_out$stan_args[[1]]$check_unknown_args,
                      catch_out$stan_args[[2]]$check_unknown_args,
                      catch_out$stan_args[[3]]$check_unknown_args,
                      catch_out$stan_args[[4]]$check_unknown_args)),

    # sampling
    expect_true(ident(catch_out$stan_args[[1]]$sampling,
                      catch_out$stan_args[[2]]$sampling,
                      catch_out$stan_args[[3]]$sampling,
                      catch_out$stan_args[[4]]$sampling)),

    # seed is integer
    expect_type(catch_out$seed, "integer"),

    # model name is right
    expect_equal(catch_out$model_name, "8schools"),

    # model code is character
    expect_type(catch_out$model_code, "character"),

    # warnings NULL due to mocking
    expect_null(catch_out$warnings)

  )
})



