context("test single_sim command and outputs")

# rstan needs loaded for loo
suppressPackageStartupMessages(library(rstan))

# read in prepared stanfit object
test_stanfit <- readRDS("objects/test_stanfit.rds")

#-----------------------------------------------------------------
#### single_sim testing ####
test_that("single_sim should return correct object (mocked stan fit)", {

  with_mock(
    `rstan::sampling` = function(...) {
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
      stan_warnings = "suppress",
      cache = F,
      stansim_data_used = F,
      data_name = NULL
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
      tryCatch(!is.na(as.Date(mydate, date.format, tz = "UTC")),
               error = function(err) FALSE)
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
    ident <- function(...) {
      args <- c(...)
      if (length(args) > 2L) {
        #  recursively call ident()
        out <- c(identical(args[1], args[2]), ident(args[-1]))
      } else{
        out <- identical(args[1], args[2])
      }
      return(all(out))
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
    expect_null(catch_out$warnings),

    # just test that alternative warning values do run correctly
    print_out <- rstansim:::single_sim(
      datafile = dir("data-raw/data",
                     full.names = TRUE)[1],
      stan_args = test_stan_args,
      calc_loo = F,
      parameters = "all",
      probs = c(.025, .25, .5, .75, .975),
      estimates = c("mean", "se_mean",
                    "sd", "n_eff", "Rhat"),
      stan_warnings = "print",
      cache = F,
      stansim_data_used = F,
      data_name = NULL
    ),

    # should be list
    expect_type(print_out, "list"),

    # should be class stansim_uni
    expect_s3_class(print_out, "stansim_uni"),

    # should have ten items
    expect_equal(length(print_out), 10),

    # check list item names are correct
    expect_equal(names(print_out),
                 c("data_name", "ran_at", "elapsed_time", "stan_inits",
                   "stan_args", "seed", "out_data", "model_name",
                   "model_code", "warnings")),

    # just test that alternative warning values do run correctly
    suppress_out <- rstansim:::single_sim(
      datafile = dir("data-raw/data",
                     full.names = TRUE)[1],
      stan_args = test_stan_args,
      calc_loo = F,
      parameters = "all",
      probs = c(.025, .25, .5, .75, .975),
      estimates = c("mean", "se_mean",
                    "sd", "n_eff", "Rhat"),
      stan_warnings = "suppress",
      cache = F,
      stansim_data_used = F,
      data_name = NULL
    ),

    # should be list
    expect_type(suppress_out, "list"),

    # should be class stansim_uni
    expect_s3_class(suppress_out, "stansim_uni"),

    # should have ten items
    expect_equal(length(suppress_out), 10),

    # check list item names are correct
    expect_equal(names(suppress_out),
                 c("data_name", "ran_at", "elapsed_time", "stan_inits",
                   "stan_args", "seed", "out_data", "model_name",
                   "model_code", "warnings"))

  )
})

#-----------------------------------------------------------------
#### single_sim catch warning testing ####
test_that("single_sim warnings behave as expectated", {

  with_mock(
    `rstan::sampling` = function(...) {
      warning("test warning 1")
      warning("test warning 2")
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
      cache = F,
      stansim_data_used = F,
      data_name = NULL
    ),

    # warnings caught should be list
    expect_type(catch_out$warnings, "list"),

    # warnings should be length 2
    expect_length(catch_out$warnings, 2),

    # both list items should have correct class
    for (i in 1:2) {
      expect_s3_class(catch_out$warnings[[i]],
                      c("simpleWarning", "warning", "condition"))
    },

    # both messages should be correct
    for (i in 1:2){
      expect_equal(catch_out$warnings[[i]]$message,
                   paste("test warning", i))
    },

    # both calls should be of type language
    for (i in 1:2){
      expect_type(catch_out$warnings[[i]]$call,
                  "language")
    }
  )
})

#-----------------------------------------------------------------
#### single_sim cache testing ####
test_that("written cache folder and files are correct", {

  with_mock(
    `rstan::sampling` = function(...) {
      test_stanfit
    },

    test_stan_args <-
      list(
        file = "data-raw/8schools.stan",
        iter = 500,
        chains = 4
      ),

    # pre-create the cache folder
    dir.create(".cache"),

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
      cache = T,
      stansim_data_used = F,
      data_name = NULL
    ),

    # cache file should have been written
    expect_true(file.exists(".cache/schoolsdat1_cached.rds")),

    # read in output file
    cached_output <- readRDS(".cache/schoolsdat1_cached.rds"),

    #-------------------------------------------------------------
    ## this section just repeates the above tests of the single_sim output but
    ## on the cached output
    # should be list
    expect_type(cached_output, "list"),

    # should be class stansim_uni
    expect_s3_class(cached_output, "stansim_uni"),

    # should have ten items
    expect_equal(length(cached_output), 10),

    # check list item names are correct
    expect_equal(names(cached_output),
                 c("data_name", "ran_at", "elapsed_time", "stan_inits",
                   "stan_args", "seed", "out_data", "model_name",
                   "model_code", "warnings")),

    # data name should be character
    expect_type(cached_output$data_name, "character"),

    is_date <- function(mydate, date.format = "%d/%m/%y") {
      tryCatch(!is.na(as.Date(mydate, date.format, tz = "UTC")),
               error = function(err) FALSE)
    },

    # ran at should be date
    expect_true(is_date(cached_output$ran_at)),

    # elapsed_time should have c(chains, 2) dim
    expect_equal(dim(cached_output$elapsed_time),
                 c(test_stan_args$chains, 2)),

    # elapsed_time should correct colnames
    expect_equal(colnames(cached_output$elapsed_time),
                 c("warmup", "sample")),

    # stan inits should be list
    expect_type(cached_output$stan_inits, "list"),

    # stan inits should have record for each chain
    expect_equal(length(cached_output$stan_inits), 4),

    # stan args that should be same across chains are
    ident <- function(...) {
      args <- c(...)
      if (length(args) > 2L) {
        #  recursively call ident()
        out <- c(identical(args[1], args[2]), ident(args[-1]))
      } else{
        out <- identical(args[1], args[2])
      }
      return(all(out))
    },

    # iter same
    expect_true(ident(cached_output$stan_args[[1]]$iter,
                      cached_output$stan_args[[2]]$iter,
                      cached_output$stan_args[[3]]$iter,
                      cached_output$stan_args[[4]]$iter)),

    # thin
    expect_true(ident(cached_output$stan_args[[1]]$thin,
                      cached_output$stan_args[[2]]$thin,
                      cached_output$stan_args[[3]]$thin,
                      cached_output$stan_args[[4]]$thin)),

    # warmup
    expect_true(ident(cached_output$stan_args[[1]]$warmup,
                      cached_output$stan_args[[2]]$warmup,
                      cached_output$stan_args[[3]]$warmup,
                      cached_output$stan_args[[4]]$warmup)),

    # init
    expect_true(ident(cached_output$stan_args[[1]]$init,
                      cached_output$stan_args[[2]]$init,
                      cached_output$stan_args[[3]]$init,
                      cached_output$stan_args[[4]]$init)),

    # algorithm
    expect_true(ident(cached_output$stan_args[[1]]$algorithm,
                      cached_output$stan_args[[2]]$algorithm,
                      cached_output$stan_args[[3]]$algorithm,
                      cached_output$stan_args[[4]]$algorithm)),

    # check_unknown_args
    expect_true(ident(cached_output$stan_args[[1]]$check_unknown_args,
                      cached_output$stan_args[[2]]$check_unknown_args,
                      cached_output$stan_args[[3]]$check_unknown_args,
                      cached_output$stan_args[[4]]$check_unknown_args)),

    # sampling
    expect_true(ident(cached_output$stan_args[[1]]$sampling,
                      cached_output$stan_args[[2]]$sampling,
                      cached_output$stan_args[[3]]$sampling,
                      cached_output$stan_args[[4]]$sampling)),

    # seed is integer
    expect_type(cached_output$seed, "integer"),

    # model name is right
    expect_equal(cached_output$model_name, "8schools"),

    # model code is character
    expect_type(cached_output$model_code, "character"),

    # warnings NULL due to mocking
    expect_null(cached_output$warnings),

    #---------------------------------------------
    ## tidy up cache folder and test that it's gone
    unlink(".cache", recursive = TRUE),

    expect_false(dir.exists(".cache"))

  )
})


#-----------------------------------------------------------------
#### single_sim testing ####
test_that("single_sim testing with stansim_data input", {

  catch <-
    capture_output(# read in prepared stanfit object
      test_stanfit <- stan_model(file = 'data-raw/simtestreg.stan'))

  ss_data <- readRDS("objects/stansim_data_for_method_tests.rds")$data[[1]]

    test_stan_args <-
      list(
        object = test_stanfit,
        iter = 500,
        chains = 4
      )

    catch <-
      capture_output(
        catch_out <- rstansim:::single_sim(
          datafile = ss_data,
          stan_args = test_stan_args,
          calc_loo = F,
          parameters = "all",
          probs = c(.025, .25, .5, .75, .975),
          estimates = c("mean", "se_mean",
                        "sd", "n_eff", "Rhat"),
          stan_warnings = "suppress",
          cache = F,
          stansim_data_used = T,
          data_name = "test name"
        )
      )

    # should be list
    expect_type(catch_out, "list")

    # should be class stansim_uni
    expect_s3_class(catch_out, "stansim_uni")

    # should have ten items
    expect_equal(length(catch_out), 10)

    # check list item names are correct
    expect_equal(names(catch_out),
                 c("data_name", "ran_at", "elapsed_time", "stan_inits",
                   "stan_args", "seed", "out_data", "model_name",
                   "model_code", "warnings"))

    # data name should be character
    expect_type(catch_out$data_name, "character")

    is_date <- function(mydate, date.format = "%d/%m/%y") {
      tryCatch(!is.na(as.Date(mydate, date.format, tz = "UTC")),
               error = function(err) FALSE)
    }

    # ran at should be date
    expect_true(is_date(catch_out$ran_at))

    # elapsed_time should have c(chains, 2) dim
    expect_equal(dim(catch_out$elapsed_time),
                 c(test_stan_args$chains, 2))

    # elapsed_time should correct colnames
    expect_equal(colnames(catch_out$elapsed_time),
                 c("warmup", "sample"))

    # stan inits should be list
    expect_type(catch_out$stan_inits, "list")

    # stan inits should have record for each chain
    expect_equal(length(catch_out$stan_inits), 4)

    # stan args that should be same across chains are
    ident <- function(...) {
      args <- c(...)
      if (length(args) > 2L) {
        #  recursively call ident()
        out <- c(identical(args[1], args[2]), ident(args[-1]))
      } else{
        out <- identical(args[1], args[2])
      }
      return(all(out))
    }

    # iter same
    expect_true(ident(catch_out$stan_args[[1]]$iter,
                      catch_out$stan_args[[2]]$iter,
                      catch_out$stan_args[[3]]$iter,
                      catch_out$stan_args[[4]]$iter))

    # thin
    expect_true(ident(catch_out$stan_args[[1]]$thin,
                      catch_out$stan_args[[2]]$thin,
                      catch_out$stan_args[[3]]$thin,
                      catch_out$stan_args[[4]]$thin))

    # warmup
    expect_true(ident(catch_out$stan_args[[1]]$warmup,
                      catch_out$stan_args[[2]]$warmup,
                      catch_out$stan_args[[3]]$warmup,
                      catch_out$stan_args[[4]]$warmup))

    # init
    expect_true(ident(catch_out$stan_args[[1]]$init,
                      catch_out$stan_args[[2]]$init,
                      catch_out$stan_args[[3]]$init,
                      catch_out$stan_args[[4]]$init))

    # algorithm
    expect_true(ident(catch_out$stan_args[[1]]$algorithm,
                      catch_out$stan_args[[2]]$algorithm,
                      catch_out$stan_args[[3]]$algorithm,
                      catch_out$stan_args[[4]]$algorithm))

    # check_unknown_args
    expect_true(ident(catch_out$stan_args[[1]]$check_unknown_args,
                      catch_out$stan_args[[2]]$check_unknown_args,
                      catch_out$stan_args[[3]]$check_unknown_args,
                      catch_out$stan_args[[4]]$check_unknown_args))

    # sampling
    expect_true(ident(catch_out$stan_args[[1]]$sampling,
                      catch_out$stan_args[[2]]$sampling,
                      catch_out$stan_args[[3]]$sampling,
                      catch_out$stan_args[[4]]$sampling))

    # seed is integer
    expect_type(catch_out$seed, "integer")

    # model name is right
    expect_equal(catch_out$model_name, "simtestreg")

    # model code is character
    expect_type(catch_out$model_code, "character")


})
