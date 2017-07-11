context("stan_sim objects should function correctly")

test_that("stan_sim_uni object constructor returns correct values", {

  # read in prepared stanfit object
  test_stanfit <- readRDS("objects/test_stanfit.rds")

  test_stansim_uni <-
    rstansim:::stansim_uni(
      test_stanfit,
      data_name = "data_name123",
      ran_at = Sys.time(),
      long_data = rstansim:::param_extract(
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

  # check output is a list
  expect_type(test_stansim_uni, "list")

  # check that the correct type is assigned
  expect_s3_class(test_stansim_uni, "stansim_uni")

  # check list item names are correct
  expect_equal(names(test_stansim_uni),
               c("data_name", "ran_at", "elapsed_time", "stan_inits",
                 "stan_args", "seed", "out_data", "model_name",
                 "model_code", "warnings"))

  # check list length is correct
  expect_equal(length(test_stansim_uni), 10)

  # check data name is correct character
  expect_equal(test_stansim_uni$data_name, "data_name123")

  # check that ran at is date format
  is_date <- function(mydate, date.format = "%d/%m/%y") {
    tryCatch(!is.na(as.Date(mydate, date.format, tz = "UTC")),
             error = function(err) FALSE)
  }
  expect_true(is_date(test_stansim_uni$ran_at))

  ## test elapsed time
  # dimensions
  expect_equal(dim(test_stansim_uni$elapsed_time), c(4, 2))

  # is matrix
  expect_true(is.matrix(test_stansim_uni$elapsed_time))

  # colnames correct
  expect_equal(colnames(test_stansim_uni$elapsed_time), c("warmup", "sample"))

  # rownames correct
  expect_equal(rownames(test_stansim_uni$elapsed_time),
               c("chain:1", "chain:2", "chain:3", "chain:4"))

  # stan inits should be list
  expect_type(test_stansim_uni$stan_inits, "list")

  # stan inits should have record for each chain
  expect_equal(length(test_stansim_uni$stan_inits), 4)

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
  expect_true(ident(test_stansim_uni$stan_args[[1]]$iter,
                    test_stansim_uni$stan_args[[2]]$iter,
                    test_stansim_uni$stan_args[[3]]$iter,
                    test_stansim_uni$stan_args[[4]]$iter))

  # thin
  expect_true(ident(test_stansim_uni$stan_args[[1]]$thin,
                    test_stansim_uni$stan_args[[2]]$thin,
                    test_stansim_uni$stan_args[[3]]$thin,
                    test_stansim_uni$stan_args[[4]]$thin))

  # warmup
  expect_true(ident(test_stansim_uni$stan_args[[1]]$warmup,
                    test_stansim_uni$stan_args[[2]]$warmup,
                    test_stansim_uni$stan_args[[3]]$warmup,
                    test_stansim_uni$stan_args[[4]]$warmup))

  # init
  expect_true(ident(test_stansim_uni$stan_args[[1]]$init,
                    test_stansim_uni$stan_args[[2]]$init,
                    test_stansim_uni$stan_args[[3]]$init,
                    test_stansim_uni$stan_args[[4]]$init))

  # algorithm
  expect_true(ident(test_stansim_uni$stan_args[[1]]$algorithm,
                    test_stansim_uni$stan_args[[2]]$algorithm,
                    test_stansim_uni$stan_args[[3]]$algorithm,
                    test_stansim_uni$stan_args[[4]]$algorithm))

  # check_unknown_args
  expect_true(ident(test_stansim_uni$stan_args[[1]]$check_unknown_args,
                    test_stansim_uni$stan_args[[2]]$check_unknown_args,
                    test_stansim_uni$stan_args[[3]]$check_unknown_args,
                    test_stansim_uni$stan_args[[4]]$check_unknown_args))

  # sampling
  expect_true(ident(test_stansim_uni$stan_args[[1]]$sampling,
                    test_stansim_uni$stan_args[[2]]$sampling,
                    test_stansim_uni$stan_args[[3]]$sampling,
                    test_stansim_uni$stan_args[[4]]$sampling))

  # seed is integer
  expect_type(test_stansim_uni$seed, "integer")

  # model name is right
  expect_equal(test_stansim_uni$model_name, "8schools")

  # model code is character
  expect_type(test_stansim_uni$model_code, "character")

  # warnings correct
  expect_equal(test_stansim_uni$warnings, "warning strings")

})

test_that("stansim_single object constructor returns correct values", {

  # read in prepared stansim_uni list
  test_stansim_uni_list <-
    readRDS("objects/test_stansim_uni_list.rds")

  stansim_single_test <-
    rstansim::stansim_single(
      sim_name = "object construct test",
      stansim_uni_list = test_stansim_uni_list,
      start_time = Sys.time(),
      end_time = Sys.time(),
      stansim_seed = 500,
      raw_call = "raw call values"
    )

  # output should be a list
  expect_type(stansim_single_test, "list")

  # list of length 9
  expect_equal(length(stansim_single_test), 9)

  # has class "stansim_single"
  expect_s3_class(stansim_single_test, "stansim_single")

  # item names should be as expected
  expect_equal(names(stansim_single_test),
               c("sim_name", "start_time", "end_time", "model_name",
                 "model_code", "sim_seed", "instances", "data",
                 "raw_call"))

  # sim_name should be correct
  expect_equal(stansim_single_test$sim_name,
               "object construct test")

  # start and end time should be of type date
  is_date <- function(mydate, date.format = "%d/%m/%y") {
    tryCatch(!is.na(as.Date(mydate, date.format, tz = "UTC")),
             error = function(err) FALSE)
  }
  expect_true(is_date(stansim_single_test$start_time))
  expect_true(is_date(stansim_single_test$end_time))

  # model name should be correct
  expect_equal(stansim_single_test$model_name, "8schools")

  # model code should be of correct length
  expect_equal(nchar(stansim_single_test$model_code), 418)

  # sim_seed should be correct
  expect_equal(stansim_single_test$sim_seed, 500)

  # raw_call should be correct
  expect_equal(stansim_single_test$raw_call, "raw call values")

  ## extract the instances for testing
  test_instances <- stansim_single_test$instances

  # function running tests over each instance list
  instance_check <- function(instance){

    # should be type list
    expect_type(instance, "list")

    # should be length 7
    expect_equal(length(instance), 7)

    # data name should be correct format
    expect_true(grepl("data_name\\d", instance$data_name))

    # ran_at should be of type date
    expect_true(is_date(instance$ran_at))

    # elapsed_time should have c(4, 2) dim
    expect_equal(dim(instance$elapsed_time),
                 c(4, 2))

    # elapsed_time should correct colnames
    expect_equal(colnames(instance$elapsed_time),
                 c("warmup", "sample"))

    # stan inits should be list
    expect_type(instance$stan_inits, "list")

    # stan inits should have record for each chain
    expect_equal(length(instance$stan_inits), 4)

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
    expect_true(ident(instance$stan_args[[1]]$iter,
                      instance$stan_args[[2]]$iter,
                      instance$stan_args[[3]]$iter,
                      instance$stan_args[[4]]$iter))

    # thin
    expect_true(ident(instance$stan_args[[1]]$thin,
                      instance$stan_args[[2]]$thin,
                      instance$stan_args[[3]]$thin,
                      instance$stan_args[[4]]$thin))

    # warmup
    expect_true(ident(instance$stan_args[[1]]$warmup,
                      instance$stan_args[[2]]$warmup,
                      instance$stan_args[[3]]$warmup,
                      instance$stan_args[[4]]$warmup))

    # init
    expect_true(ident(instance$stan_args[[1]]$init,
                      instance$stan_args[[2]]$init,
                      instance$stan_args[[3]]$init,
                      instance$stan_args[[4]]$init))

    # algorithm
    expect_true(ident(instance$stan_args[[1]]$algorithm,
                      instance$stan_args[[2]]$algorithm,
                      instance$stan_args[[3]]$algorithm,
                      instance$stan_args[[4]]$algorithm))

    # check_unknown_args
    expect_true(ident(instance$stan_args[[1]]$check_unknown_args,
                      instance$stan_args[[2]]$check_unknown_args,
                      instance$stan_args[[3]]$check_unknown_args,
                      instance$stan_args[[4]]$check_unknown_args))

    # sampling
    expect_true(ident(instance$stan_args[[1]]$sampling,
                      instance$stan_args[[2]]$sampling,
                      instance$stan_args[[3]]$sampling,
                      instance$stan_args[[4]]$sampling))

    # seed should be int
    expect_type(instance$seed, "integer")

    # warning strings should be correct
    expect_true(grepl("warning strings\\d", instance$warnings))
  }

  lapply(test_instances, instance_check)

})

