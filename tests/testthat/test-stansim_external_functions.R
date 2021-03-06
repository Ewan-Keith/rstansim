context("test user exposed functions for correct behaviour")

#-----------------------------------------------------------------
#### general stansim tests with mocked rstan ####
test_that("stansim test; cache FALSE, loo FALSE", {

  with_mock(
    `rstansim:::single_sim` = function(...){
      readRDS("objects/test_stansim_uni_single.rds")
    },

    `rstan::stan_model` = function(...){
      readRDS("objects/test_stanmodel.rds")
    },

    test_stan_args <-
      list(
        file = "data-raw/8schools.stan",
        iter = 500,
        chains = 4
      ),

    test_sim_data <- dir("data-raw/data", full.names = TRUE),

    catch <-
      capture_output(
        stansim_output <- fit_models(
          stan_args = test_stan_args,
          sim_data = test_sim_data,
          cache = FALSE,
          sim_name = "stansim no cache & loo test",
          seed = 500
        )
      ),

    # output should be a list
    expect_type(stansim_output, "list"),

    # list of length 10
    expect_equal(length(stansim_output), 10),

    # has class "stansim_simulation"
    expect_s3_class(stansim_output, "stansim_simulation"),

    # item names should be as expected
    expect_equal(names(stansim_output),
                 c("sim_name", "start_time", "end_time", "model_name",
                   "model_code", "seed", "instances", "data", "raw_call",
                   "refitted")),

    # sim_name should be correct
    expect_equal(stansim_output$sim_name,
                 "stansim no cache & loo test"),

    # start and end time should be of type date
    is_date <- function(mydate, date.format = "%d/%m/%y") {
      tryCatch(!is.na(as.Date(mydate, date.format, tz = "UTC")),
               error = function(err) FALSE)
    },
    expect_true(is_date(stansim_output$start_time)),
    expect_true(is_date(stansim_output$end_time)),

    # model name should be correct
    expect_equal(stansim_output$model_name, "8schools"),

    # model code should be of correct length
    expect_equal(nchar(stansim_output$model_code), 418),

    # sim_seed should be correct
    expect_equal(stansim_output$seed, 500),

    ## extract the instances for testing
    test_instances <- stansim_output$instances,

    # function running tests over each instance list
    instance_check <- function(instance){

      # should be type list
      expect_type(instance, "list")

      # should be length 7
      expect_equal(length(instance), 7)

      # data name should be correct format
      expect_true(grepl("data-raw/data/schoolsdat\\d.rds", instance$data_name))

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

      warn_regex <-
        paste0("There were 2 divergent transitions after warmup.",
               " Increasing adapt_delta above 0.8 may help. ",
               "See\nhttp://mc-stan.org/misc/warnings.html#",
               "divergent-transitions-after-warmup")

      # expect_equal(instance$warnings[[1]][[1]],
      #              warn_regex)
    },

    lapply(test_instances, instance_check)

  )
})


#-----------------------------------------------------------------
#### start to finish cache TRUE test ####
test_that("stansim test; cache TRUE, loo FALSE", {

  dir.create(".cache/")

  file.copy(from = "objects/test_stansim_uni_single.rds",
            to = ".cache/schoolsdat1_cached.rds")

  file.copy(from = "objects/test_stansim_uni_single.rds",
            to = ".cache/schoolsdat2_cached.rds")

  file.copy(from = "objects/test_stansim_uni_single.rds",
            to = ".cache/schoolsdat3_cached.rds")

  file.copy(from = "objects/test_stansim_uni_single.rds",
            to = ".cache/schoolsdat4_cached.rds")

  # unlink(".cache", recursive = TRUE)

#### drop down long tests ####
  with_mock(
    `rstansim:::single_sim` = function(...) {
      readRDS("objects/test_stansim_uni_single.rds")
    },

    `rstan::stan_model` = function(...){
      readRDS("objects/test_stanmodel.rds")
    },

    test_stan_args <-
      list(
        file = "data-raw/8schools.stan",
        iter = 500,
        chains = 4
      ),

    test_sim_data <- dir("data-raw/data", full.names = TRUE),

    catch <-
      capture_output(
        stansim_output <- fit_models(
          stan_args = test_stan_args,
          sim_data = test_sim_data,
          cache = TRUE,
          sim_name = "stansim no loo test",
          seed = 500
        )
      ),

    # output should be a list
    expect_type(stansim_output, "list"),

    # list of length 10
    expect_equal(length(stansim_output), 10),

    # has class "stansim_simulation"
    expect_s3_class(stansim_output, "stansim_simulation"),

    # item names should be as expected
    expect_equal(names(stansim_output),
                 c("sim_name", "start_time", "end_time", "model_name",
                   "model_code", "seed", "instances", "data",
                   "raw_call", "refitted")),

    # sim_name should be correct
    expect_equal(stansim_output$sim_name,
                 "stansim no loo test"),

    # start and end time should be of type date
    is_date <- function(mydate, date.format = "%d/%m/%y") {
      tryCatch(!is.na(as.Date(mydate, date.format, tz = "UTC")),
               error = function(err) FALSE)
    },
    expect_true(is_date(stansim_output$start_time)),
    expect_true(is_date(stansim_output$end_time)),

    # model name should be correct
    expect_equal(stansim_output$model_name, "8schools"),

    # model code should be of correct length
    expect_equal(nchar(stansim_output$model_code), 418),

    # sim_seed should be correct
    expect_equal(stansim_output$seed, 500),

    ## extract the instances for testing
    test_instances <- stansim_output$instances,

    # function running tests over each instance list
    instance_check <- function(instance){

      # should be type list
      expect_type(instance, "list")

      # should be length 7
      expect_equal(length(instance), 7)

      # data name should be correct format
      expect_true(grepl("data-raw/data/schoolsdat\\d.rds", instance$data_name))

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

      warn_regex <-
        paste0("There were 2 divergent transitions after warmup.",
               " Increasing adapt_delta above 0.8 may help. ",
               "See\nhttp://mc-stan.org/misc/warnings.html#",
               "divergent-transitions-after-warmup")

      # expect_equal(instance$warnings[[1]][[1]],
      #              warn_regex)
    },

    lapply(test_instances, instance_check),

    # test that .cache has been deleted
    expect_false(dir.exists(".cache"))

  )

#### end drop down ####
#### second long test drop ####

  # mocking single_sim running and saving output to cache
  with_mock(
    `rstansim:::single_sim` = function(...) {

      dir.create(".cache/")

      file.copy(from = "objects/test_stansim_uni_single.rds",
                to = ".cache/schoolsdat1_cached.rds")

      file.copy(from = "objects/test_stansim_uni_single.rds",
                to = ".cache/schoolsdat2_cached.rds")

      file.copy(from = "objects/test_stansim_uni_single.rds",
                to = ".cache/schoolsdat3_cached.rds")

      file.copy(from = "objects/test_stansim_uni_single.rds",
                to = ".cache/schoolsdat4_cached.rds")

      readRDS("objects/test_stansim_uni_single.rds")
    },

    `rstan::stan_model` = function(...){
      readRDS("objects/test_stanmodel.rds")
    },

    test_stan_args <-
      list(
        file = "data-raw/8schools.stan",
        iter = 500,
        chains = 4
      ),

    test_sim_data <- dir("data-raw/data", full.names = TRUE),

    catch <-
      capture_output(
        stansim_output <- fit_models(
          stan_args = test_stan_args,
          sim_data = test_sim_data,
          cache = TRUE,
          sim_name = "stansim no loo test",
          seed = 500
        )
      ),

    # output should be a list
    expect_type(stansim_output, "list"),

    # list of length 10
    expect_equal(length(stansim_output), 10),

    # has class "stansim_simulation"
    expect_s3_class(stansim_output, "stansim_simulation"),

    # item names should be as expected
    expect_equal(names(stansim_output),
                 c("sim_name", "start_time", "end_time", "model_name",
                   "model_code", "seed", "instances", "data",
                   "raw_call", "refitted")),

    # sim_name should be correct
    expect_equal(stansim_output$sim_name,
                 "stansim no loo test"),

    # start and end time should be of type date
    is_date <- function(mydate, date.format = "%d/%m/%y") {
      tryCatch(!is.na(as.Date(mydate, date.format, tz = "UTC")),
               error = function(err) FALSE)
    },
    expect_true(is_date(stansim_output$start_time)),
    expect_true(is_date(stansim_output$end_time)),

    # model name should be correct
    expect_equal(stansim_output$model_name, "8schools"),

    # model code should be of correct length
    expect_equal(nchar(stansim_output$model_code), 418),

    # sim_seed should be correct
    expect_equal(stansim_output$seed, 500),

    ## extract the instances for testing
    test_instances <- stansim_output$instances,

    # function running tests over each instance list
    instance_check <- function(instance){

      # should be type list
      expect_type(instance, "list")

      # should be length 7
      expect_equal(length(instance), 7)

      # data name should be correct format
      expect_true(grepl("data-raw/data/schoolsdat\\d.rds", instance$data_name))

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

      warn_regex <-
        paste0("There were 2 divergent transitions after warmup.",
               " Increasing adapt_delta above 0.8 may help. ",
               "See\nhttp://mc-stan.org/misc/warnings.html#",
               "divergent-transitions-after-warmup")

      # expect_equal(instance$warnings[[1]][[1]],
      #              warn_regex)
    },

    lapply(test_instances, instance_check),

    # test that .cache has been deleted
    expect_false(dir.exists(".cache"))

  )
#### end scond dropdown ####

#### third long test dropdown ####
  # partial cache test

  dir.create(".cache/")

  file.copy(from = "objects/test_stansim_uni_single.rds",
            to = ".cache/schoolsdat1_cached.rds")

  file.copy(from = "objects/test_stansim_uni_single.rds",
            to = ".cache/schoolsdat2_cached.rds")


  #### drop down long tests ####
  with_mock(
    `rstansim:::single_sim` = function(...) {

      file.copy(from = "objects/test_stansim_uni_single.rds",
                to = ".cache/schoolsdat3_cached.rds")

      file.copy(from = "objects/test_stansim_uni_single.rds",
                to = ".cache/schoolsdat4_cached.rds")

      readRDS("objects/test_stansim_uni_single.rds")
    },

    `rstan::stan_model` = function(...){
      readRDS("objects/test_stanmodel.rds")
    },

    test_stan_args <-
      list(
        file = "data-raw/8schools.stan",
        iter = 500,
        chains = 4
      ),

    test_sim_data <- dir("data-raw/data", full.names = TRUE),

    catch <-
      capture_output(
        stansim_output <- fit_models(
          stan_args = test_stan_args,
          sim_data = test_sim_data,
          cache = TRUE,
          sim_name = "stansim no loo test",
          seed = 500
        )
      ),

    # output should be a list
    expect_type(stansim_output, "list"),

    # list of length 10
    expect_equal(length(stansim_output), 10),

    # has class "stansim_simulation"
    expect_s3_class(stansim_output, "stansim_simulation"),

    # item names should be as expected
    expect_equal(names(stansim_output),
                 c("sim_name", "start_time", "end_time", "model_name",
                   "model_code", "seed", "instances", "data",
                   "raw_call", "refitted")),

    # sim_name should be correct
    expect_equal(stansim_output$sim_name,
                 "stansim no loo test"),

    # start and end time should be of type date
    is_date <- function(mydate, date.format = "%d/%m/%y") {
      tryCatch(!is.na(as.Date(mydate, date.format, tz = "UTC")),
               error = function(err) FALSE)
    },
    expect_true(is_date(stansim_output$start_time)),
    expect_true(is_date(stansim_output$end_time)),

    # model name should be correct
    expect_equal(stansim_output$model_name, "8schools"),

    # model code should be of correct length
    expect_equal(nchar(stansim_output$model_code), 418),

    # sim_seed should be correct
    expect_equal(stansim_output$seed, 500),

    ## extract the instances for testing
    test_instances <- stansim_output$instances,

    # function running tests over each instance list
    instance_check <- function(instance){

      # should be type list
      expect_type(instance, "list")

      # should be length 7
      expect_equal(length(instance), 7)

      # data name should be correct format
      expect_true(grepl("data-raw/data/schoolsdat\\d.rds", instance$data_name))

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

      warn_regex <-
        paste0("There were 2 divergent transitions after warmup.",
               " Increasing adapt_delta above 0.8 may help. ",
               "See\nhttp://mc-stan.org/misc/warnings.html#",
               "divergent-transitions-after-warmup")

      # expect_equal(instance$warnings[[1]][[1]],
      #              warn_regex)
    },

    lapply(test_instances, instance_check),

    # test that .cache has been deleted
    expect_false(dir.exists(".cache"))

  )

})

#-----------------------------------------------------------------
#### brief rename tests ####
test_that("rename should work as expected", {

  ## read in test stansim_simulation obj to rename
  stansim_obj <-
    readRDS("objects/test_stansim.rds")

  # error if name not character
  expect_error(rename(object = stansim_obj, new_name = 555),
               "new_name must be of type character")

  # precheck object name
  expect_match(stansim_obj$sim_name, "Stansim_.*")

  # check name is reset correctly
  renamed_stansim <- rename(stansim_obj, "new name123")
  expect_equal(renamed_stansim$sim_name, "new name123")

})


#-----------------------------------------------------------------
#### general stansim tests with stansim_data input ####
test_that("stansim test with stansim_data object; cache FALSE, loo FALSE", {

    test_stan_args <-
      list(
        file = "data-raw/simtestreg.stan",
        iter = 500,
        chains = 4
      )

    reg_sim <- function(N = 100) {
      list("N" = N, "x" = rep(0, N), "y" = rep(0, N))
    }

    reg_data <- reg_sim(100)

    test_vals <- list("alpha" = 100, "beta" = -5, "sigma" = 20)

    # check that testdir doesn't already exist
    expect_false(dir.exists("testdir"))

    catch <- capture_output(
      ss_data <- simulate_data(
        file = 'data-raw/simtestreg.stan',
        data_name = "saved stansim_data",
        input_data = reg_data,
        nsim = 1,
        path = "testdir",
        param_values = test_vals,
        vars = c("sim_x", "sim_y", "N"),
        use_cores = 1
      )$datasets
    )

    # check that testdir now exist
    expect_true(dir.exists("testdir"))

    catch <-
      capture_output(
        stansim_output <- fit_models(
          stan_args = test_stan_args,
          sim_data = ss_data,
          cache = FALSE,
          parameters = c("alpha", "beta", "sigma"),
          sim_name = "stansim no cache & loo test",
          seed = 500
        )
      )

    # output should be a list
    expect_type(stansim_output, "list")

    # list of length 10
    expect_equal(length(stansim_output), 10)

    # has class "stansim_simulation"
    expect_s3_class(stansim_output, "stansim_simulation")

    # item names should be as expected
    expect_equal(names(stansim_output),
                 c("sim_name", "start_time", "end_time", "model_name",
                   "model_code", "seed", "instances", "data", "raw_call",
                   "refitted"))

    # sim_name should be correct
    expect_equal(stansim_output$sim_name,
                 "stansim no cache & loo test")

    # start and end time should be of type date
    is_date <- function(mydate, date.format = "%d/%m/%y") {
      tryCatch(!is.na(as.Date(mydate, date.format, tz = "UTC")),
               error = function(err) FALSE)
    }
    expect_true(is_date(stansim_output$start_time))
    expect_true(is_date(stansim_output$end_time))

    # model name should be correct
    expect_equal(stansim_output$model_name, "simtestreg")

    # model code should be of correct length
    expect_equal(nchar(stansim_output$model_code), 338)

    # sim_seed should be correct
    expect_equal(stansim_output$seed, 500)

    ## extract the instances for testing
    test_instances <- stansim_output$instances

    # function running tests over each instance list
    instance_check <- function(instance){

      # should be type list
      expect_type(instance, "list")

      # should be length 7
      expect_equal(length(instance), 7)

      # data name should be correct format
      expect_true(grepl("testdir/saved stansim_data_\\d+.rds", instance$data_name))

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

    }

    lapply(test_instances, instance_check)

    # check that testdir still  exist
    expect_true(dir.exists("testdir"))

    # delete testthat
    unlink("testdir", recursive = T)

    # check that testdir now gone
    expect_false(dir.exists("testdir"))
})

#-----------------------------------------------------------------
#### general stansim tests with stansim_data input ####
test_that("stansim test with stansim_data object; cache TRUE, loo FALSE", {

  test_stan_args <-
    list(
      file = "data-raw/simtestreg.stan",
      iter = 500,
      chains = 4
    )

  reg_sim <- function(N = 100) {
    list("N" = N, "x" = rep(0, N), "y" = rep(0, N))
  }

  reg_data <- reg_sim(100)

  test_vals <- list("alpha" = 100, "beta" = -5, "sigma" = 20)

  # check that testdir doesn't already exist
  expect_false(dir.exists("testdir"))

  catch <- capture_output(
    ss_data <- simulate_data(
      file = 'data-raw/simtestreg.stan',
      data_name = "saved stansim_data",
      input_data = reg_data,
      nsim = 1,
      path = "testdir",
      param_values = test_vals,
      vars = c("sim_x", "sim_y", "N"),
      use_cores = 1
    )$datasets
  )

  # check that testdir now exist
  expect_true(dir.exists("testdir"))

  catch <-
    capture_output(
      stansim_output <- fit_models(
        stan_args = test_stan_args,
        sim_data = ss_data,
        cache = TRUE,
        parameters = c("alpha", "beta", "sigma"),
        sim_name = "stansim no cache & loo test",
        seed = 500
      )
    )

  # output should be a list
  expect_type(stansim_output, "list")

  # list of length 10
  expect_equal(length(stansim_output), 10)

  # has class "stansim_simulation"
  expect_s3_class(stansim_output, "stansim_simulation")

  # item names should be as expected
  expect_equal(names(stansim_output),
               c("sim_name", "start_time", "end_time", "model_name",
                 "model_code", "seed", "instances", "data", "raw_call",
                 "refitted"))

  # sim_name should be correct
  expect_equal(stansim_output$sim_name,
               "stansim no cache & loo test")

  # start and end time should be of type date
  is_date <- function(mydate, date.format = "%d/%m/%y") {
    tryCatch(!is.na(as.Date(mydate, date.format, tz = "UTC")),
             error = function(err) FALSE)
  }
  expect_true(is_date(stansim_output$start_time))
  expect_true(is_date(stansim_output$end_time))

  # model name should be correct
  expect_equal(stansim_output$model_name, "simtestreg")

  # model code should be of correct length
  expect_equal(nchar(stansim_output$model_code), 338)

  # sim_seed should be correct
  expect_equal(stansim_output$seed, 500)

  ## extract the instances for testing
  test_instances <- stansim_output$instances

  # function running tests over each instance list
  instance_check <- function(instance){

    # should be type list
    expect_type(instance, "list")

    # should be length 7
    expect_equal(length(instance), 7)

    # data name should be correct format
    expect_true(grepl("testdir/saved stansim_data_\\d+.rds", instance$data_name))

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

  }

  lapply(test_instances, instance_check)

  # check that testdir still  exist
  expect_true(dir.exists("testdir"))

  # delete testthat
  unlink("testdir", recursive = T)

  # check that testdir now gone
  expect_false(dir.exists("testdir"))
})
