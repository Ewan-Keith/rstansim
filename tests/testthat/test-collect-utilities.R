context("internal collect_* utilities return expected results")

#-----------------------------------------------------------------
#### collect_simulations ####

## read in test stansim_simulation obj to use in tests
sim1 <-
  readRDS("objects/test_stansim.rds")

sim2 <-
  readRDS("objects/refitted_for_collection_tests.rds")

collection <- collect("collection 1", sim1, sim2)


test_that("collect_simulations output structure is right", {

  # class is correct
  expect_s3_class(collection, "stansim_collection")

  # check type
  expect_type(collection, "list")

  # check length
  expect_equal(length(collection), 4)

  # check names
  expect_equal(names(collection),
               c("collection_name", "data", "refitted", "simulations"))

})

test_that("collection_name, data, and refitted output is correct", {

  # collection is character
  expect_type(collection$collection_name, "character")

  # collection name is correct
  expect_equal(collection$collection_name, "collection 1")

  # data is a dataframe
  expect_true(is.data.frame(collection$data))

  # data dims is correct
  expect_equal(dim(collection$data), c(1520, 5))

  # check names are correct
  expect_equal(names(collection$data), c("sim_name", "dataset",
                                         "parameter", "estimate",
                                         "value"))

  # data is a dataframe
  expect_true(is.data.frame(collection$refitted))

  # data dims is correct
  expect_equal(dim(collection$refitted), c(2, 2))

  # check names are correct
  expect_equal(names(collection$refitted), c("sim_name", "dataset"))

  # check refitted datafiles are correct
  expect_equal(collection$refitted$dataset,
               c("data-raw/data/schoolsdat1.rds",
                 "data-raw/data/schoolsdat3.rds"))

})

test_that("test that the simualtions are correctly structured", {

  coll_sims <- collection$simulations

  # is list
  expect_type(coll_sims, "list")

  # length of two
  expect_equal(length(coll_sims), 2)

  # names should be the simulation names
  expect_named(coll_sims,
               c("Stansim_2017-08-07 21:46:47","refitted test sim"))

  # break down into second simualtion
  sim_breakdown <- coll_sims$`refitted test sim`

  ### from here on tests have been repurposed from the external functions tests
  # output should be a list
  expect_type(sim_breakdown, "list")

  # list of length 10
  expect_equal(length(sim_breakdown), 7)

  # has no class
  expect_null(attributes(sim_breakdown$class))

  # item names should be as expected
  expect_equal(names(sim_breakdown),
               c("start_time", "end_time", "model_name",
                 "model_code", "sim_seed", "instances",
                 "raw_call"))

  # start and end time should be of type date
  is_date <- function(mydate, date.format = "%d/%m/%y") {
    tryCatch(!is.na(as.Date(mydate, date.format, tz = "UTC")),
             error = function(err) FALSE)
  }
  expect_true(is_date(sim_breakdown$start_time))
  expect_true(is_date(sim_breakdown$end_time))

  # model name should be correct
  expect_equal(sim_breakdown$model_name, "8schools")

  # model code should be of correct length
  expect_equal(nchar(sim_breakdown$model_code), 418)

  # sim_seed should be correct
  expect_equal(sim_breakdown$sim_seed, 12345)

  ## extract the instances for testing
  test_instances <- sim_breakdown$instances

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

    # warning strings should be null


    expect_null(instance$warnings[[1]])
  }

  lapply(test_instances, instance_check)
})

#-----------------------------------------------------------------
#### collect_collections ####

## read in test stansim_collection obj to use in tests
coll1 <-
  readRDS("objects/collection_for_method_tests.rds")

coll2 <-
  rename(readRDS("objects/collection_for_method_tests.rds"),
         "test collection 2")

collection <- collect("collection 2", coll1, coll2)

test_that("collect_collections output structure is right", {

  # class is correct
  expect_s3_class(collection, "stansim_collection")

  # check type
  expect_type(collection, "list")

  # check length
  expect_equal(length(collection), 4)

  # check names
  expect_equal(names(collection),
               c("collection_name", "data", "refitted", "simulations"))

})

test_that("collection_name, data, and refitted output is correct", {

  # collection is character
  expect_type(collection$collection_name, "character")

  # collection name is correct
  expect_equal(collection$collection_name, "collection 2")

  # data is a dataframe
  expect_true(is.data.frame(collection$data))

  # data dims is correct
  expect_equal(dim(collection$data), c(3040, 5))

  # check names are correct
  expect_equal(names(collection$data), c("sim_name", "dataset",
                                         "parameter", "estimate",
                                         "value"))

  # data is a dataframe
  expect_true(is.data.frame(collection$refitted))

  # data dims is correct
  expect_equal(dim(collection$refitted), c(4, 2))

  # check names are correct
  expect_equal(names(collection$refitted), c("sim_name", "dataset"))

  # check refitted datafiles are correct
  expect_equal(collection$refitted$dataset,
               rep(c("data-raw/data/schoolsdat1.rds",
                 "data-raw/data/schoolsdat3.rds"), 2))

})

test_that("test that the simualtions are correctly structured", {

  coll_sims <- collection$simulations

  # is list
  expect_type(coll_sims, "list")

  # length of two
  expect_equal(length(coll_sims), 4)

  # names should be the simulation names
  expect_named(coll_sims,
               c("Stansim_2017-08-07 21:46:47", "refitted test sim",
                 "Stansim_2017-08-07 21:46:47", "refitted test sim"))

  # break down into second simualtion
  sim_breakdown <- coll_sims$`refitted test sim`

  ### from here on tests have been repurposed from the external functions tests
  # output should be a list
  expect_type(sim_breakdown, "list")

  # list of length 10
  expect_equal(length(sim_breakdown), 7)

  # has no class
  expect_null(attributes(sim_breakdown$class))

  # item names should be as expected
  expect_equal(names(sim_breakdown),
               c("start_time", "end_time", "model_name",
                 "model_code", "sim_seed", "instances",
                 "raw_call"))

  # start and end time should be of type date
  is_date <- function(mydate, date.format = "%d/%m/%y") {
    tryCatch(!is.na(as.Date(mydate, date.format, tz = "UTC")),
             error = function(err) FALSE)
  }
  expect_true(is_date(sim_breakdown$start_time))
  expect_true(is_date(sim_breakdown$end_time))

  # model name should be correct
  expect_equal(sim_breakdown$model_name, "8schools")

  # model code should be of correct length
  expect_equal(nchar(sim_breakdown$model_code), 418)

  # sim_seed should be correct
  expect_equal(sim_breakdown$sim_seed, 12345)

  ## extract the instances for testing
  test_instances <- sim_breakdown$instances

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

    # warning strings should be null


    expect_null(instance$warnings[[1]])
  }

  lapply(test_instances, instance_check)



})

#-----------------------------------------------------------------
#### collect_mixed ####

## read in test stansim_collection obj to use in tests
sim1 <-
  rename(readRDS("objects/test_stansim.rds"), "new sim name")

coll2 <-
  readRDS("objects/collection_for_method_tests.rds")

collection <- collect("collection 3", sim1, coll2)

test_that("collect_collections output structure is right", {

  # class is correct
  expect_s3_class(collection, "stansim_collection")

  # check type
  expect_type(collection, "list")

  # check length
  expect_equal(length(collection), 4)

  # check names
  expect_equal(names(collection),
               c("collection_name", "data", "refitted", "simulations"))

})

test_that("collection_name, data, and refitted output is correct", {

  # collection is character
  expect_type(collection$collection_name, "character")

  # collection name is correct
  expect_equal(collection$collection_name, "collection 3")

  # data is a dataframe
  expect_true(is.data.frame(collection$data))

  # data dims is correct
  expect_equal(dim(collection$data), c(2280, 5))

  # check names are correct
  expect_equal(names(collection$data), c("sim_name", "dataset",
                                         "parameter", "estimate",
                                         "value"))

  # data is a dataframe
  expect_true(is.data.frame(collection$refitted))

  # data dims is correct
  expect_equal(dim(collection$refitted), c(2, 2))

  # check names are correct
  expect_equal(names(collection$refitted), c("sim_name", "dataset"))

  # check refitted datafiles are correct
  expect_equal(collection$refitted$dataset,
               c("data-raw/data/schoolsdat1.rds",
                     "data-raw/data/schoolsdat3.rds"))

})

test_that("test that the simualtions are correctly structured", {

  coll_sims <- collection$simulations

  # is list
  expect_type(coll_sims, "list")

  # length of two
  expect_equal(length(coll_sims), 3)

  # names should be the simulation names
  expect_named(coll_sims,
               c("new sim name", "Stansim_2017-08-07 21:46:47", "refitted test sim"))

  # break down into first simualtion
  sim_breakdown <- coll_sims$`new sim name`

  ### from here on tests have been repurposed from the external functions tests
  # output should be a list
  expect_type(sim_breakdown, "list")

  # list of length 7
  expect_equal(length(sim_breakdown), 7)

  # has no class
  expect_null(attributes(sim_breakdown$class))

  # item names should be as expected
  expect_equal(names(sim_breakdown),
               c("start_time", "end_time", "model_name",
                 "model_code", "sim_seed", "instances",
                 "raw_call"))

  # start and end time should be of type date
  is_date <- function(mydate, date.format = "%d/%m/%y") {
    tryCatch(!is.na(as.Date(mydate, date.format, tz = "UTC")),
             error = function(err) FALSE)
  }
  expect_true(is_date(sim_breakdown$start_time))
  expect_true(is_date(sim_breakdown$end_time))

  # model name should be correct
  expect_equal(sim_breakdown$model_name, "8schools")

  # model code should be of correct length
  expect_equal(nchar(sim_breakdown$model_code), 418)

  # sim_seed should be correct
  expect_equal(sim_breakdown$sim_seed, 12345)

  ## extract the instances for testing
  test_instances <- sim_breakdown$instances

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

  }

  lapply(test_instances, instance_check)


})
