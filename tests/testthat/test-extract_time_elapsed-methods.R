context("testing all extract_time_elapsed methods")

test_that(
  paste(
    "extract_time_elapsed.stansim_simulation function should fail",
    "as expected"
  ),
  {
    ## read in test stansim obj to extract from
    extract_test_data <-
      readRDS("objects/test_stansim.rds")

    ## input errors should be as expected
    expect_error(
      extract_time_elapsed(extract_test_data, datasets = 5),
      "datasets argument must be of type character"
    )

    expect_error(
      extract_time_elapsed(extract_test_data, chains = "test"),
      "chains argument must be \"all\" or of type numeric"
    )

    expect_error(
      extract_time_elapsed(extract_test_data, stages = 5),
      "stages argument must be of type character"
    )

    expect_error(
      extract_time_elapsed(extract_test_data, stages = "test"),
      "stages must be all, warmup, sample or total"
    )

    expect_error(
      extract_time_elapsed(extract_test_data, stages = c("test", "test2")),
      "stages must be all, warmup, sample or total"
    )

    expect_error(
      extract_time_elapsed(extract_test_data, elapsed = 5),
      "elapsed argument must be NULL or a function"
    )

    expect_error(
      extract_time_elapsed(extract_test_data, elapsed = "test"),
      "elapsed argument must be NULL or a function"
    )

    # if all provided to arguments must be only length 1
    expect_error(
      extract_time_elapsed(extract_test_data, datasets = c("all", "test")),
      paste(
        "if datasets argument contains \"any\",",
        "length\\(datasets\\) must be 1"
      )
    )

    expect_error(
      extract_time_elapsed(extract_test_data, chains = c("all", "test")),
      paste(
        "if chains argument contains \"any\",",
        "length\\(chains\\) must be 1"
      )
    )

    expect_error(
      extract_time_elapsed(extract_test_data, stages = c("all", "warmup")),
      paste(
        "if stages argument contains \"any\",",
        "length\\(stages\\) must be 1"
      )
    )
  }
)

test_that("extract_time_elapsed.stansim_simulation returns correct default values",
          {
            ## read in test stansim obj to extract from
            extract_test_data <-
              readRDS("objects/test_stansim.rds")

            defaults <- extract_time_elapsed(extract_test_data)

            ##  defaults should return correct object
            # should be a dataframe
            expect_true(is.data.frame(defaults))

            # should have correct dimensions
            expect_equal(dim(defaults), c(48, 4))

            # should have correct colnames
            expect_named(defaults, c("dataset", "chain", "stage", "elapsed"))

            # all columns should be correct type
            expect_equal(
              lapply(defaults, typeof),
              list(
                "dataset" = "character",
                "chain" = "integer",
                "stage" = "character",
                "elapsed" = "double"
              )
            )
          })

test_that(
  paste(
    "extract_time_elapsed.stansim_simulation returns correct values",
    "filtering on datasets"
  ),
  {
    ## read in test stansim obj to extract from
    extract_test_data <-
      readRDS("objects/test_stansim.rds")

    datasets_filter <-
      extract_time_elapsed(
        extract_test_data,
        datasets = c(
          "data-raw/data/schoolsdat1.rds",
          "data-raw/data/schoolsdat3.rds"
        )
      )

    ##  dataset filter should return correct object
    # should be a dataframe
    expect_true(is.data.frame(datasets_filter))

    # should have correct dimensions
    expect_equal(dim(datasets_filter), c(24, 4))

    # should have correct colnames
    expect_named(datasets_filter, c("dataset", "chain", "stage", "elapsed"))

    # all columns should be correct type
    expect_equal(
      lapply(datasets_filter, typeof),
      list(
        "dataset" = "character",
        "chain" = "integer",
        "stage" = "character",
        "elapsed" = "double"
      )
    )

    # only specified datasets should be returned
    expect_equal(sum(!(
      datasets_filter$dataset %in% c(
        "data-raw/data/schoolsdat1.rds",
        "data-raw/data/schoolsdat3.rds"
      )
    )), 0)

  }
)

test_that(
  paste(
    "extract_time_elapsed.stansim_simulation returns correct values",
    "filtering on chains"
  ),
  {
    ## read in test stansim obj to extract from
    extract_test_data <-
      readRDS("objects/test_stansim.rds")

    chains_filter <-
      extract_time_elapsed(extract_test_data,
                           chains = c(1, 3))

    ##  dataset filter should return correct object
    # should be a dataframe
    expect_true(is.data.frame(chains_filter))

    # should have correct dimensions
    expect_equal(dim(chains_filter), c(24, 4))

    # should have correct colnames
    expect_named(chains_filter, c("dataset", "chain", "stage", "elapsed"))

    # all columns should be correct type
    expect_equal(
      lapply(chains_filter, typeof),
      list(
        "dataset" = "character",
        "chain" = "integer",
        "stage" = "character",
        "elapsed" = "double"
      )
    )

    # only specified chains should be returned
    expect_equal(sum(!(chains_filter$chain %in% c(1, 3))), 0)

  }
)

test_that(
  paste(
    "extract_time_elapsed.stansim_simulation returns correct values",
    "filtering on stage"
  ),
  {
    ## read in test stansim obj to extract from
    extract_test_data <-
      readRDS("objects/test_stansim.rds")

    stages_filter <-
      extract_time_elapsed(extract_test_data, stages = "total")

    ##  dataset filter should return correct object
    # should be a dataframe
    expect_true(is.data.frame(stages_filter))

    # should have correct dimensions
    expect_equal(dim(stages_filter), c(16, 4))

    # should have correct colnames
    expect_named(stages_filter, c("dataset", "chain", "stage", "elapsed"))

    # all columns should be correct type
    expect_equal(
      lapply(stages_filter, typeof),
      list(
        "dataset" = "character",
        "chain" = "integer",
        "stage" = "character",
        "elapsed" = "double"
      )
    )

    # only specified stages should be returned
    expect_equal(sum(!(stages_filter$stage == "total")), 0)

  }
)

test_that(
  paste(
    "extract_time_elapsed.stansim_simulation returns correct values",
    "filtering on elapsed"
  ),
  {
    ## read in test stansim obj to extract from
    extract_test_data <-
      readRDS("objects/test_stansim.rds")

    elapsed_filter <-
      extract_time_elapsed(
        extract_test_data,
        elapsed = function(x)
          x > .02
      )

    ##  dataset filter should return correct object
    # should be a dataframe
    expect_true(is.data.frame(elapsed_filter))

    # should have correct dimensions
    expect_equal(dim(elapsed_filter), c(34, 4))

    # should have correct colnames
    expect_named(elapsed_filter, c("dataset", "chain", "stage", "elapsed"))

    # all columns should be correct type
    expect_equal(
      lapply(elapsed_filter, typeof),
      list(
        "dataset" = "character",
        "chain" = "integer",
        "stage" = "character",
        "elapsed" = "double"
      )
    )

    # only specified stages should be returned
    expect_equal(sum(!(elapsed_filter$elapsed > .02)), 0)

  }
)


test_that(
  paste(
    "extract_time_elapsed.stansim_collection function should fail",
    "as expected"
  ),
  {
    ## read in test stansim obj to extract from
    extract_test_data <-
      readRDS("objects/collection_for_method_tests.rds")

    ## input errors should be as expected
    expect_error(
      extract_time_elapsed(extract_test_data, sim_names = 5),
      "sim_names argument must be of type character"
    )

    expect_error(
      extract_time_elapsed(extract_test_data, datasets = 5),
      "datasets argument must be of type character"
    )

    expect_error(
      extract_time_elapsed(extract_test_data, chains = "test"),
      "chains argument must be \"all\" or of type numeric"
    )

    expect_error(
      extract_time_elapsed(extract_test_data, stages = 5),
      "stages argument must be of type character"
    )

    expect_error(
      extract_time_elapsed(extract_test_data, stages = "test"),
      "stages must be all, warmup, sample or total"
    )

    expect_error(
      extract_time_elapsed(extract_test_data, stages = c("test", "test2")),
      "stages must be all, warmup, sample or total"
    )

    expect_error(
      extract_time_elapsed(extract_test_data, elapsed = 5),
      "elapsed argument must be NULL or a function"
    )

    expect_error(
      extract_time_elapsed(extract_test_data, elapsed = "test"),
      "elapsed argument must be NULL or a function"
    )

    # if all provided to arguments must be only length 1
    expect_error(
      extract_time_elapsed(extract_test_data, sim_names = c("all", "test")),
      paste(
        "if sim_names argument contains \"any\",",
        "length\\(sim_names\\) must be 1"
      )
    )

    # if all provided to arguments must be only length 1
    expect_error(
      extract_time_elapsed(extract_test_data, datasets = c("all", "test")),
      paste(
        "if datasets argument contains \"any\",",
        "length\\(datasets\\) must be 1"
      )
    )

    expect_error(
      extract_time_elapsed(extract_test_data, chains = c("all", "test")),
      paste(
        "if chains argument contains \"any\",",
        "length\\(chains\\) must be 1"
      )
    )

    expect_error(
      extract_time_elapsed(extract_test_data, stages = c("all", "warmup")),
      paste(
        "if stages argument contains \"any\",",
        "length\\(stages\\) must be 1"
      )
    )
  }
)

test_that("extract_time_elapsed.stansim_collection returns correct default values",
          {
            ## read in test stansim obj to extract from
            extract_test_data <-
              readRDS("objects/collection_for_method_tests.rds")

            defaults <- extract_time_elapsed(extract_test_data)

            ##  defaults should return correct object
            # should be a dataframe
            expect_true(is.data.frame(defaults))

            # should have correct dimensions
            expect_equal(dim(defaults), c(96, 5))

            # should have correct colnames
            expect_named(defaults,
                         c("sim_name", "dataset", "chain", "stage", "elapsed"))

            # all columns should be correct type
            expect_equal(
              lapply(defaults, typeof),
              list(
                "sim_name" = "character",
                "dataset" = "character",
                "chain" = "integer",
                "stage" = "character",
                "elapsed" = "double"
              )
            )
          })

test_that(
  paste(
    "extract_time_elapsed.stansim_collection returns correct values",
    "filtering on sim_names"
  ),
  {
    ## read in test stansim obj to extract from
    extract_test_data <-
      readRDS("objects/collection_for_method_tests.rds")

    sim_names_filter <-
      extract_time_elapsed(extract_test_data,
                           sim_names = "refitted test sim")

    ##  dataset filter should return correct object
    # should be a dataframe
    expect_true(is.data.frame(sim_names_filter))

    # should have correct dimensions
    expect_equal(dim(sim_names_filter), c(48, 5))

    # should have correct colnames
    expect_named(sim_names_filter,
                 c("sim_name" , "dataset", "chain", "stage", "elapsed"))

    # all columns should be correct type
    expect_equal(
      lapply(sim_names_filter, typeof),
      list(
        "sim_name" = "character",
        "dataset" = "character",
        "chain" = "integer",
        "stage" = "character",
        "elapsed" = "double"
      )
    )

    # only specified sim_name should be returned
    expect_equal(sum(!(
      sim_names_filter$sim_name == "refitted test sim"
    )), 0)

  }
)

test_that(
  paste(
    "extract_time_elapsed.stansim_collection returns correct values",
    "filtering on datasets"
  ),
  {
    ## read in test stansim obj to extract from
    extract_test_data <-
      readRDS("objects/collection_for_method_tests.rds")

    datasets_filter <-
      extract_time_elapsed(
        extract_test_data,
        datasets = c(
          "data-raw/data/schoolsdat1.rds",
          "data-raw/data/schoolsdat3.rds"
        )
      )

    ##  dataset filter should return correct object
    # should be a dataframe
    expect_true(is.data.frame(datasets_filter))

    # should have correct dimensions
    expect_equal(dim(datasets_filter), c(48, 5))

    # should have correct colnames
    expect_named(datasets_filter,
                 c("sim_name", "dataset", "chain", "stage", "elapsed"))

    # all columns should be correct type
    expect_equal(
      lapply(datasets_filter, typeof),
      list(
        "sim_name" = "character",
        "dataset" = "character",
        "chain" = "integer",
        "stage" = "character",
        "elapsed" = "double"
      )
    )

    # only specified datasets should be returned
    expect_equal(sum(!(
      datasets_filter$dataset %in% c(
        "data-raw/data/schoolsdat1.rds",
        "data-raw/data/schoolsdat3.rds"
      )
    )), 0)

  }
)

test_that(
  paste(
    "extract_time_elapsed.stansim_collection returns correct values",
    "filtering on chains"
  ),
  {
    ## read in test stansim obj to extract from
    extract_test_data <-
      readRDS("objects/collection_for_method_tests.rds")

    chains_filter <-
      extract_time_elapsed(extract_test_data,
                           chains = c(1, 3))

    ##  dataset filter should return correct object
    # should be a dataframe
    expect_true(is.data.frame(chains_filter))

    # should have correct dimensions
    expect_equal(dim(chains_filter), c(48, 5))

    # should have correct colnames
    expect_named(chains_filter,
                 c("sim_name", "dataset", "chain", "stage", "elapsed"))

    # all columns should be correct type
    expect_equal(
      lapply(chains_filter, typeof),
      list(
        "sim_name" = "character",
        "dataset" = "character",
        "chain" = "integer",
        "stage" = "character",
        "elapsed" = "double"
      )
    )

    # only specified chains should be returned
    expect_equal(sum(!(chains_filter$chain %in% c(1, 3))), 0)

  }
)

test_that(
  paste(
    "extract_time_elapsed.stansim_collection returns correct values",
    "filtering on stage"
  ),
  {
    ## read in test stansim obj to extract from
    extract_test_data <-
      readRDS("objects/collection_for_method_tests.rds")

    stages_filter <-
      extract_time_elapsed(extract_test_data, stages = "total")

    ##  dataset filter should return correct object
    # should be a dataframe
    expect_true(is.data.frame(stages_filter))

    # should have correct dimensions
    expect_equal(dim(stages_filter), c(32, 5))

    # should have correct colnames
    expect_named(stages_filter,
                 c("sim_name", "dataset", "chain", "stage", "elapsed"))

    # all columns should be correct type
    expect_equal(
      lapply(stages_filter, typeof),
      list(
        "sim_name" = "character",
        "dataset" = "character",
        "chain" = "integer",
        "stage" = "character",
        "elapsed" = "double"
      )
    )

    # only specified stages should be returned
    expect_equal(sum(!(stages_filter$stage == "total")), 0)

  }
)

test_that(
  paste(
    "extract_time_elapsed.stansim_collection returns correct values",
    "filtering on elapsed"
  ),
  {
    ## read in test stansim obj to extract from
    extract_test_data <-
      readRDS("objects/collection_for_method_tests.rds")

    elapsed_filter <-
      extract_time_elapsed(
        extract_test_data,
        elapsed = function(x)
          x > .02
      )

    ##  dataset filter should return correct object
    # should be a dataframe
    expect_true(is.data.frame(elapsed_filter))

    # should have correct dimensions
    expect_equal(dim(elapsed_filter), c(52, 5))

    # should have correct colnames
    expect_named(elapsed_filter,
                 c("sim_name", "dataset", "chain", "stage", "elapsed"))

    # all columns should be correct type
    expect_equal(
      lapply(elapsed_filter, typeof),
      list(
        "sim_name" = "character",
        "dataset" = "character",
        "chain" = "integer",
        "stage" = "character",
        "elapsed" = "double"
      )
    )

    # only specified stages should be returned
    expect_equal(sum(!(elapsed_filter$elapsed > .02)), 0)

  }
)
