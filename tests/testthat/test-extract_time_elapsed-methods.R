context("testing all extract_time_elapsed methods")

test_that(
  paste("extract_time_elapsed.stansim_simulation function should fail",
        "as expected"), {

          ## read in test stansim obj to extract from
          extract_test_data <-
            readRDS("objects/test_stansim.rds")

          ## input errors should be as expected
          expect_error(extract_time_elapsed(extract_test_data, datasets = 5),
                       "datasets argument must be of type character")

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
          expect_error(extract_time_elapsed(extract_test_data, datasets = c("all", "test")),
                       paste(
                         "if datasets argument contains \"any\",",
                         "length\\(datasets\\) must be 1"
                       ))

          expect_error(extract_time_elapsed(extract_test_data, chains = c("all", "test")),
                       paste(
                         "if chains argument contains \"any\",",
                         "length\\(chains\\) must be 1"
                       ))

          expect_error(extract_time_elapsed(extract_test_data, stages = c("all", "warmup")),
                       paste(
                         "if stages argument contains \"any\",",
                         "length\\(stages\\) must be 1"
                       ))
})

test_that(
  "extract_time_elapsed.stansim_simulation returns correct default values", {

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
          expect_named(defaults, c("datasets", "chains", "stage", "elapsed"))

          # all columns should be correct type
          expect_equal(lapply(defaults, typeof),
                       list("datasets" = "character", "chains" = "integer",
                            "stage" = "character", "elapsed" = "double"))
})

test_that(
  paste("extract_time_elapsed.stansim_simulation returns correct values",
                "filtering on datasets"), {

      ## read in test stansim obj to extract from
      extract_test_data <-
        readRDS("objects/test_stansim.rds")

      datasets_filter <-
        extract_time_elapsed(extract_test_data,
                             datasets = c("data-raw/data/schoolsdat1.rds",
                                          "data-raw/data/schoolsdat3.rds"))

      ##  dataset filter should return correct object
      # should be a dataframe
      expect_true(is.data.frame(datasets_filter))

      # should have correct dimensions
      expect_equal(dim(datasets_filter), c(24, 4))

      # should have correct colnames
      expect_named(datasets_filter, c("datasets", "chains", "stage", "elapsed"))

      # all columns should be correct type
      expect_equal(lapply(datasets_filter, typeof),
                   list("datasets" = "character", "chains" = "integer",
                        "stage" = "character", "elapsed" = "double"))

      # only specified datasets should be returned
      expect_equal(datasets_filter$datasets %in% )

    })
