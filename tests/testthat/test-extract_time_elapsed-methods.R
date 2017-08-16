context("testing all extract_time_elapsed methods")

test_that(
  paste("extract_time_elapsed.stansim_simulation function should return",
        "expected results"), {

          ## read in test stansim obj to extract from
          extract_test_data <-
            readRDS("objects/test_stansim.rds")

          ## input errors should be as expected
          expect_error(extract_time_elapsed(extract_test_data, values = 5),
                       "value argument must be NULL or a function")

          expect_error(
            extract_time_elapsed(extract_test_data, datasets = 5),
            "datasets argument must be of type character"
          )

          expect_error(
            extract_time_elapsed(extract_test_data, parameters = 5),
            "parameter argument must be of type character"
          )

          expect_error(
            extract_time_elapsed(extract_test_data, estimates = 5),
            "estimate argument must be of type character"
          )
})
