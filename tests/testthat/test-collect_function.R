context("collect functions should fucntion correctly")

test_that("collect function fails correctly", {

  ## read in test stansim_simulation obj to test
  stansim_obj <-
    readRDS("objects/test_stansim.rds")


  # collection_name should be a character
  expect_error(collect(collection_name = 55),
               "collection_name must be of type character")

  expect_error(collect(collection_name = NULL),
               "collection_name must be of type character")

  # all non-collection name args must have proper class
  expect_error(
    collect(collection_name = "test",
            object = 55),
    paste(
      "all arguments except collection_name must be",
      "of class \"stansim_simulation\" or \"stansim_collect\""
    )
  )

  expect_error(
    collect(collection_name = "test",
            object = "test"),
    paste(
      "all arguments except collection_name must be",
      "of class \"stansim_simulation\" or \"stansim_collect\""
    )
  )

  expect_error(
    collect(collection_name = "test",
            object = stansim_obj,
            55),
    paste(
      "all arguments except collection_name must be",
      "of class \"stansim_simulation\" or \"stansim_collect\""
    )
  )

  expect_error(
    collect(collection_name = "test",
            object = stansim_obj,
            stansim_obj,
            stansim_obj,
            55),
    paste(
      "all arguments except collection_name must be",
      "of class \"stansim_simulation\" or \"stansim_collect\""
    )
  )

  expect_error(
    collect(collection_name = "test",
            object = 55),
    "still need to add tests for this using stansim_collection objects"
  )

  ## error if only 1 stansim_simulation is provided
  expect_error(
    collect(collection_name = "test",
            object = stansim_obj),
    "A single simulation cannot be used to make a collection."
  )

})

