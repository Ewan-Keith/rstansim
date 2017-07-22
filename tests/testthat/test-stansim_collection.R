context("test stansim_collection constructor for basic consistency")

test_that("constructed object has the right data in the right places", {

  collection_output <-
    rstansim:::stansim_collection(
      collection_name = "test name",
      refitted = "refitted test",
      data = "data test",
      simulations = "simulations test"
    )

  # class is correct
  expect_s3_class(collection_output, "stansim_collection")

  # check type
  expect_type(collection_output, "list")

  # check length
  expect_equal(length(collection_output), 4)

  # check names
  expect_equal(names(collection_output),
               c("collection_name", "data", "refitted", "simulations"))

  # check all named slots are as expected
  expect_equal(collection_output$collection_name, "test name")
  expect_equal(collection_output$refitted, "refitted test")
  expect_equal(collection_output$data, "data test")
  expect_equal(collection_output$simulations, "simulations test")

})
