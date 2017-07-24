context("brief rename tests")

## read in test stansim_simulation obj to use in tests
sim1 <-
  readRDS("objects/test_stansim.rds")

coll1 <-
  readRDS("objects/collection_for_method_tests.rds")

test_that("rename fails correctly", {

  expect_error(rename(sim1, 55),
               "new_name must be of type character")

  expect_error(rename(coll1, 55),
               "new_name must be of type character")

})

test_that("rename updates names correctly", {

  expect_equal(rename(sim1, "new sim name")$sim_name,
               "new sim name")

  expect_equal(rename(coll1, "new coll name")$collection_name,
               "new coll name")

})
