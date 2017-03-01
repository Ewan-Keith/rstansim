context("Calculating number of parallel cores")

test_that("Invalid parameters return the correct error message", {
  expect_error(coresCalc("1", 1),
               "stanChains parameter must be numeric")

  expect_error(coresCalc(1, "1"),
               "useCores parameter must be numeric")

  expect_error(coresCalc(1.5, 1),
               "stanChains parameter must be an integer value")

  expect_error(coresCalc(1, 1.5),
               "useCores parameter must be an integer value")

  expect_error(coresCalc(-1, 1),
               "stanChains parameter must be positive")

  expect_error(coresCalc(1, -1),
               "useCores parameter must be positive")

  expect_error(coresCalc(1, 9999),
               paste0("UseCores parameter must be less than",
               "the number of detected cores"))

})

test_that("Calculations are correct", {
  expect_equal(coresCalc(), 1)

  expect_equal(coresCalc(4.0, 4.0), 1)

  expect_equal(coresCalc(2, 4), 2)

  expect_equal(coresCalc(4, 2), 1)

  expect_equal(suppressMessages(
    coresCalc(2, 3)), 1)

  expect_message(coresCalc(2, 3), paste0(
    "1 cores will go unused with 3 cores available and 2",
    " chains per run"))

})
