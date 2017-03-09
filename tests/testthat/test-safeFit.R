context("The convergence checker function (and it's internal recursive helper) should run as expected")

convergedModel <- readRDS("convergedFit.rds")
nonConvergedModel <- readRDS("nonConvergedFit.rds")


##-------------------------------------------------
test_that("if converged, safeFitRecurs should return the correct list", {
  with_mock(
    # always return a converged stanfit object
    `rstan::stan` = function(...) {
      convergedModel
    },

    # can't test direct so check that s4 slots are the same
    testList <- safe_fit(
      max_rhat = 1.05,
      max_failures = 5,
      stan_args = list(
        stanModel = "test",
        stanData = list("X" = 1),
        stanIter = 20,
        stanChains = 4)
      ),

    testList[[1]] <- getSlots(is(testList[[1]])),


    expect_equal(testList,
                 list(getSlots(
                   is(convergedModel)
                 ), "attempts" = 1)
    )
  )
})


##-------------------------------------------------
test_that("if not converged, correct error should be returned", {
  with_mock(
    # always return a npn-converged stanfit object
    `rstan::stan` = function(...) {
      nonConvergedModel
    },

    expect_equal(safe_fit(
      max_rhat = 1.05,
      max_failures = 5,
      stan_args = list(
        stanModel = "test",
        stanData = list("X" = 1),
        stanIter = 20,
        stanChains = 4)
      ), "convergence failed for 5 attempts")
    )
})

