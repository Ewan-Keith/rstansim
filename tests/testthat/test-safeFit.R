context("The convergence checker function (and it's internal recursive helper should run as expected")

convergedModel <- readRDS("convergedFit.rds")
nonConvergedModel <- readRDS("nonConvergedFit.rds")


##-------------------------------------------------
test_that("if converged, safeFitRecurs should return the correct list", {
  with_mock(
    # always return a converged stanfit object
    `rstan::stan` = function(file, data,
                             iter, chains) {
      convergedModel
    },

    # can't test direct so check that s4 slots are the same
    testList <- safeFit(
      stanModel = "test",
      stanData = list("X" = 1),
      stanIter = 20,
      stanChains = 4,
      maxRhat = 1.05,
      maxFailure = 5
    ),

    testList[[1]] <- getSlots(is(testList[[1]])),


    expect_equal(testList,
                 list(getSlots(
                   is(convergedModel)
                 ), "attempts" = 1)


    )
  )

})

