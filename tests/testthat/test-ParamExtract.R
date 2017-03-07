context("Parameter extraction and flatten should function correctly")

convergedModel <- readRDS("convergedFit.rds")
nonConvergedModel <- readRDS("nonConvergedFit.rds")

# converged extracts for testing
convergedExtract1 <- readRDS("convergedExtractTest1.rds")
convergedExtract2 <- readRDS("convergedExtractTest2.rds")
convergedExtract3 <- readRDS("convergedExtractTest3.rds")

test_that("correct output is extracted from fitted test models (LOO=FALSE)", {

  expect_equal(paramExtract(convergedModel, c("mu", "^eta"), FALSE, c("2.5%", "50%", "97.5%")),
               convergedExtract1)

  expect_equal(paramExtract(convergedModel, c("mu", "eta"), FALSE, c("mean", "se_mean", "sd")),
               convergedExtract2)

  expect_equal(paramExtract(convergedModel, c("tau", "lp__"), FALSE, c("n_eff", "Rhat", "25%", "75%")),
               convergedExtract3)


})
