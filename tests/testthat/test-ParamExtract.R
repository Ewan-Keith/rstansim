context("Parameter extraction and flatten should function correctly")

attempts <- setNames(1, "fit_attempts")
convergedModel <- c(readRDS("convergedFit.rds"), attempts)
nonConvergedModel <- c(readRDS("nonConvergedFit.rds"), attempts)

# converged extracts for testing
attempts <- setNames(1, "fit_attempts")

convergedExtract1 <- c(readRDS("convergedExtractTest1.rds"), attempts)
convergedExtract2 <- c(readRDS("convergedExtractTest2.rds"), attempts)
convergedExtract3 <- c(readRDS("convergedExtractTest3.rds"), attempts)

# loo in/output for testing
loo_input <- readRDS("loo_input_test.rds")
loo_output <- readRDS("loo_output_test.rds")

test_that("correct output is extracted from fitted test models (LOO=FALSE)", {

  expect_equal(paramExtract(convergedModel, c("mu", "^eta"), FALSE, c("2.5%", "50%", "97.5%")),
               convergedExtract1)

  expect_equal(paramExtract(convergedModel, c("mu", "eta"), FALSE, c("mean", "se_mean", "sd")),
               convergedExtract2)

  expect_equal(paramExtract(convergedModel, c("tau", "lp__"), FALSE, c("n_eff", "Rhat", "25%", "75%")),
               convergedExtract3)


})

test_that("correct output is extracted from fitted test models (LOO=TRUE)", {
  with_mock(

    # pass over the log_lik extraction (at least for now)
    `loo::extract_log_lik` = function(...) {},

    # force the return of valid loo extract (from loo vignette)
    # https://cran.r-project.org/web/packages/loo/vignettes/loo-example.html
    `loo::loo` = function(...) {
      loo_input
    },

    expect_equal(paramExtract(convergedModel, c("tau", "lp__"), TRUE, c("n_eff", "Rhat", "25%", "75%")),
                 loo_output)
  )

})
