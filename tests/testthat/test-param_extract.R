context("Parameter extraction and flatten should function correctly")


convergedModel <- readRDS("convergedFit.rds")
nonConvergedModel <- readRDS("nonConvergedFit.rds")
# converged extracts for testing
convergedExtract1 <- readRDS("convergedExtractTest1.rds")
convergedExtract2 <- readRDS("convergedExtractTest2.rds")
convergedExtract3 <- readRDS("convergedExtractTest3.rds")

# loo in/output for testing
loo_input <- readRDS("loo_input_test.rds")
loo_output <- readRDS("loo_output_test.rds")

test_that("correct output is extracted from fitted test models (LOO=FALSE)", {

  expect_equal(param_extract(convergedModel, FALSE, c("mu", "^eta"), c("2.5%", "50%", "97.5%")),
               convergedExtract1)

  expect_equal(param_extract(convergedModel, FALSE, c("mu", "eta"), c("mean", "se_mean", "sd")),
               convergedExtract2)

  expect_equal(param_extract(convergedModel, FALSE, c("tau", "lp__"), c("n_eff", "Rhat", "25%", "75%")),
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

    expect_equal(param_extract(convergedModel, TRUE, c("tau", "lp__"), c("n_eff", "Rhat", "25%", "75%")),
                 loo_output)
  )

})
