context("log_likCheck should detect absence of 'log_lik' in model")

##-------------------------------------------------
test_that(
  paste(
    "With user entering 'Y' when prompted",
    "log_likCheck should always return TRUE"
  ),
  {
    with_mock(
      # always return 1 on menu for testing
      `utils::menu` = function(choices,
                               graphics = FALSE,
                               title = NULL) {
        1
      },

      good <- "generated quantities {
      vector[P] log_lik[N]",

      bad <- "generated quantities vector[P] log_lik[N]",

      expect_equal(Log_likCheck(good), TRUE),

      expect_equal(Log_likCheck(bad), TRUE)

    )
  }
)

##-------------------------------------------------
test_that(
  paste(
    "With user entering 'N' when prompted",
    "log_likCheck should return TRUE when",
    "regex is correct and false otherwise"
  ),
  {
    with_mock(
      # always return 2 on menu for testing
      `utils::menu` = function(choices,
                               graphics = FALSE,
                               title = NULL) {
        2
      },

      good <- "generated quantities {
      vector[P] log_lik[N]",

      bad <-
        "generated quantities vector[P] log_lik[N]",

      expect_equal(Log_likCheck(good), TRUE),

      expect_equal(Log_likCheck(bad), FALSE)

    )


  }
)

##-------------------------------------------------
test_that(
  paste(
    "if LOO requested and log_likCheck returns false",
    "stop the main functionwith the correct message"
  ),
  {

    with_mock(
      # always return 2 on menu for testing
      `utils::menu` = function(choices,
                               graphics = FALSE,
                               title = NULL) {
        2
      },

      bad <-
        "generated quantities vector[P] log_lik[N]",

      stanArgs <- list("file" = bad),
      simArgs <- list("LOO" = TRUE),

      # error if false and LOO requested
      expect_error(stanSim(stanArgs, simArgs),
                   "Simulation Stopped as 'log_lik' generated quantity could not be found")


    )


  })
