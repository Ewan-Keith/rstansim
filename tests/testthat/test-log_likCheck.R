context("log_likCheck should detect absence of 'log_lik' in model")

##-------------------------------------------------
test_that(
  paste(
    "With user entering 'Y' when prompted",
    "log_likCheck should always return TRUE"
  ),
  {
    with_mock(
      # always return 1 on menu for regex testing
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
      # always return 1 on menu for regex testing
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
