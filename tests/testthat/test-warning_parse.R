context("stan warnings should be parsed correctly by warning_parse")

test_that("divergent transition messages are identified and parsed correctly", {

  divergent_test1 <- paste0("simpleWarning: There were 4 divergent transitions ",
                            "after warmup. Increasing adapt_delta above 0.8 may ",
                            "help. See\nhttp://mc-stan.org/misc/warnings.html#div",
                            "ergent-transitions-after-warmup\n")

  divergent_test2 <- paste0("simpleWarning: There were 229 divergent transitions ",
                            "after warmup. Increasing adapt_delta above 0.957 may ",
                            "help. See\nhttp://mc-stan.org/misc/warnings.html#div",
                            "ergent-transitions-after-warmup\n")

  ##-------------------------------------------------
  ## stan args must be of type list
  expect_error(stan_sim(stan_args = "1"),
               "stan_args must be a list of stan parameters")

})
