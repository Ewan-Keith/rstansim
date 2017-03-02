context("stanSimDefaults should produce correct values")

test_that("correct defaults returned when queried", {

  expect_equal(
    stanSimDefaults(),
    list(
      "stanArgsDefault" =
        formals(rstan::stan),
      "simArgsDefault" = list(
        "simData" = NULL,
        "LOO" = FALSE,
        "useCores" = 1,
        "maxFailures" = 5,
        "maxRhat" = 1.05
      ),
      "returnArgsDefault"  = list("pars" = NULL,
                                  "probs" = c(.025, .500, .975))
    )
  )

  expect_equal(stanSimDefaults("stanArgsDefault"),
               formals(rstan::stan))

  expect_equal(stanSimDefaults("simArgsDefault"),
               list("simData" = NULL,
                    "LOO" = FALSE,
                    "useCores" = 1,
                    "maxFailures" = 5,
                    "maxRhat" = 1.05))

  expect_equal(stanSimDefaults("returnArgsDefault"),
               list("pars" = NULL,
                    "probs" = c(.025, .500, .975)))

  })

test_that("returns correct error message with invalid input", {

  expect_error(stanSimDefaults("wrong"),
               "Invalid 'getDefault' Parameter, see documentation")

})
