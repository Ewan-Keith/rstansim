## code that produces a converged and un-converged stan model for unit testing
# produced data is stored in the tests/testthat folder along with tests

schools_dat <- list(J = 8,
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

##-------------------------------------------------
## converged model

set.seed(1234)

fitConverged <- stan(file = 'data-raw/8schools.stan', data = schools_dat,
            iter = 600, chains = 4)

## check that rhat is sufficient
# > max(summary(fitConverged)$summary[, "Rhat"])
# [1] 1.015001

saveRDS(fitConverged,
        "tests/testthat/convergedFit.rds")

##-------------------------------------------------
## non-converged model


set.seed(1234)

fitNonConverged <- stan(file = 'data-raw/8schools.stan', data = schools_dat,
                     iter = 20, chains = 4)

## check that rhat is not sufficient
# > max(summary(fitNonConverged)$summary[, "Rhat"])
# [1] 2.329573

saveRDS(fitNonConverged,
        "tests/testthat/nonConvergedFit.rds")
