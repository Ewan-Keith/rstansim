
schools_dat <- list(J = 8,
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

testStanArgs <- list(file = 'devtest/8schools.stan', data = schools_dat,
                    iter = 1000, chains = 4)

testSimArgs <- list(simData = dir("devtest/data", full.names = TRUE), maxRhat = 1.05, useCores = 4)

testReturnArgs <- list("pars" = c("mu", "^eta"),
                       "probs" = c("2.5%", "50%", "97.5%"))

stanSim(stanArgs = testStanArgs, simArgs = testSimArgs, returnArgs = testReturnArgs)


params <- readRDS("devtest/data/schoolsdat1.rds")

library(rstan)

fit <- stan(file = 'devtest/8schools.stan', data = params,
            iter = 1000, chains = 4)

fit2 <- do.call(stan, list(file = 'devtest/8schools.stan', data = params,
                       iter = 1000, chains = 4))

# extract testing, test is a stanfit object

params <- c("mu", "tau", "eta")

rstan::summary(test)$summary[params,]

rstan::summary(test)$summary[grepl("^eta"),]

# working regex example
rstan::summary(test)$summary[grepl("^eta", rownames(rstan::summary(test)$summary)),]
