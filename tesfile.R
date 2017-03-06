

testStanArgs <- list(file = 'devtest/8schools.stan', data = list(),
                    iter = 1000, chains = 4)

testSimArgs <- list(simData = dir("devtest/data", full.names = TRUE), maxRhat = 1.05, useCores = 4)

stanSim(stanArgs = testStanArgs, simArgs = testSimArgs)


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
