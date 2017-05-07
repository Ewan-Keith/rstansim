
stan_args <- list("data-raw/lin_reg_test/lin_reg_test.stan", data = list(N = 50))
data_locations <- dir("data-raw/lin_reg_test/data", full.names = T)


test <- stan_sim(stan_args, sim_data = data_locations)


#########
schools_dat <- list(J = 8,
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

testStanArgs <- list(file = 'devtest/8schools.stan', data = schools_dat,
                    iter = 1000, chains = 4)

testSimArgs <- list(simData = dir("devtest/data", full.names = TRUE), useCores = 4)

testReturnArgs <- list("pars" = c("mu", "^eta"),
                       "probs" = c("2.5%", "50%", "97.5%"))

testout <- stan_sim(stan_args = testStanArgs, sim_data = dir("devtest/data", full.names = TRUE), use_cores = 4)


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
