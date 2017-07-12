
stan_args <- list("data-raw/lin_reg_test/lin_reg_test.stan",
                  data = list(N = 50))
data_locations <- dir("data-raw/lin_reg_test/data", full.names = T)


test <- stan_sim(stan_args, sim_data = data_locations)


#########
schools_dat <- list(J = 8,
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

testStanArgs <- list(file = 'tests/testthat/data-raw/8schools.stan',
                    iter = 1000, chains = 4)


testout <- stansim(stan_args = testStanArgs,
                    sim_data = dir("tests/testthat/data-raw/data",
                                   full.names = TRUE),
                    use_cores = 4,
                    cache = F)


params <- readRDS("tests/testthat/data-raw/data/schoolsdat1.rds")

library(rstan)

fit <- stan(file = 'tests/testthat/data-raw/8schools.stan', data = params,
            iter = 1000, chains = 4)

fitwarm <- stan(file = 'data-raw/8schools.stan', data = params,
                iter = 1000, chains = 4, warmup = 0)

fit2 <- do.call(stan, list(file = 'data-raw/8schools.stan', data = params,
                       iter = 1000, chains = 4, cores = 4))

### testing precompiling
testpc <- stan_model(file = 'tests/testthat/data-raw/8schools.stan')

testpc_samp <- sampling(testpc, data = params, iter = 1000, chains = 4)

# extract testing, test is a stanfit object

params <- c("mu", "tau", "eta")

rstan::summary(test)$summary[params,]

rstan::summary(test)$summary[grepl("^eta"),]

# working regex example
rstan::summary(test)$summary[grepl("^eta", rownames(rstan::summary(test)$summary)),]

zz <- file("all.Rout", open = "wt")
sink(zz)
sink(zz, type = "message")
fit <- stan(file = 'tests/testthat/data-raw/8schools.stan', data = params,
            iter = 500, chains = 4)
## revert output back to the console -- only then access the file!
sink(type = "message")
sink()
close(zz)

file.remove("all.Rout")

fit.out <- capture.output(stan(file = 'tests/testthat/data-raw/8schools.stan', data = params,
            iter = 500, chains = 4))


