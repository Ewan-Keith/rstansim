#### code that produces various objects (usually stan and stansim obs)
## to be used in unit testing

library(rstansim)
library(rstan)

set.seed(12345)

#-----------------------------------------------------------------
#### small scale 8schools example for basic stansim methods testing ####

test_stan_args <- list(file = "data-raw/8schools.stan",
                     iter = 1000, chains = 4)

test_stansim <- stansim(stan_args = test_stan_args,
                         sim_data = dir("data-raw/data",
                                        full.names = TRUE), use_cores = 4,
                        stansim_seed = 12345)

saveRDS(test_stansim, "objects/test_stansim.rds")

#-----------------------------------------------------------------
#### small scale 8schools example for testing that needs a stanfit object ####

test_stan_args <- list(file = "data-raw/8schools.stan",
                       iter = 1000, chains = 4,
                       data = readRDS(dir("data-raw/data",
                                          full.names = TRUE)[1]),
                       seed = 12345)

test_stanfit <- do.call(stan, test_stan_args)

saveRDS(test_stanfit, "objects/test_stanfit.rds")

#-----------------------------------------------------------------
#### loo friendly small stanfit for extraction testing ####

# Prepare data
url <- "http://stat.columbia.edu/~gelman/arm/examples/arsenic/wells.dat"
wells <- read.table(url)
wells$dist100 <- with(wells, dist / 100)
X <- model.matrix(~ dist100 + arsenic, wells)

row_nums <- sample(nrow(X), size = 50, replace = FALSE)
small_X <- X[row_nums, ]

standata <- list(y = wells$switch[row_nums],
                 X = small_X, N = nrow(small_X),
                 P = ncol(small_X))

# Fit model
fit_loo <- stan("data-raw/logistic.stan",
                data = standata, iter = 500,
                seed = 12345)

saveRDS(fit_loo, "objects/test_stanfit_loo.rds")

#-----------------------------------------------------------------
#### output of two stansim_uni calls for testing stansim constructor ####

test_stanfit <- readRDS("objects/test_stanfit.rds")

#### roll down long stansim_uni calls ####
test_stansim_uni1 <-
  rstansim:::stansim_uni(
    test_stanfit,
    data_name = "data_name1",
    ran_at = Sys.time(),
    long_data = rstansim:::param_extract(
      test_stanfit,
      calc_loo = F,
      parameters = "all",
      probs = c(.025, .25, .5, .75, .975),
      estimates = c("mean",
                    "se_mean",
                    "sd",
                    "n_eff",
                    "Rhat"),
      data = "datafile location1.rds"
    ),
    stan_warnings = "warning strings1",
    cache = F
  )

test_stansim_uni2 <-
  rstansim:::stansim_uni(
    test_stanfit,
    data_name = "data_name2",
    ran_at = Sys.time(),
    long_data = rstansim:::param_extract(
      test_stanfit,
      calc_loo = F,
      parameters = "all",
      probs = c(.025, .25, .5, .75, .975),
      estimates = c("mean",
                    "se_mean",
                    "sd",
                    "n_eff",
                    "Rhat"),
      data = "datafile location2.rds"
    ),
    stan_warnings = "warning strings2",
    cache = F
  )


#### un roll down calls ####

test_stansim_uni_list <- list(test_stansim_uni1, test_stansim_uni2)

saveRDS(test_stansim_uni_list,
        "objects/test_stansim_uni_list.rds")


#-----------------------------------------------------------------
#### output of a stansim_uni object for stansim() function mocking ####

test_stan_args <-
  list(
    object = rstan::stan_model("data-raw/8schools.stan"),
    iter = 500,
    chains = 4,
    seed = 12345
  )

single_out <- rstansim:::single_sim(
  datafile = dir("data-raw/data",
                 full.names = TRUE)[1],
  stan_args = test_stan_args,
  calc_loo = F,
  parameters = "all",
  probs = c(.025, .25, .5, .75, .975),
  estimates = c("mean", "se_mean",
                "sd", "n_eff", "Rhat"),
  stan_warnings = "catch",
  cache = F
)

saveRDS(single_out,
        "objects/test_stansim_uni_single.rds")

#-----------------------------------------------------------------
#### object of type stanmodel to avoid compile time in tests ####
testpc <- stan_model(file = 'data-raw/8schools.stan')

saveRDS(testpc,
        "objects/test_stanmodel.rds")

#-----------------------------------------------------------------
#### Partial stansim() output for testing refit ####

test_stan_args_refit <- list(file = "data-raw/8schools.stan",
                       iter = 1000, chains = 4, seed = 12345)

test_stansim_refit <- stansim(stan_args = test_stan_args_refit,
                        sim_data = dir("data-raw/data",
                                       full.names = TRUE)[c(1, 3)], use_cores = 4,
                        stansim_seed = 12345)

saveRDS(test_stansim_refit, "objects/test_stansim_refit.rds")



