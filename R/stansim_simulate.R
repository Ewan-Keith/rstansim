
reg_sim <- function(N = 100) {
  list("N" = N, "x" = rep(0, N), "y" = rep(0, N))
}

reg_data <- reg_sim(1000)

file <- 'tests/testthat/data-raw/simtestreg.stan'

#-----------------------------------------------------------------
#### stansim_simulate ####
stansim_simulate <-
  function(file,
           save_dir,
           hold_data = NULL,
           sim_params = "all",
           param_values = NULL,
           datasets = 1,
           sim_drop = TRUE) {


  #-----------------------------------------------------------------
  #### input checks ####

  # save_dir must be character


  # save_dir must not already exist


  # file must be character


  # data must be NULL or list


  # param values must be NULL or list


  # datasets must be a positive integer

  #-----------------------------------------------------------------
  #### prep saving dir ####

  #-----------------------------------------------------------------
  #### run simulations ####
  simulate_internal(file = file, hold_data = hold_data, sim_params = sim_params,
                    sim_drop = sim_drop)
}



simulate_internal <- function(file, hold_data, sim_params, sim_drop) {
  fitted <-
    rstan::stan(
      file = file,
      data = hold_data,
      iter = 1,
      chains = 1,
      cores = 1,
      algorithm = "Fixed_param"
    )

  params_and_data <- c(hold_data, rstan::extract(fitted))

  extracted <- params_and_data[sim_params]

  # remove unnecesary dimnames attributes
  remove_attributes <- function(x) {
    attributes(x)$dimnames <- NULL
    x
  }
  clean_extracted <- lapply(X = extracted, FUN = remove_attributes)

  # if sim_drop then cut "sim_" from all param/data names
  names(clean_extracted) <-
    if (sim_drop) {
      gsub("^sim_", "", names(clean_extracted))
    } else {
      names(clean_extracted)
    }

  clean_extracted

}


fit <- stansim_simulate(file = file, hold_data = reg_data, datasets = 2, sim_params = c("sim_x", "sim_y", "N"))

extracted <- extract(fit)

plot(extracted$stansim_x, extracted$stansim_y)


## refit test
fit_test <- rstan::stan(file, data = fit)



