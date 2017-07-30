
reg_sim <- function(N = 100, run_estimation = 1) {
  list("N" = N, "x" = rep(0, N), "y" = rep(0, N), "run_estimation" = run_estimation)
}

reg_data <- reg_sim(1000, run_estimation = 0)

file <- 'tests/testthat/data-raw/simtestreg.stan'

#-----------------------------------------------------------------
#### stansim_simulate ####
stansim_simulate <- function(file, save_dir, data = NULL, param_values = NULL, datasets = 1){

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
  simulate_internal(file = file, data = data)
}



simulate_internal <- function(file, data) {
  fitted <-
    rstan::stan(
      file = file,
      data = data,
      iter = 1,
      chains = 1,
      cores = 1,
      algorithm = "Fixed_param"
    )

  extracted <- extract(fitted)

  stansim_name_ind <- grepl("^stansim_.+$", names(extracted))

  sim_data <- extracted[stansim_name_ind]

  # remove "stansim_" from name
  names(sim_data) <-
    substr(names(sim_data), start = 9, stop = nchar(names(sim_data)))

  # remove unnecesary dimnames attributes
  remove_attributes <- function(data) {
    attributes(data)$dimnames <- NULL
    data
  }
  lapply(X = sim_data, FUN = remove_attributes)
}


fit <- stansim_simulate(file = file, data = reg_data, datasets = 2)

extracted <- extract(fit)

plot(extracted$stansim_x, extracted$stansim_y)
