# simulate_internal
simulate_internal <- function(cmodel, input_data, vars, param_values, nsim, use_cores, sim_drop, seed) {

  fitted <-
    rstan::sampling(
      object = cmodel,
      data = input_data,
      init = list(param_values),
      iter = nsim,
      chains = 1,
      cores = use_cores,
      warmup = 0,
      algorithm = "Fixed_param",
      seed = seed
    )

  # extract parameter and generated quantities
  param_and_gen_extract <- rstan::extract(fitted)

  # split simulations
  split_p_and_g <- lapply(param_and_gen_extract, split_sims)

  ## for each parameter, put the nth item into list n
  # init list
  arranged_list <- vector("list", nsim)

  # extract and re-order
  for(i in 1:nsim){
    arranged_list[[i]] <- lapply(split_p_and_g, `[[`, i)
  }

  # merge simulated data with user specified data for subsetting
  params_and_data <- lapply(arranged_list, "c", input_data)

  # extract the values specified for recording
  if ("all" %in% vars) {
    extracted <- params_and_data
  } else {
    extracted <- lapply(params_and_data, "[", vars)
  }

  ## remove unnecesary dimnames attributes
  # set low level reset function
  remove_attributes <- function(x) {
    attributes(x)$dimnames <- NULL
    x
  }
  # init output list
  clean_extracted <- vector(mode = "list", length = nsim)

  # loop over seperate lists and reset dimnames
  for (i in seq(nsim)) {
    clean_extracted[[i]] <- lapply(X = extracted[[i]], FUN = remove_attributes)
  }

  # if sim_drop then cut "sim_" from all param/data names
  sim_dropper <- function(clean_list, do_drop = sim_drop) {
      if (do_drop) {
        gsub("^sim_", "", names(clean_list))
      } else {
        names(clean_list)
      }
  }
  # init output list
  output_data <- vector(mode = "list", length = nsim)

  # call sim_dropper over lists
  for (i in seq(nsim)) {
    output_data[[i]] <-
      stats::setNames(clean_extracted[[i]], sim_dropper(clean_extracted[[i]]))
  }

  return(output_data)

}

# splits out multiple simulations into a list of data objects
split_sims <- function(param2split){

  # if it's a vector it's actually a scalar and needs special treatment
  if (length(dim(param2split)) == 1) {
    return(split(param2split, seq_along(param2split)))

  } else {

    # if it's greater dimension that vector use the general purpose splitter
    return(split_along_dim(param2split, 1))
  }
}


# define the general purpose array splitter if not a vector
# below Stack Overflow saved my life here
# https://stackoverflow.com/questions/20198751/three-dimensional-array-to-list
split_along_dim <- function(stan_array, split_dim) {
  stats::setNames(lapply(
    split(stan_array, arrayInd(
      seq_along(stan_array), dim(stan_array)
    )[, split_dim]),
    array,
    dim = dim(stan_array)[-split_dim],
    dimnames(stan_array)[-split_dim]
  ),
  dimnames(stan_array)[[split_dim]])
}

