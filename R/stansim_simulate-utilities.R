# simulate_internal
simulate_internal <- function(cmodel, input_data, vars, param_values, sim_drop) {
  fitted <-
    rstan::sampling(
      object = cmodel,
      data = input_data,
      init = list(param_values),
      iter = 1,
      chains = 1,
      cores = 1,
      algorithm = "Fixed_param",
      refresh = -1
    )

  # merge simulated data with user specified data for subsetting
  params_and_data <- c(input_data, rstan::extract(fitted))

  # extract the values to simulate
  extracted <- params_and_data[vars]

  # remove unnecesary dimnames attributes
  remove_attributes <- function(x) {
    attributes(x)$dimnames <- NULL
    x
  }
  clean_extracted <- lapply(X = extracted, FUN = remove_attributes)

  # simplify any one dim arrays, careful with this. come back to and check.
  simplified_extracted <- lapply(clean_extracted, "c")

  # if sim_drop then cut "sim_" from all param/data names
  names(simplified_extracted) <-
    if (sim_drop) {
      gsub("^sim_", "", names(simplified_extracted))
    } else {
      names(simplified_extracted)
    }

  return(simplified_extracted)

}
