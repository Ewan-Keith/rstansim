


#-----------------------------------------------------------------
#### stansim_simulate ####
stansim_simulate <-
  function(file,
           save_dir,
           holding_data = NULL,
           sim_params = "all",
           param_values = NULL,
           datasets = 1,
           sim_drop = TRUE) {


  #-----------------------------------------------------------------
  #### input checks ####
  # file must be character
  if(typeof(file) != "character")
    stop("file must be of type character")

  # save_dir must be character
  if(typeof(save_dir) != "character")
    stop("save_dir must be of type character")

  # holding_data must be NULL or list
  if(!(is.null(holding_data) | typeof(holding_data) == "list"))
    stop("holding_data must be NULL or of type list")

  # param values must be NULL or list
  if(!(is.null(param_values) | typeof(param_values) == "list"))
    stop("param_values must be NULL or of type list")

  # datasets must be numeric
    if(typeof(datasets) != "numeric")

  # datasets must be a positive integer
  if(datasets < 1 | datasets %% 1 != 0)
    stop("datasets must be a positive integer")

  # sim_drop must be logical
  if(typeof(sim_drop) != "logical")
    stop("sim_drop must be of type logical")

  #-----------------------------------------------------------------
  #### prep saving dir ####

  #-----------------------------------------------------------------
  #### run simulations ####
  simulate_internal(file = file,
                    holding_data = holding_data,
                    sim_params = sim_params,
                    param_values = param_values,
                    sim_drop = sim_drop)
}



simulate_internal <- function(file, holding_data, sim_params, param_values, sim_drop) {
  fitted <-
    rstan::stan(
      file = file,
      data = holding_data,
      init = list(param_values),
      iter = 1,
      chains = 1,
      cores = 1,
      algorithm = "Fixed_param"
    )

  # merge simulated data with user specified data for subsetting
  params_and_data <- c(holding_data, rstan::extract(fitted))

  # extract the values to simulate
  extracted <- params_and_data[sim_params]

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

  simplified_extracted

}




