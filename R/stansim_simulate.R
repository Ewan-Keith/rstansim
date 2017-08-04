#-----------------------------------------------------------------
#### stansim_simulate ####
#' Simulate datasets from a stan model
#'
#' @description \code{stansim_simulate()} takes a specified stan model and allows the user to simulate data from it
#' based on specified parameter values. The user then specified which data they wish to return and how many simulations
#' they wish to run. By default an object of class \code{stansim_data} will be returned, but if a character is provided to
#' \code{save_dir} then the data will be saved as individual .rds files in the specified directory.
#'
#' To allow for simulated data to be directly fed into stan model that simulated them as input data, the sim_drop argument is provided.
#' If \code{sim_drop} is true then any stan data object with a name beginning with "sim_" wil have this string removed from it's name.
#' For example, the simulated data "sim_x" would be returned simply as "x". This helps avoid the issue of overlapping data names for both
#' input and output
#' @param file temp
#' @param data_name temp
#' @param save_dir temp
#' @param holding_data temp
#' @param sim_params temp
#' @param param_values temp
#' @param datasets temp
#' @param use_cores temp
#' @param sim_drop temp
#'
#' @export
stansim_simulate <-
  function(file,
           data_name = paste0("Simdata_", Sys.time()),
           save_dir = NULL,
           holding_data = NULL,
           sim_params = "all",
           param_values = NULL,
           datasets = 1,
           use_cores = 1,
           sim_drop = TRUE) {


  #-----------------------------------------------------------------
  #### input checks ####
  # file must be character
  if(typeof(file) != "character")
    stop("file must be of type character")

  # data_name must be character
  if (!is.character(data_name))
    stop("data_name must be of type character")

  # save_dir must be character or NULL
  if(typeof(save_dir) != "character" & !(is.null(save_dir)))
    stop("save_dir must be NULL or of type character")

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

  # ----------------------------------------------------------------
  #### set up for parallel running ####
  cl <- parallel::makeCluster(use_cores)
  doParallel::registerDoParallel(cl)

  # define %dopar% alias
  `%doparal%` <- foreach::`%dopar%`

  # -----------------------------------------------------------------
  ## pre-compile stan model
  compiled_model <- rstan::stan_model(file = file)

  #-----------------------------------------------------------------
  #### run simulations ####
  data_list <-
    foreach::foreach(1:datasets) %doparal%
    simulate_internal(
      cmodel = compiled_model,
      holding_data = holding_data,
      sim_params = sim_params,
      param_values = param_values,
      sim_drop = sim_drop
    )

  # de-register the parallel background once done
  parallel::stopCluster(cl)

  # store in stansim_data object
  data_object <- stansim_data(data_name = data_name,
                              data = data_list,
                              compiled_model = compiled_model)

  #-----------------------------------------------------------------
  #### return or save output ####
  if(is.null(save_dir)){
    # return
    return(data_object)
  } else {
    write_data(data_object, path = save_dir, data_name = data_name)
  }

}






# simulate_internal placeholder
simulate_internal <- function(cmodel, holding_data, sim_params, param_values, sim_drop) {
  fitted <-
    rstan::sampling(
      object = cmodel,
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

  return(simplified_extracted)

}




