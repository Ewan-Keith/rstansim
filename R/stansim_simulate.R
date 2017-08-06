#-----------------------------------------------------------------
#### stansim_simulate ####
#' Simulate datasets from a stan model
#'
#' @description \code{stansim_simulate()} takes a specified stan model and allows the user to simulate data from it
#' based on specified parameter values. The user then specifies which data they wish to save and how many simulations
#' they wish to run. The data will be saved as individual .rds files in the directory specified by \code{path}.
#'
#' By default an object of class \code{stansim_data} will be returned, providing an index of the saved data that can then be provided
#' directly to a \code{stansim()} call.
#'
#' To allow for simulated data to be directly fed into stan model that simulated them as input data, the sim_drop argument is provided.
#' If \code{sim_drop} is true then any stan data object with a name beginning with "sim_" wil have this string removed from it's name.
#' For example, the simulated data "sim_x" would be returned simply as "x". This helps avoid the issue of overlapping data names for both
#' input and output
#'
#' @param file A character string or a connection that R supports specifying the Stan model specification in Stan's modeling language.
#' @param data_name A name attached to the \code{stansim_data} object to
#'   help identify it. It is strongly recomended that an informative name is
#'   assigned. If \code{path} isn't NULL, this will also be the name stem for the saved .rds files.
#' @param input_data Values for the data field in the provided stan model. Values must be provided for all
#' entries even if they are not used in the 'generate quantities' model section producing the simulated data.
#' @param vars The names of the stan variables to return. Defaults to "all", otherwise a vector of variable names should be provided.
#' @param param_values A list containing the named values for the stan model parameters used to simulate data. If a parameter's
#' value is not specified here it will be initialised randomly. Recommended to specify all parameter values.
#' @param datasets The number of simulated datasets to produce.
#' @param path The name of the directory to save the simulated data to, if this doesn't exist it will be created. Defaults to NULL in which the datasets are saved to the working directory
#' @param return_object if FALSE then no \code{stansim_data} object is returned.
#' @param use_cores Number of cores to use when running in parallel.
#' @param sim_drop If TRUE then any simulated data objects beginning in "sim_"
#'   will have this removed. So "sim_x" becomes "x".
#' @param recursive logical. Should elements of the path other than the last be
#'   created? If true, like the Unix command mkdir -p.
#' @return An object of S3 class stansim_data or NULL.
#'
#' @export
stansim_simulate <-
  function(file,
           data_name = paste0("Simdata_", Sys.time()),
           input_data = NULL,
           vars = "all",
           param_values = NULL,
           datasets = 1,
           path = NULL,
           return_object = TRUE,
           use_cores = 1,
           sim_drop = TRUE,
           recursive = TRUE) {


  #-----------------------------------------------------------------
  #### input checks ####
  # file must be character
  if(typeof(file) != "character")
    stop("file must be of type character")

  # data_name must be character
  if (!is.character(data_name))
    stop("data_name must be of type character")

  # path must be character or NULL
  if(typeof(path) != "character" & !(is.null(path)))
    stop("path must be NULL or of type character")

  # input_data must be NULL or list
  if(!(is.null(input_data) | typeof(input_data) == "list"))
    stop("input_data must be NULL or of type list")

  # vars must be character
  if (!is.character(vars))
    stop("vars must be of type character")

  # if "all" provided to vars length must be 1
  if ("all" %in% vars & length(vars) != 1)
    stop("if vars argument contains \"all\", length(vars) must be 1")

  # param values must be NULL or list
  if(!(is.null(param_values) | typeof(param_values) == "list"))
    stop("param_values must be NULL or of type list")

  # datasets must be a positive integer
  if(datasets < 1 | datasets %% 1 != 0)
    stop("datasets must be a positive integer")

  # use_cores must be a positive integer
  if(use_cores < 1 | use_cores %% 1 != 0)
    stop("use_cores must be a positive integer")

  # return_object must be logical
  if(typeof(return_object) != "logical")
    stop("return_object must be of type logical")

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
      input_data = input_data,
      vars = vars,
      param_values = param_values,
      sim_drop = sim_drop
    )

  # de-register the parallel background once done
  parallel::stopCluster(cl)

  #-----------------------------------------------------------------
  #### create list of data with datafile name ####
  # create directory if doesn't exist
  if (!is.null(path)) {
    if (!dir.exists(path)) {
      dir.create(path = path, recursive = recursive)
    }
  }
  # setup path for save
  if (is.null(path)) {
    path <- ""
  } else{
    path <- paste0(path, "/")
  }

  # prep name stem
  name_stem <- data_name

  # write names vector for all objects
  names_vector <- paste0(path, name_stem, "_", seq(length(data_list)), ".rds")

  # attach names to all object data
  named_data <- stats::setNames(data_list, names_vector)

  #-----------------------------------------------------------------
  #### write data to rds files ####

  # function to write each file
  write_named_data <- function(name_list, obj_list){
    saveRDS(obj_list[[name_list]], file = name_list)
  }
  catch <- lapply(names(named_data), write_named_data, named_data)

  #-----------------------------------------------------------------
  #### return stansim_data object or not ####
  if (return_object) {
    return(
      stansim_data(
        data_name = data_name,
        datasets = names_vector,
        compiled_model = compiled_model
      )
    )
  }

}
