#-----------------------------------------------------------------
#### refit function ####
#' Refit specified datasets in a stansim_simulation object
#'
#' @description \code{refit} Takes a \code{stansim_simulation} object and a
#'   vector of characters corresponding to the names of datasets fitted within
#'   the \code{stansim_simulation} object, and refits the stan model for each of
#'   these instances. This allows users to refit any specific models using new
#'   stan arguments if need be (e.g. if the model fails to converge in the
#'   original run).
#'
#' @param object An object of S3 class stansim_simulation.
#' @param datasets The full names of the data files to be refitted. These must
#'   be consistent both with the dataset names stored within the
#'   \code{stansim_simulation} object, and with the copies of the data files
#'   relative to the current working directory. This is best ensured by running
#'   refit from the same working directory as the original \code{fit_models()} call.
#' @param stan_args A list of function arguments to be used by the internal
#'   \code{rstan::sampling()} function when fitting the models. If not specified
#'   then the \code{sampling()}  defaults are used.
#' @param calc_loo If \code{TRUE} then model fit statistics will be calculated
#'   using the \code{loo} package. If \code{TRUE} there must be a valid log_lik
#'   quantity specified in the generated quantities section of the provided stan
#'   model.
#' @param use_cores Number of cores to use when running in parallel. Each stan
#'   model is fitted serially regardless of the number of chains ran as
#'   parallelisation across models is more flexible than within.
#' @param cache If \code{TRUE} then the results for each instance are written to
#'   a local, temporary file so that data is not lost should the function not
#'   terminate properly. This temporary data is removed upon the model
#'   terminating as expected. If \code{FALSE} no data is written and results are
#'   only returned upon the correct termination of the whole function. The
#'   default value of \code{TRUE} is recommended unless there are relevant
#'   write-permission restrictions.
#' @param seed Set a seed for the \code{fit_models()} function.
#' @return An S3 object of class \code{stansim_simulation} recording relevant
#'   simulation data.
#'
#' @examples
#' \dontrun{
#' # refit datasets "data_file-12.rds" & "data_file-08.rds"
#' refit(simulation,
#'       datasets = c("data_file-12.rds", "data_file-08.rds")
#'       use_cores = 4)
#'
#' # refit dataset "data_file-12.rds" using a larger number of samples
#' refit(simulation,
#'       datasets = "data_file-12.rds",
#'       stan_args = list(iter = 4000),
#'       use_cores = 4)
#' }
#'
#' @export
refit <-
  function(object,
           datasets,
           stan_args = list(),
           calc_loo = FALSE,
           use_cores = 1L,
           cache = TRUE,
           seed = floor(stats::runif(1, 1, 1e+05))) {


  ## input checks

  if (class(object) != "stansim_simulation")
    stop("object must be of class stansim_simulation")

  if (typeof(datasets) != "character")
    stop("datasets argument must be of type character")

  # check all dataset args exist and can be found
  file_exists <- function(dataset)
    if (!file.exists(dataset)) {
      stop(paste0("dataset \"", dataset,
        "\" could not be found. Check your file structure"
      ))
    }
  lapply(datasets, file_exists)

  # check all dataset args are in the stansim_simulation object
  data_exists <- function(dataset, object_data){
    if (!(dataset %in% object_data$dataset))
      stop(paste0(
        "datasets argument \"",
        dataset,
        "\" not found in provided stansim_simulation object data"))
  }
  lapply(datasets, data_exists, object_data = object$data)

  ####-----------------------------------------------------------------
  ## prepare relevant call args

  # get call arguments to rerun fit_models
  call_args <- object$raw_call

  # overwrite old stan args with any new ones
  new_stan_args <- utils::modifyList(
    call_args$stan_args,
    stan_args
  )
  call_args$stan_args <- new_stan_args

  ## overwrite other call args with new vals
  call_args$calc_loo <- calc_loo
  call_args$use_cores <- use_cores
  call_args$cache <- cache
  call_args$seed <- seed
  call_args$sim_data <- datasets

  ####-----------------------------------------------------------------
  ## refitting datasets
  # refit the specified simulations
  refitted_stansim <- do.call(fit_models, call_args)

  ####-----------------------------------------------------------------
  ## replace the relevant entries of the original object
  new_object <- object

  ## update the instances entries
  # loop over the current instances
  for (i in seq(length(new_object$instances))){
    # store current instance name
    data_name <- new_object$instances[[i]]$data_name

    # if instance name is one to be replaced
    if (data_name %in% datasets){

      # find the matching instance data_name in the new stansim obj
      new_instance_index <- which(sapply(
        refitted_stansim$instances,
        function(x) x$data_name == data_name ))

      # replace the previous instance with the new one
      new_object$instances[[i]] <-
        refitted_stansim$instances[[new_instance_index]]

    }
  }

  ## update the data field with new data
  # remove the to be replaced data
  trimmed_data <-
    new_object$data[!(new_object$data$data %in% datasets), ]

  # add the new replacement data
  new_object$data <- rbind(trimmed_data, refitted_stansim$data)

  ## update the refitted field with new data
  new_object$refitted <- unique(c(new_object$refitted, datasets))


  ####-----------------------------------------------------------------
  ## return the new object
  new_object
}
