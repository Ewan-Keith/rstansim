#-----------------------------------------------------------------
#### refit generic method ####
#' Refit specified datafiles in a stansim object
#'
#' @description Generic function for refitting specified datafiles in
#' rstansim objects. By default refits all datafiles, following a user
#' prompt given the potential computational cost (this can be turned off).
#' Otherwise only datafile names provided will be refit.
#'
#' @param object An object of S3 class stansim_single.
#' @param ... Arguments specifying datafiles to refit and control
#' of warning behaviour.
#'
#' @export
refit <- function (object, ...) {
  UseMethod("refit", object)
}

#-----------------------------------------------------------------
#### refit.stansim_single method ####
#' Refit specified datafiles in a stansim_single object
#'
#' @description \code{refit} NOTE YOU'LL HAVE TO RUN THIS FROM THE
#' SAME WD AS BEFORE OTHERWISE FILE NAMES WILL BE WRONG
#'
#' @param object A list of function arguments to be used by
#' the internal \code{stan} function when fitting the models.
#' If not specified then the \code{stan} function defaults are used.
#' @param sim_data A list of strings pointing to the location of
#' .rds files containing the simulation data. See the vignette on
#' producing simulation data for details on the formatting of these datasets.
#' @param calc_loo If \code{TRUE} then model fit statsics will be
#' calculated using the \code{loo} package. If \code{TRUE} there must be
#' a valid log_lik quantity specified in the generated quantities
#' section of the provided stan model.
#' @return An S3 object of class \code{stansim_single} recording relevant
#' simulation data.
#'
#' @export
refit.stansim_single <- function(object, datafiles = "all", stan_args = list(),
                                 calc_loo = FALSE, use_cores = 1L, cache = TRUE,
                                 stansim_seed = floor(stats::runif(1, 1, 1e+05)),
                                 all_warn = TRUE){

  ## input checks
  if(typeof(datafiles) != "character")
    stop("datafiles argument must be of type character")

  if(!is.logical(all_warn))
    stop("all_warn argument must be of type logical")

  # check all datafile args exist and can be found
  file_exists <- function(datafile)
    if (!file.exists(datafile)) {
      stop(paste0("datafile \"", datafile,
        "\" could not be found. Check your file structure"
      ))
    }
  lapply(datafiles, file_exists)

  # check all datafile args are in the stansim_single object
  data_exists <- function(datafile, object_data){
    if(!(datafile %in% object_data))
      stop(paste0(
        "datafiles argument \"",
        datafile,
        "\" not found in provided stansim_object"))
  }
  lapply(datafiles, data_exists, object_data = object$raw_call$sim_data)

  ####-----------------------------------------------------------------
  ## prepare relevant call args

  # get call arguments to rerun stansim
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
  call_args$stansim_seed <- stansim_seed
  call_args$sim_data <- datafiles

  ####-----------------------------------------------------------------
  ## refitting datasets
  # check if user wishes to refit all data
  if(datafiles == "all"){
    message("Are you sure you wish to refit all datafiles?")
    if(menu(c("Yes", "No")) == 2) invisible()
  }

  # refit the specified simulations
  refitted_stansim <- do.call(stansim, call_args)

  ####-----------------------------------------------------------------
  ## replace the relevant entries of the original object
  new_object <- object

  ## update the instances entries
  # loop over the current instances
  for(i in 1:length(new_object$instances)){
    # store current instance name
    data_name <- new_object$instances[[i]]$data_name

    # if instance name is one to be replaced
    if(data_name %in% datafiles){

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
    new_object$data[!(new_object$data$data %in% datafiles), ]

  # add the new replacement data
  new_object$data <- rbind(trimmed_data, refitted_stansim$data)

  ## update the refitted field with new data
  if (datafiles != "all") {
    new_object$refitted <- c(new_object$refitted, datafiles)
  } else if (datafiles == "all") {
    new_object$refitted <- new_object$raw_call$sim_data
  }

  ####-----------------------------------------------------------------
  ## return the new object
  new_object
}
