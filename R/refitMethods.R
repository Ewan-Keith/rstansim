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
#' @description \code{refit}
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

  # check all datafile args are in the stansim_single object
  data_exists <- function(datafile, object_data){
    if(!(datafile %in% object_data))
      stop(paste0(
        "datafiles argument \"",
        datafile,
        "\" not found in provided stansim_object"))
  }
  lapply(datafiles, data_exists, object_data = unique(object$data$data))

  ####------------------------------------------------------------------------------------
  ## if refitting all datafiles
  if(datafiles == "all"){

    stansim(stan_args = stan_args,
            sim_data = unique(object$data$data))
  }

}
