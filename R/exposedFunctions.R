#-----------------------------------------------------------------
#### stan_sim ####
#' Fits a stan model to multiple datasets and returns estimated values
#'
#' @description \code{stan_sim} fits a specified stan model across multiple
#' datasets and collates and returns the estimated parameters for all models.
#' It fits the specified model to all datasets indicated and returns the
#' specified results for each model. It provides an optional set of controls
#' in case any fitted models do not appear to have converged, in which case
#' model fit will be re-attempted or a string indicating lack of fit will be
#' returned for that model. The function allows for stan instances to be
#' ran in parallel, reducing run the time taken on multicore machines.
#'
#' @param stan_args A list of model parameters to be taken by
#' the \code{stan()} function. If not specified then the \code{stan()}
#' function defaults are used.
#' @param sim_data A list of strings pointing to the .csv files that
#' contain the simulation data. Only data that varies from model to
#' model must be specified here, data common to all models can be
#' specified as part of \code{stan_args}.
#' @param loo If \code{TRUE} the model will attempt to extract fit
#' statistics using the loo package. If \code{TRUE} there must be
#' a valid log_lik quantity specified in the generated quantities
#' section of the stan model.
#' @param use_cores Number of cores to use when running in parallel.
#' Each stan model is fitted serially regardless of the number of chains
#' ran. However as the number of available cores increases more models
#' can be fit in parallel
#' @param max_failures The maximum number of times that each model
#' will attempt refitting when its greatest R-hat statstics
#' is > \code{max_rhat}. If this number is exceeded a string will be returned
#' indicating convergence failure, rather than model estimates.
#' @param max_rhat A maximum cut-off for crudely assessing convergence
#' across fitted models. If any parameters in the fitted model have an
#' R-hat >  \code{max_rhat} then the model will either attempt re-fitting
#' or will return a string indicating lack of convergence (depending on
#' the number of fitting attempts made and the value of \code{max_failures}).
#' Setting this to a large value (e.g. 1000) will
#' efectively turn off the convergence assessment functionality.
#' @param parameters A set of characters indicating which parameters
#' should have estimates returned from the fitted models. Regular
#' expressions are used to extract parameters, so care has to be taken
#' with similarly named parameters (e.g. 'eta' and 'theta').
#' @param estimates The estimates that should be returned for each
#' model parameter. Currently only supports the standard 'summary'
#' parameters c('mean', 'se_mean',  'sd', '2.5%', '25%',  '50%',
#' '75%', '97.5%', 'n_eff', 'Rhat').
#' @return A dataframe of estimated values across all datasets.
#'
#' @export
stan_sim <- function(stan_args = list(), sim_data = NULL, loo = FALSE,
                     use_cores = 1L, parameters = ".*",
                     estimates = c("2.5%", "50%", "97.5%")){


  ##-------------------------------------------------
  ## stan input must be a list
  if (!is.list(stan_args))
    stop("stan_args must be a list of stan parameters")


  ##-------------------------------------------------
  ## error checks
  # carry out basic input validation
  stan_sim_checker(sim_data, loo, use_cores,
                   parameters, estimates)


  ##-------------------------------------------------
  ## set up for parallel running and run over the datasets

  cl <- parallel::makeCluster(use_cores)
  doParallel::registerDoParallel(cl)

  # define %dopar% alias
  `%doparal%` <- foreach::`%dopar%`

  # define datafile locally first to avoid R CMD Check Note
  datafile <- NULL

  # parallel loop over datasets, default list combine used for dev
  sim_estimates <-
    foreach::foreach(datafile = sim_data,
                     .combine = "rbind") %doparal%
    single_sim(datafile, stan_args, loo,
               parameters, estimates)

  # de-register the parallel background once done
  parallel::stopCluster(cl)

  return(sim_estimates)
}
