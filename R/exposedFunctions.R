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
#' @param calc_loo If \code{TRUE} the model will attempt to extract fit
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
stan_sim <- function(stan_args = list(), sim_data = NULL, calc_loo = FALSE,
                     use_cores = 1L, parameters = ".*",
                     estimates = c("2.5%", "50%", "97.5%", "n_eff", "Rhat"),
                     stan_warnings = "catch", # options should be print, catch, parse, suppress
                     cache = TRUE,
                     stansim_seed = floor(runif(1, 1, 100000)),
                     sim_name = paste0("Stansim_", start_time)){

  start_time <- Sys.time()
  set.seed(stansim_seed)

  ##-------------------------------------------------
  ## error checks
  # carry out basic input validation
  stan_sim_checker(sim_data, calc_loo, use_cores,
                   parameters, estimates, stan_args)


  ##-------------------------------------------------
  ## set up for parallel running
  cl <- parallel::makeCluster(use_cores)
  doParallel::registerDoParallel(cl)

  # define %dopar% alias
  `%doparal%` <- foreach::`%dopar%`

  # define datafile locally first to avoid R CMD Check Note
  datafile <- NULL

  ##-------------------------------------------------
  ## cache results set up and handling
  if(cache){
    if(dir.exists(".cache/") &
       length(dir(".cache/", full.names = TRUE)) != 0){

      # remove already ran cached data from datafile
      cache_files <- dir(".cache", full.names = TRUE)
      clean_cache_files <- sub("_cached.rds", "", sub("\\.cache/", "", cache_files))

      cache_match <-
        sub(".rds", "", sub("(.*)/", "", sim_data)) %in% clean_cache_files

      sim_data <- sim_data[!cache_match]

    } else if(!dir.exists(".cache/")) {
      dir.create(".cache/")
    }
  }

  ##-------------------------------------------------
  ## run over the datasets

  # parallel loop over datasets, default list combine used for dev
  sim_estimates <-
    foreach::foreach(datafile = sim_data) %doparal%
    single_sim(datafile, stan_args, calc_loo,
               parameters, estimates, stan_warnings, cache)

  # de-register the parallel background once done
  parallel::stopCluster(cl)

  ##-------------------------------------------------
  ## if using the cache, use cached results instead
  if(cache)
    sim_estimates <- lapply(dir(".cache/", full.names = TRUE), function (x) readRDS(x))

  ##-------------------------------------------------
  ## collect stansim_uni objects into stansim obj and return
  end_time <- Sys.time()

  stansim_obj <- stansim(sim_name = sim_name, stansim_uni_list = sim_estimates,
                         start_time = start_time, end_time = end_time,
                         stansim_seed = stansim_seed,
                         stan_warnings = stan_warnings)

  # if using cache delete folder
  if(cache)
    unlink(".cache/", recursive = TRUE)

  return(stansim_obj)
}
