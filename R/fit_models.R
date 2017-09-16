#-----------------------------------------------------------------
#### fit_models ####
#' Fit a stan model to multiple datasets
#'
#' @description \code{fit_models()} fits a stan model across multiple datasets,
#'   collates, and returns summary information and data for all fitted models as
#'   a \code{stansim_simulation} object. All fitted models have basic
#'   reproducibility information recorded; such as parameter inits and seeds,
#'   along with parameter estimates, and simulation information such as time and
#'   date ran.
#'
#'   Raw stan posterior samples are not returned, rather the user specifies the
#'   estimates they wish to record (e.g. posterior percentiles, Rhat, etc.) and
#'   the parameters for which they wish to record these estimates. All data is
#'   collated into a single, \href{http://tidyverse.org/}{tidy} dataframe for
#'   further analysis.
#'
#'   By default the function caches completed runs as it progresses, so that
#'   progress is not lost in the case of function failure. By simply running the
#'   function with the same calls in the same working directory it will pick up
#'   where it left off. When the function terminates as expected this cache is
#'   removed.
#'
#' @param sim_name A name attached to the \code{stansim_simulation} object to
#'   help identify it. It is strongly recommended that an informative name is
#'   assigned, especially if \code{stansim_simulation} objects are to be
#'   combined in to a \code{stansim_collection} object for management of
#'   results.
#' @param sim_data Either an object of class \code{stansim_data} or a vector of
#'   strings pointing to the location of .rds files containing the simulation
#'   data. See the vignette on producing simulation data for details on the
#'   formatting of these datasets.
#' @param stan_args A list of function arguments to be used by the internal
#'   \code{rstan::sampling()} function when fitting the models. If not specified
#'   then the \code{rstan::sampling()} function defaults are used.
#' @param calc_loo If \code{TRUE} then model fit statistics will be calculated
#'   using the \code{loo} package. If \code{TRUE} there must be a valid log_lik
#'   quantity specified in the generated quantities section of the provided stan
#'   model.
#' @param use_cores Number of cores to use when running in parallel. Each stan
#'   model is fitted serially regardless of the number of chains ran, as
#'   parallelisation across models is more flexible than within.
#' @param parameters A character vector indicating which parameters should have
#'   estimates returned and stored from the fitted models. By default all
#'   parameters are returned, for non-scalar parameters you cannot select
#'   subsets of the parameter (e.g. must request \code{theta} rather than
#'   \code{theta[1]}).
#' @param probs A numeric vector of values between 0 and 1. Corresponding
#'   quantiles will be estimated and returned for all fitted models.
#' @param estimates A character vector of non-quantile estimates to be returned
#'   for each model parameter. Argument must be some subset of the default
#'   character vector.
#' @param stan_warnings How warnings returned by individual \code{stan}
#'   instances should be handled. \code{"catch"} records all warnings in the
#'   returned object alongside other instance level data, \code{"print"} simply
#'   prints warnings to the console as the models are fit (default \code{stan}
#'   behaviour), and \code{"suppress"} suppresses all warnings without
#'   recording them.
#' @param cache If \code{TRUE} then the results for each instance are written to
#'   a local, temporary file so that data is not lost should the function not
#'   terminate properly. This temporary data is removed upon the model
#'   terminating as expected. If \code{FALSE} no data is written and results are
#'   only returned upon the correct termination of the whole function. The
#'   default value of \code{TRUE} is recommended unless there are relevant
#'   write-permission restrictions.
#' @param seed Set a seed for the function.
#' @return An S3 object of class \code{stansim_simulation} recording relevant
#'   simulation data.
#'
#' @examples
#' \dontrun{
#' # specify arguments for stan
#' StanArgs <- list(file = '8schools.stan',
#'                  iter = 1000, chains = 4)
#'
#' # get number of cores
#' core_num <- parallel::detectCores()
#'
#' # get the list of data file locations
#' datasets <- dir("data/repo", full.names = TRUE)
#'
#' # fit the model to all datasets using specified stan arguments
#' # store the specified estimates for all parameters
#' simulation <- fit_models(
#'   sim_name = "stansim simulation",
#'   sim_data = datasets,
#'   stan_args = StanArgs,
#'   calc_loo = T,
#'   use_cores = core_num,
#'   probs =  c(.025, .5, .975),
#'   estimates = c("mean", "n_eff", "Rhat")
#' )
#' }
#'
#' @export
fit_models <- function(sim_name = paste0("Stansim_", Sys.time()),
                       sim_data = NULL,
                       stan_args = list(),
                       calc_loo = FALSE,
                       use_cores = 1L,
                       parameters = "all",
                       probs = c(.025, .25, .5, .75, .975),
                       estimates = c("mean", "se_mean", "sd", "n_eff", "Rhat"),
                       stan_warnings = "catch",
                       # options print, catch, suppress
                       cache = TRUE,
                       seed = floor(stats::runif(1, 1, 100000))) {


  # store raw arguments in case of refitting
  raw_call <- as.list(environment())

  start_time <- Sys.time()

  ## -------------------------------------------------
  ## error checks
  # carry out basic input validation
  fit_models_checker(sim_data, calc_loo, use_cores,
                   parameters, probs, estimates, stan_args,
                   stan_warnings, cache, seed,
                   sim_name)

  ## -------------------------------------------------
  ## if stansim_data object used then convert to same format as string vector
  if (class(sim_data) == "stansim_data") {
    # replace sim_data with sim_data$data
    sim_data <- sim_data$datasets
  }

  ##-------------------------------------------------
  ## pre-compile stan model

  file <- stan_args$file

  # if file ends in '.stan' assume it's a file connection
  if (grepl("\\.stan$", file)){
    compiled_model <- rstan::stan_model(file = file)
  } else {
    compiled_model <- rstan::stan_model(model_code = file)
  }

  # tidy up
  stan_args$file <- NULL
  stan_args$object <- compiled_model

  ##-------------------------------------------------
  ## cache results set up and handling
  if (cache){
    if (dir.exists(".cache/") &
       length(dir(".cache/")) != 0){

      # remove already ran cached data from dataset
      cache_files <- dir(".cache", full.names = TRUE)
      clean_cache_files <-
        sub("_cached.rds", "", sub("\\.cache/", "", cache_files))

      cache_match <-
        sub(".rds", "", sub("(.*)/", "", sim_data)) %in% clean_cache_files

      sim_data <- sim_data[!cache_match]

    } else if (!dir.exists(".cache/")) {
      dir.create(".cache/")
    }
  }

  ## -------------------------------------------------
  ## set up for parallel running
  cl <- parallel::makeCluster(use_cores)
  doSNOW::registerDoSNOW(cl)

  # define %dopar% alias
  # tests get angry if this is not defined, not sure why so it stays for now
  `%dopar%` <- foreach::`%dopar%`

  # define %dorng% alias
  `%dorng%` <- doRNG::`%dorng%`

  # define dataset locally first to avoid R CMD Check Note
  dataset <- NULL

  set.seed(seed)

  ##-------------------------------------------------
  ## run over the datasets

  # if datasets is empty (can happen if full .cache results are there)
  # then skip fitting
  if (length(sim_data > 0)) {

    # set up progress bar items
    pb <- utils::txtProgressBar(max = length(sim_data), style = 3)
    progress <- function(n) {
      utils::setTxtProgressBar(pb, n)
    }
    opts <- list(progress = progress)

    # parallel loop over datasets, default list combine used
    # note, .export only called to enable mocking of single_sim in testing
    sim_estimates <-
      foreach::foreach(dataset = sim_data,
                       .export = "single_sim",
                       .options.snow = opts) %dorng%
      single_sim(dataset,
                 stan_args,
                 calc_loo,
                 parameters,
                 probs,
                 estimates,
                 stan_warnings,
                 cache)

    # close out progress bar
    close(pb)

  }



  # de-register the parallel background once done
  parallel::stopCluster(cl)


  ##-------------------------------------------------
  ## if using the cache, use cached results instead
  if (cache)
    sim_estimates <- lapply(dir(".cache/", full.names = TRUE),
                            function (x) readRDS(x))

  ##-------------------------------------------------
  ## collect stansim_uni objects into stansim obj and return
  end_time <- Sys.time()

  stansim_obj <-
    stansim_simulation(
      sim_name = sim_name,
      stansim_uni_list = sim_estimates,
      start_time = start_time,
      end_time = end_time,
      seed = seed,
      raw_call = raw_call
    )

  # if using cache delete folder
  if (cache)
    unlink(".cache", recursive = TRUE)

  return(stansim_obj)
}
