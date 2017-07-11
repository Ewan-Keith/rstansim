#-----------------------------------------------------------------
#### stansim ####
#' Fit a stan model to multiple datasets
#'
#' @description \code{stansim} fits a specified stan model across multiple
#' datasets,  collates, and returns summary information and data for all
#' models as a \code{stansim_single} object. All fitted models will have basic
#' reproducability information recorded; such as parameter inits and seeds,
#' along with parameter estimates, and simulation info such as time and date
#' ran.
#'
#' Raw stan posterior samples are not returned, rather the user specifies the
#' estimates they wish to record (e.g. posterior percentiles, Rhat, etc.)
#' and the parameters for which they wish to record these estimates.
#' All data is collated into a single long-format dataframe for further
#' analysis.
#'
#' By default the function caches completed runs as it progresses, so that
#' progress is not lost in the case of function failure. When the function
#' terminates as expected this cache is removed.
#'
#' @param stan_args A list of function arguments to be used by
#' the internal \code{stan} function when fitting the models.
#' If not specified then the \code{stan} function defaults are used.
#' @param sim_data A list of strings pointing to the location of
#' .rds files containing the simulation data. See the vignette on
#' producing simulation data for details on the formatting of these datasets.
#' @param calc_loo If \code{TRUE} then model fit statsics will be
#' calculated using the \code{loo} package. If \code{TRUE} there must be
#' a valid log_lik quantity specified in the generated quantities
#' section of the provided stan model.
#' @param use_cores Number of cores to use when running in parallel.
#' Each stan model is fitted serially regardless of the number of chains
#' ran as parallelisation across models is more flexible than within.
#' @param parameters A character vector indicating which parameters
#' should have estimates returned and stored from the fitted models.
#' By default all parameters are returned, for non-scalar parameters
#' you cannot select subsets of the parameter (e.g. must request
#' \code{theta} rather than \code{theta[1]}).
#' @param probs A numeric vector of values between 0 and 1. Corresponding
#' quantiles will be estimated and returned for all fitted models.
#' @param estimates A character vector of non-quantile estimates to be
#' returned for each model parameter. Argument must be some subset of the
#' default character vector.
#' @param stan_warnings How warnings returned by individual \code{stan}
#' instances should be handled. \code{"catch"} records all warnings in the
#' returned object alongside other instance level data, \code{"print"} simply
#' prints warnings to the console as the models are fit (default \code{stan}
#' behaviour), and \code{"suppress"} suppressess all warnings without
#' recording them.
#' @param cache If \code{TRUE} then the results for each instance are
#' written to a local, temporary file so that data is not lost should the
#' function not terminate properly. This temporary data is removed upon the
#' model terminating as expected. if \code{FALSE} no data is written and
#' results are only returned upon the correct termination of the whole
#' function. The default value of \code{TRUE} is recommended unless there
#' are relevant write-permission restrictions.
#' @param stansim_seed Set a seed for the \code{stansim} function.
#' @param sim_name A name attached to the \code{stansim_single} object to help
#' identify it. It is strongly recomended that an informative name is
#' assigned, especially if \code{stansim_single} objects are to be combined in
#' to a \code{stansim_collection} object.
#' @return An S3 object of class \code{stansim_single} recording relevant
#' simulation data.
#' @import Rcpp
#'
#' @export
stansim <- function(stan_args = list(), sim_data = NULL, calc_loo = FALSE,
                     use_cores = 1L, parameters = "all",
                     probs = c(.025, .25, .5, .75, .975),
                     estimates = c("mean", "se_mean", "sd", "n_eff", "Rhat"),
                     stan_warnings = "catch", # options print, catch, suppress
                     cache = TRUE,
                     stansim_seed = floor(stats::runif(1, 1, 100000)),
                     sim_name = paste0("Stansim_", start_time)){

  start_time <- Sys.time()
  set.seed(stansim_seed)

  ## -------------------------------------------------
  ## error checks
  # carry out basic input validation
  stansim_checker(sim_data, calc_loo, use_cores,
                   parameters, probs, estimates, stan_args,
                   stan_warnings, cache, stansim_seed,
                   sim_name)

  # coerce sim_name to character if need be
  if (!is.character(sim_name))
    sim_name <- as.character(sim_name)


  ## -------------------------------------------------
  ## set up for parallel running
  cl <- parallel::makeCluster(use_cores)
  doParallel::registerDoParallel(cl)

  # define %dopar% alias
  `%doparal%` <- foreach::`%dopar%`

  # define datafile locally first to avoid R CMD Check Note
  datafile <- NULL

  ##-------------------------------------------------
  ## pre-compile stan model
  compiled_model <- rstan::stan_model(file = stan_args$file)

  # tidy up
  stan_args$file <- NULL
  stan_args$object <- compiled_model

  ##-------------------------------------------------
  ## cache results set up and handling
  if (cache){
    if (dir.exists(".cache/") &
       length(dir(".cache/")) != 0){

      # remove already ran cached data from datafile
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

  ##-------------------------------------------------
  ## run over the datasets

  # parallel loop over datasets, default list combine used
  # note, .export only called to enable mocking of single_sim in testing
  sim_estimates <-
    foreach::foreach(datafile = sim_data,
                     .export = "single_sim") %doparal%
    single_sim(datafile, stan_args, calc_loo,
               parameters, probs, estimates, stan_warnings, cache)

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
    stansim_single(
      sim_name = sim_name,
      stansim_uni_list = sim_estimates,
      start_time = start_time,
      end_time = end_time,
      stansim_seed = stansim_seed
    )

  # if using cache delete folder
  if (cache)
    unlink(".cache", recursive = TRUE)

  return(stansim_obj)
}

### don't forget to add the rerun function
# something like rerun(testout, datafiles_to_rerun)
# and with a default to rerun all, but with a warning
# and request for confirmation if the user tries this
# (warning can be turned off but default is on)



