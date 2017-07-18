#-----------------------------------------------------------------
#### single_sim ####
# single_sim is the primary 'under the hood' function. It calls
# on the other internal functions to safely fit the stan model,
# extract the correct parameters and format them into a suitable
# row format. It then returns them. single_sim is only intended
# to be called by stan_sim using the foreach package. One instance
# of single_sim will be ran for every dataset provided to stan_sim
# with the sim_data parameter.
single_sim <- function(datafile, stan_args,
                       calc_loo, parameters, probs,
                       estimates, stan_warnings, cache){

  # garbage collect on start
  gc()

  ##-------------------------------------------------
  ## setup stan data properly
  # read in the data varying from model to model
  stan_args$data <- readRDS(datafile)

  # fix stan's use of cores to 1
  stan_args$cores <- 1L

  ##-------------------------------------------------
  ## set up call handler functions

  # warning handler to catch warnings in fitting
  # parsing is done in stansim constructor
  if (stan_warnings == "catch"){
    w_handler <- function(w) {
      my_warnings <<- c(my_warnings, list(w))
      invokeRestart("muffleWarning")
    }
  } else if (stan_warnings == "suppress"){
    w_handler <- function(w) {
      invokeRestart("muffleWarning")
    }
  } else if (stan_warnings == "print"){
    w_handler <- function(w) {
      print(w)
      invokeRestart("muffleWarning")
      }
  }

  # init warning store
  my_warnings <- NULL


  ##-------------------------------------------------
  ## fit the model
  start_time <- Sys.time()

  fitted_stan <- withCallingHandlers(do.call(rstan::sampling, stan_args),
                                     warning = w_handler)

  # garbage collect after model fitting
  rm(stan_args, w_handler)
  gc()

  ##-------------------------------------------------
  ## extract all param values
  extracted_data <- param_extract(fitted_stan, calc_loo,
                                  parameters, probs, estimates,
                                  data = datafile)

  # garbage collect after para, extraction
  rm(calc_loo, parameters, probs, estimates)
  gc()

  ##-------------------------------------------------
  ## package into internal stansim_uni object
  single_out <- stansim_uni(
    fit = fitted_stan,
    data_name = datafile,
    ran_at = start_time,
    long_data = extracted_data,
    stan_warnings = my_warnings
  )

  ##-------------------------------------------------
  ## if cache is true then write to .sim_cache folder, cleaning datafile first
  if (cache){
    cleaned_datafile <- sub(".rds", "", sub("(.*)/", "", datafile))
    saveRDS(single_out, paste0(".cache/", cleaned_datafile, "_cached.rds"))
    }

  ##-------------------------------------------------
  ## return object
  single_out

}


#-----------------------------------------------------------------
#### param_extract ####
# param_extract takes a fitted stan mode and returns a flattened
# set of summary parameter estimates. The parameters and estimates
# to report are user specified. Also specified is whether or not
# the user wishes to extract loo statistics for the model (requires
# a valid log_lik quantity in the stan model). If true then the
# output of loo::loo are returned in the output row.
param_extract <- function(fitted_stan, calc_loo, parameters,
                          probs, estimates, data){

  ##-------------------------------------------------
  ## extract specified parameter quantiles and all string estimates

  if ("all" %in% parameters){
    parameters <- fitted_stan@model_pars
  } else if (sum(parameters %in% fitted_stan@model_pars) == 0){
    stop(paste(
      "None of the provided parameters were identified in the model,",
         "check your stan code and arguments."))
  } else if (sum(parameters %in% fitted_stan@model_pars) <
             length(parameters)){
    stop(paste0("Following parameter(s) could not be found in the model, ",
               "check your stan code and arguments.\n[",
               paste(parameters[!(parameters %in% fitted_stan@model_pars)],
                     collapse = ", "), "]"
               )
         )
  }

  all_param_estimates <- rstan::summary(fitted_stan,
                                        probs = probs,
                                        pars = parameters)$summary

  ##-------------------------------------------------
  ## remove any non-requested string estimates
  estimates_vect <- unlist(estimates)

  estimates_to_drop <- setdiff(c("Rhat", "n_eff", "mean",
                                 "se_mean", "sd"), estimates_vect)

  output <-
    all_param_estimates[, !colnames(all_param_estimates) %in%
                          estimates_to_drop]

  ##-------------------------------------------------
  ## reshape to long format

  colnames(output) <- paste0("value-", colnames(output))

  wide_output <- data.frame(parameters = row.names(output),
                            output, row.names = NULL, check.names = F)

  long_output <- stats::reshape(
    wide_output,
    direction = "long",
    sep = "-",
    varying = 2:ncol(wide_output),
    idvar = "parameters"
  )

  rownames(long_output) <- NULL
  colnames(long_output)[1] <- "parameter"
  colnames(long_output)[2] <- "estimate"


  ##-------------------------------------------------
  ## if calc_loo, then calculate loo values and append
  if (calc_loo){
    log_lik_1 <- loo::extract_log_lik(fitted_stan)
    loo_1 <- loo::loo(log_lik_1, cores = 1)

    loo_params <-
      c("elpd_loo",
        "se_elpd_loo",
        "p_loo",
        "se_p_loo",
        "looic",
        "se_looic")

    loo_value <- as.vector(loo_1[loo_params], "numeric")

    loo_output <- data.frame(
      "parameter" = c(rep("elpd_loo", 2),
                     rep("p_loo", 2),
                     rep("looic", 2)),
      "estimate" = c("estimate", "se"),
      "value" = loo_value
    )

    long_output <- rbind(long_output, loo_output)
  }

  ##-------------------------------------------------
  ## add an indicator for dataset and sort rows
  indicator_data <- cbind("datafile" = data, long_output)

  # order output
  ordered_data <-
    indicator_data[with(indicator_data, order(parameter, estimate)), ]

  ##-------------------------------------------------
  ## remove rownames and format columns
  rownames(ordered_data) <- NULL

  factor_index <- sapply(ordered_data, is.factor)

  ordered_data[factor_index] <-
    lapply(ordered_data[factor_index], as.character)

  # if present drop 'value' estimate, appears if probs = NA
  # then return
  ordered_data[ordered_data$estimate != "value", ]

}


#-----------------------------------------------------------------
#### stansim_checker ####
# stansim_checker runs several tests on input to stan_sim() to
# check for input validity early in the function. Only basic type
# checks are made.
stansim_checker <- function(sim_data, calc_loo, use_cores,
                             parameters, probs, estimates, stan_args,
                             stan_warnings, cache, stansim_seed,
                             sim_name){


  ##-------------------------------------------------
  ## specify helper 'is positive integer' function
  is_pos_int <- function(val){
    if (is.numeric(val)){
      val %% 1 == 0 && val > 0
    } else FALSE
  }


  ##-------------------------------------------------
  ## stanSim parameter checks
  # sim data must be specified
  if (is.null(sim_data))
    stop("sim_data must be specified")

  if (length(sim_data) < 1)
    stop("sim_data must have length > 0")

  # all sim_data locations must exist
  # sim_data must be .rds file
  sim_data_checks <- function(each_sim_data){
    if (!file.exists(each_sim_data))
      stop(paste0("sim_data arg \"",
                  each_sim_data,
                  "\" could not be found"))

    if (tolower(
      substr(each_sim_data,
             nchar(each_sim_data) - 3 + 1,
             nchar(each_sim_data))
      ) != "rds") {
      stop(paste0("sim_data arg \"",
                  each_sim_data,
                  "\" is not a .rds file"))
    }
  }

  lapply(sim_data, sim_data_checks)

  # calc_loo must be Boolean
  if (!is.logical(calc_loo))
    stop("calc_loo must be of type logical")

  # calc_loo cant be missing
  if (is.na(calc_loo))
    stop("calc_loo cannot be NA")

  # use_cores must be a positive integer
  if (!is_pos_int(use_cores))
    stop("use_cores must be a positive integer")

  # stan args must be a list
  if (!is.list(stan_args))
    stop("stan_args must be of type list")

  # parameters must be character
  if (!is.character(parameters))
    stop("parameters must be of type character")

  # if one parameter is all length must be 1
  if ("all" %in% parameters) {
    if (length(parameters) > 1) {
      stop(
        paste0("if parameters argument contains \"any\", ",
               "length(parameters) must be 1")
        )
    }
  }

  # stan_args$cores will just be overwritten
  # if specified warn user
  if ("cores" %in% names(stan_args))
    warning(paste("stan_sim is parallel across stan instances,",
                   "not within. stan_arg$cores is fixed to 1"))

  # stan_warnings must be one of print, catch, suppress
  if (!(stan_warnings %in% c("print", "catch", "suppress")))
    stop(
      "stan_warnings must be one of \"print\", \"catch\", or \"suppress\""
      )


  # stan_args$data must not be provided
  if (!is.null(stan_args$data))
    stop(
      "stan_args$data cannot be directly specified, sim_data should be used")

  # stan_args$pars must not be provided
  if (!is.null(stan_args$pars))
    stop(
      "stan_args$pars cannot be directly specified, parameters should be used")

  # cache must be Boolean
  if (!is.logical(cache))
    stop("cache must be of type logical")

  # sample_file must be NULL
  if (!is.null(stan_args$sample_file))
    stop("stan_args$sample_file must be NULL to prevent write conflicts")

  # diagnostic_file must be NULL
  if (!is.null(stan_args$diagnostic_file))
    stop("stan_args$diagnostic_file must be NULL to prevent write conflicts")

  # warn if sim_name is overwritten
  if (!is.character(sim_name))
    warning("sim_name corced to character")

  # probs must be numeric between 0 and 1
  probs_validate <- function(prob) {
    if (is.numeric(prob) & prob >= 0 & prob <= 1) {
      NULL
    } else {
      stop("all probs arguments must be numbers between 0 and 1")
    }
  }
  sapply(probs, probs_validate)


  ##-------------------------------------------------
  ## check that all estimates values are valid
  estimate_validate <- function(estimate) {
    # valid if numeric between 0 and 1
    if (estimate %in%
        c("Rhat", "n_eff", "mean",
          "se_mean", "sd")) {
      NULL
    } else {
      stop(
        paste("estimate arguments must be one of \"Rhat\",",
              "\"n_eff\", \"mean\", \"se_mean\", or \"sd\".")
      )
    }
  }
  sapply(estimates, estimate_validate)

}
