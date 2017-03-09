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
                       loo, max_failures, max_rhat,
                       parameters, estimates){


  ##-------------------------------------------------
  ## setup stan data properly
  # read in the data varying from model to model
  var_data <- suppressMessages(readr::read_csv(datafile))

  # convert csv to a list
  var_data_list <- as.list(var_data)

  # join the new varying data to the constant data
  stan_args$data <- utils::modifyList(stan_args$data, var_data_list,
                                      keep.null = TRUE)


  ##-------------------------------------------------
  ## fit the model in a convergence safe manner
  fitted_stan <- safe_fit(max_rhat,
                          max_failures,
                          stan_args)


  ##-------------------------------------------------
  ## extract all param values
  extracted <- param_extract(fitted_stan, loo,
                             parameters, estimates)


  ##-------------------------------------------------
  ## return
  return(extracted)
}


#-----------------------------------------------------------------
#### safe_fit ####
# safe_fit places a wrapper around rstan's stan function
# allowing for a maximum R-hat convergence criteria to be specified.
# If after fitting this criteria is not met by all parameters the
# function will attempt to refit the model. A maximum number of attempts
# is specified by the user (default = 5). If the R-hat criteria is met
# before this limit is reached then the standard stan fit object,
# along with the number of attempts made before convergence was achieved,
# will be returned. If convergence is not achieved in this number of
# attempts, then a string "convergence failed for X attempts" is
# returned, where X is the maximum number of permitted failures.
safe_fit <- function(max_rhat, max_failures, stan_args) {


  ##-------------------------------------------------
  ## recursive function loops over each time there's
  ## a fit failure until it hits the maximum failure
  safe_fit_recurs <- function(max_rhat, max_failures,
                              stan_args, count = 1) {


    ##-------------------------------------------------
    ## if the attempt is past the maximum amount return string
    if (count > max_failures)
      return(paste("convergence failed for",
                   max_failures,
                   "attempts"))


    ##-------------------------------------------------
    ## fit the stan model with specified arguments
    fit <- do.call(rstan::stan, stan_args)


    ##-------------------------------------------------
    ## extract greatest Rhat
    greatest_rhat <- max(rstan::summary(fit)$summary[, "Rhat"])


    ##-------------------------------------------------
    ## if greatest Rhat acceptable return model with
    ## attempt count else try again with count + 1
    if (greatest_rhat < max_rhat)
      return(c(fit, "attempts" = count))
    else
      safe_fit_recurs(max_rhat, max_failures,
                      stan_args, count + 1)
  }


  ##-------------------------------------------------
  ## call safe_fit_recurs helper function
  safe_fit_recurs(max_rhat, max_failures, stan_args)
}


#-----------------------------------------------------------------
#### param_extract ####
# param_extract takes a fitted stan mode and returns a flattened
# set of summary parameter estimates. The parameters and estimates
# to report are user specified. Also specified is whether or not
# the user wishes to extract loo statistics for the model (requires
# a valid log_lik quantity in the stan model). If true then the
# output of loo::loo are returned in the output row.
param_extract <- function(fitted_stan, loo,
                          parameters, estimates){


  ##-------------------------------------------------
  ## prep data
  # split out attempts and stanfit
  attempts <- setNames(fitted_stan[[2]], "fit_attempts")
  fitted_stan <- fitted_stan[[1]]

  # for the first version, just allow standard summary values
  # rather than custom percentiles, etc. To do later.


  ##-------------------------------------------------
  ## fit model
  # extract values and estimates
  model_summary <- rstan::summary(fitted_stan)$summary


  ##-------------------------------------------------
  ## extract parameters
  # at the momemnt user will have to use exact regex
  # e.g. 'eta' below to avoid also getting 'theta'
  #params <- c("mu", "^eta")
  regex_parameters <- paste(parameters, sep = "", collapse = "|")
  rows <- row.names(model_summary)

  parameter_index <- grepl(regex_parameters, rows)

  selected_parameters <- model_summary[parameter_index, ]

  ## extract all select estimates
  output <- selected_parameters[, estimates]


  ##-------------------------------------------------
  ## flatten parameters to named row
  rnames <- rownames(output)
  cnames <- colnames(output)
  paste_u <- function(x, y) paste(x, y, sep = "_")
  return_names <- c(t(outer(rnames, cnames, paste_u)))

  return_vals <- as.vector(t(output))

  return_joined <- setNames(return_vals, return_names)

  return_joined <- c(return_joined, attempts)


  ##-------------------------------------------------
  ## if loo, then calculate loo values and append
  if (loo){
    log_lik_1 <- loo::extract_log_lik(fitted_stan)
    loo_1 <- loo::loo(log_lik_1)

    loo_params <-
      c("elpd_loo",
        "se_elpd_loo",
        "p_loo",
        "se_p_loo",
        "looic",
        "se_looic")

    loo_vals <- as.vector(loo_1[loo_params], "numeric")

    loo_output <- setNames(loo_vals, loo_params)

    return_joined <- c(return_joined, loo_output)
  }


  ##-------------------------------------------------
  ## return
  return(return_joined)
}


#-----------------------------------------------------------------
#### stan_sim_checker ####
# stan_sim_checker runs several tests on input to stan_sim() to
# check for input validity early in the function. Only basic type
# checks are made.
stan_sim_checker <- function(sim_data, loo, use_cores, max_failures,
                             max_rhat, parameters, estimates){


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

  # loo must be Boolean
  if (!is.logical(loo))
    stop("loo must be of type logical")

  # use_cores must be a positive integer
  if (!is_pos_int(use_cores))
    stop("use_cores must be a positive integer")

  # max_failures must be a positive integer
  if (!is_pos_int(max_failures))
    stop("max_failures must be a positive integer")

  # max_rhat must be numeric
  if (!is.numeric(max_rhat))
    stop("max_rhat must be numeric")

  # max_rhat must be >= 1
  if (max_rhat < 1)
    stop("max_rhat must be >= 1")
}
