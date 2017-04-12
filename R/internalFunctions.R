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
                       loo, parameters, estimates){


  ##-------------------------------------------------
  ## setup stan data properly
  # read in the data varying from model to model
  #var_data <- suppressMessages(readr::read_csv(datafile))

  # convert csv to a list
  #var_data_list <- as.list(var_data)

  var_data_list <- readRDS(datafile)

  # join the new varying data to the constant data
  stan_args$data <- utils::modifyList(stan_args$data, var_data_list,
                                      keep.null = TRUE)


  ##-------------------------------------------------
  ## fit the model
  fitted_stan <- do.call(rstan::stan, stan_args)


  ##-------------------------------------------------
  ## extract all param values
  extracted <- param_extract(fitted_stan, loo,
                             parameters, estimates)


  ##-------------------------------------------------
  ## return
  return(extracted)
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
stan_sim_checker <- function(sim_data, loo, use_cores,
                             parameters, estimates){


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
}
