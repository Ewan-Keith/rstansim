#-----------------------------------------------------------------
#### stansim_uni ####
# constructor for internal object that is returned by each parallel
# run of foreach. Only for internal use and will be combined into
# stansim object before being returned.
stansim_uni <- function(fit, data_name, ran_at, long_data, stan_warnings,
                        cache) {

  structure(
    list(
      "data_name" = data_name,
      "ran_at" = ran_at,
      "elapsed_time" = rstan::get_elapsed_time(fit),
      "stan_inits" = fit@inits,
      "stan_args" = fit@stan_args,
      "seed" = rstan::get_seed(fit),
      "out_data" = long_data,
      "model_name" = fit@model_name,
      "model_code" = fit@stanmodel@model_code,
      "warnings" = stan_warnings
    ),
    class = "stansim_uni"
  )

}

#-----------------------------------------------------------------
#### stansim ####
# method for constructing stansim objects by merging together
# stansim_uni objects stored in a list with other global args.
# It is this object that will be returned to the user.

stansim <-
  function(sim_name, stansim_uni_list, start_time,
           end_time, stansim_seed, stan_warnings) {

    ## extract all individual stan instance level data
    # function for cleaning out simstan_uni elements for storage
    ind_run_clean <- function(single_list){
      single_list$model_code <- NULL
      single_list$model_name <- NULL
      single_list$out_data <- NULL

      unclass(single_list)
    }

    # clean simstan_uni list
    ind_runs <- lapply(stansim_uni_list, ind_run_clean)

    ## extract all simulation level data
    # extract model name and code
    model_code <- stansim_uni_list[[1]]$model_code[[1]]
    model_name <- stansim_uni_list[[1]]$model_name

    # extract long data into own list
    data_list <- lapply(stansim_uni_list, function(i) i$out_data)

    # bind datalist into one object
    longer_data <- do.call(rbind, lapply(data_list, "["))

    # create s3 object
    structure(
      list(
        "sim_name" = sim_name,
        "start_time" = start_time,
        "end_time" = end_time,
        "model_name" = model_name,
        "model_code" = model_code,
        "sim_seed" = stansim_seed,
        "instances" = ind_runs,
        "data" = longer_data
      ),
      class = "stansim"
    )
  }


#-----------------------------------------------------------------
#### print.stansim method ####
#' Print a summary for a stansim simulation object
#'
#' @description Print basic  information regarding a stansim simulation object,
#' including simulation title, time/date ran, number of models fitted, and parameters
#' and estimates recorded.
#'
#' @param x An object of S3 class stansim.
#' @param ... other arguments not used by this method
#'
#' @export
print.stansim <- function(x, ...){

  # helper method for clean matrix printing
  print.matrix <- function(m){

    m <- m[rowSums(is.na(m)) != ncol(m), , drop = FALSE]

    m[is.na(m)] <- ""

    utils::write.table(format(m, justify = "left"),
                row.names = F, col.names = F, quote = F)
  }

  # stored parameters
  paramaters <- unique(as.character(x$data$parameter))

  # stored estimates
  estimates <- unique(as.character(x$data$estimate))

  cat("Stan Simulation Title:", x$sim_name, "\n")
  cat("Model Name:", x$model_name, "\n\n")
  cat("Started Running at:", format(x$start_time), "\n")
  cat("Finished Running at:", format(x$end_time), "\n\n")

  cat("Number of Models Fitted:", length(x$instances), "\n\n")

  if (length(paramaters) > 50) {
    cat("Parameter Estimates Recorded:",
        length(paramaters),
        "(first 50 shown)\n")
  } else {
    cat("Parameter Estimates Recorded:",
        length(paramaters),
        "\n")
  }
  print.matrix(matrix(paramaters[1:50], ncol = 5, byrow = TRUE))

  if (length(estimates) > 50) {
    cat("\nEstimates Recorded:",
        length (estimates),
        "(first 50 shown)\n")
  } else {
    cat("\nEstimates Recorded:",
        length (estimates),
        "\n")
  }
  print.matrix(matrix(estimates[1:50], ncol = 5, byrow = TRUE))

}

#-----------------------------------------------------------------
#### extract_data generic method ####
#' Extract data from rstansim objects
#'
#' @description Generic function for extracting data from rstansim objects.
#' Default arguments will return full data as a dataframe, otherwise
#' rows will be filtered based on provided arguments. DONT FORGET TO ADD USAGE CASES FOR DOC
#'
#' @param object An object of S3 class stansim.
#' @param ... Arguments for filtering returned data, see specific methods for further detail.
#'
#' @export
extract_data <- function (object, ...) {
  UseMethod("extract_data", object)
}

#-----------------------------------------------------------------
#### extract_data.stansim method ####
# method to extract data from a stansim object based on string filtering
# of the fields
#' Extract data from a stansim object
#'
#' @description Applied to an object of type stansim, \code{extract_data}
#' will return the objects simulation data as a dataframe, subject to the
#' filtering specified by the function arguments.
#'
#' @param object An object of S3 class stansim.
#' @param datasets Either a character vector containing the names of datasets
#' (as provided to the original \code{stan_sim()} call) fitted, or the string
#' \code{"all"}. The former will only return values for the corresponding
#' datasets, the latter applies no filtering on stansim datasets.
#' @param parameters Either a character vector containing the names of stan
#' model parameters present in the fitted stan model, or the string
#' \code{"all"}. The former will only return values for the corresponding
#' parameters, the latter applies no filtering on parameters. See also
#' the effect of the \code{param_expand} argument.
#' @param estimates Either a character vector containing the names of parameter
#' estimates (as provided to the original \code{stan_sim()} call) calculated,
#' or the string \code{"all"}. The former will only return values for the
#' corresponding estimates, the latter applies no filtering on estimates
#' @param values Either a function taking a single numeric
#' argument that returns a Boolean value, or \code{NULL}. The former will
#' only return values for which the provided function evaluates as
#' \code{TRUE}, the latter applies no filtering on values.
#' @param param_expand If \code{TRUE} then any provided \code{parameters}
#' arguments, without specified dimension, will be exapnded to capture all
#' dimensions of that parameter. For example, \code{"eta"} becomes
#' \code{c("eta[1]", "eta[2]", "eta[3]", ...)}. Expansion isn't carried out
#' if a parameters dimension is specified (e.g. \code{parameters = "eta[1]"})
#' or if \code{param_expand = FALSE}.
#' @param ... other arguments not used by this method
#'
#' @export
extract_data.stansim <- function(object, datasets = "all", parameters = "all",
                         estimates = "all", values = NULL,
                         param_expand = TRUE, ...) {

  ## carry out basic input validation
  if (!is.function(values) & !is.null(values))
    stop("value argument must be NULL or a function")

  if (!is.character(datasets))
    stop("dataset argument must be of type character")

  if (!is.character(parameters))
    stop("parameter argument must be of type character")

  if (!is.character(estimates))
    stop("estimate argument must be of type character")

  ## if param_expand is on extract all dimensions for given param
  if (param_expand){
    all_params <- as.character(unique(object$data$parameter))

    dim_removed_params <- gsub("\\[\\d*\\]$", "", all_params)

    # function to expand matching functions
    param_expansion <- function(single_parameter,
                                all_params, dim_removed_params){

      match_index <- dim_removed_params %in% single_parameter

      all_params[match_index]

    }

    parameters <- unique(c(parameters,
                           unlist(
                             sapply(
                               parameters,
                               param_expansion,
                               dim_removed_params = dim_removed_params,
                               all_params = all_params,
                               USE.NAMES = FALSE
                             )
                           )))

  }

  ## extract data
  data_extract <- object$data


  ## filter on dataset
  if ("all" %in% datasets) {
    if (length(datasets) > 1) {
      stop("if datasets argument contains \"any\", length(datasets) must be 1")
    }
  } else {
    data_extract <- data_extract[data_extract$data %in% datasets, ]
  }

  # filter on parameter
  if ("all" %in% parameters) {
    if (length(parameters) > 1) {
      stop(
        paste("if parameters argument contains \"any\",",
              "length(parameters) must be 1"))
    }
  } else {
    data_extract <- data_extract[data_extract$parameter %in% parameters, ]
  }

  # filter on estimate
  if ("all" %in% estimates) {
    if (length(estimates) > 1) {
      stop(
        paste("if estimates argument contains \"any\",",
              "length(estimates) must be 1"))
    }
  } else {
    data_extract <- data_extract[data_extract$estimate %in% estimates,]
  }

  # filter on value function
  if (!is.null(values))
    data_extract <- data_extract[values(data_extract$value), ]

  # return data
  data_extract

}




