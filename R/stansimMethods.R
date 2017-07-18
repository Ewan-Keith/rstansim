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
print.stansim_simulation <- function(x, ...){

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

  cat(paste0("Stan Simulation Title: ", x$sim_name, "\n"))
  cat(paste0("Model Name: ", x$model_name, "\n\n"))
  cat(paste0("Started Running at: ", format(x$start_time), "\n"))
  cat(paste0("Finished Running at: ", format(x$end_time), "\n\n"))

  cat(paste0("Number of Models Fitted: ", length(x$instances), "\n\n"))

  if (length(paramaters) > 50) {
    cat(paste0("Parameters Recorded: ",
        length(paramaters),
        " (first 50 shown)\n"))
  } else {
    cat(paste0("Parameters Recorded: ",
        length(paramaters),
        "\n"))
  }
  print.matrix(matrix(paramaters[1:50], ncol = 5, byrow = TRUE))

  if (length(estimates) > 50) {
    cat(paste0("\nEstimates Recorded: ",
        length (estimates),
        " (first 50 shown)\n"))
  } else {
    cat(paste0("\nEstimates Recorded: ",
        length (estimates),
        "\n"))
  }
  print.matrix(matrix(estimates[1:50], ncol = 5, byrow = TRUE))

  if (length(x$refitted) > 5) {
    cat(paste0("\nDatasets Refitted: ",
               length(x$refitted),
               " (first 5 shown)\n"))
  } else {
    cat(paste0("\nDatasets Refitted: ",
               length(x$refitted),
               "\n"))
  }

  if (length(x$refitted) > 0) {
    for (i in 1:min(c(length(x$refitted), 5))) {
      cat(x$refitted[i])
    }
  }
}

#-----------------------------------------------------------------
#### extract_data generic method ####
#' Extract data from rstansim objects
#'
#' @description Generic function for extracting data from rstansim objects.
#' Default arguments will return full data as a dataframe, otherwise
#' rows will be filtered based on provided arguments.
#'
#' @param object An object of S3 class stansim_simulation.
#' @param ... Arguments for filtering returned data, see specific methods for further detail.
#'
#' @export
extract_data <- function (object, ...) {
  UseMethod("extract_data", object)
}

#-----------------------------------------------------------------
#### extract_data.stansim_simulation method ####
# method to extract data from a stansim object based on string filtering
# of the fields
#' Extract data from a stansim_simulation object
#'
#' @description Applied to an object of type stansim_simulation,
#' \code{extract_data} will return the objects simulation data as a
#' dataframe, subject to the filtering specified by the function arguments.
#'
#' @param object An object of S3 class stansim_simulation.
#' @param datasets Either a character vector containing the names of datasets
#' (as provided to the original \code{stansim} call) fitted, or the string
#' \code{"all"}. The former will only return values for the corresponding
#' datasets, the latter applies no filtering on stansim datasets.
#' @param parameters Either a character vector containing the names of stan
#' model parameters present in the fitted stan model, or the string
#' \code{"all"}. The former will only return values for the corresponding
#' parameters, the latter applies no filtering on parameters. See also
#' the effect of the \code{param_expand} argument.
#' @param estimates Either a character vector containing the names of parameter
#' estimates (as provided to the original \code{stansim} call) calculated,
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
extract_data.stansim_simulation <-
  function(object,
           datasets = "all",
           parameters = "all",
           estimates = "all",
           values = NULL,
           param_expand = TRUE,
           ...) {
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
    if (param_expand) {
      all_params <- as.character(unique(object$data$parameter))

      dim_removed_params <- gsub("\\[\\d*\\]$", "", all_params)

      # function to expand matching functions
      param_expansion <- function(single_parameter,
                                  all_params,
                                  dim_removed_params) {
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
        stop(paste("if datasets argument contains \"any\",",
                   "length(datasets) must be 1"))
      }
    } else {
      data_extract <- data_extract[data_extract$data %in% datasets, ]
    }

    # filter on parameter
    if ("all" %in% parameters) {
      if (length(parameters) > 1) {
        stop(
          paste(
            "if parameters argument contains \"any\",",
            "length(parameters) must be 1"
          )
        )
      }
    } else {
      data_extract <-
        data_extract[data_extract$parameter %in% parameters, ]
    }

    # filter on estimate
    if ("all" %in% estimates) {
      if (length(estimates) > 1) {
        stop(paste(
          "if estimates argument contains \"any\",",
          "length(estimates) must be 1"
        ))
      }
    } else {
      data_extract <- data_extract[data_extract$estimate %in% estimates, ]
    }

    # filter on value function
    if (!is.null(values))
      data_extract <- data_extract[values(data_extract$value), ]

    # return data
    data_extract

  }
