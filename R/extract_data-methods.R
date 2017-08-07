#-----------------------------------------------------------------
#### extract_data generic method ####
#' Extract data from rstansim objects
#'
#' @description Generic function for extracting data from rstansim objects.
#'   Default arguments will return full data as a dataframe, otherwise rows will
#'   be filtered based on provided arguments.
#'
#' @param object An S3 object of class stansim_simulation or stansim_collection.
#' @param ... Arguments for filtering returned data, see specific methods for
#'   further detail.
#' @return A dataframe containing the specified data.
#'
#' @export
extract_data <- function (object, ...) {
  UseMethod("extract_data", object)
}

#-----------------------------------------------------------------
#### extract_data.stansim_simulation method ####
#' Extract data from a stansim_simulation object
#'
#' @description Applied to an object of type stansim_simulation,
#'   \code{extract_data()} will return the object's simulation data as a
#'   dataframe, subject to the filtering specified by the function arguments.
#'
#' @param object An object of S3 class stansim_simulation.
#' @param datasets Either a character vector containing the names of datasets
#'   (as provided to the original \code{stansim()} call) fitted, or the string
#'   \code{"all"}. The former will only return values for the corresponding
#'   datasets, the latter applies no filtering on datasets
#' @param parameters Either a character vector containing the names of stan
#'   model parameters present in the fitted stan models, or the string
#'   \code{"all"}. The former will only return values for the corresponding
#'   parameters, the latter applies no filtering on parameters. See also the
#'   effect of the \code{param_expand} argument.
#' @param estimates Either a character vector containing the names of parameter
#'   estimates calculated (e.g. c("2.5%", "mean", "97.5%")), or the string
#'   \code{"all"}. The former will only return values for the corresponding
#'   estimates, the latter applies no filtering on estimates
#' @param values Either a function taking a single numeric argument that returns
#'   a Boolean value, or \code{NULL}. The former will only return values for
#'   which the provided function is \code{TRUE}, the latter applies no filtering
#'   on values.
#' @param param_expand If \code{TRUE} then any provided \code{parameters}
#'   arguments, without specified dimension, will be expanded to capture all
#'   dimensions of that parameter. For example, \code{"eta"} becomes
#'   \code{c("eta[1]", "eta[2]", "eta[3]", ...)}. Expansion isn't carried out if
#'   a parameters dimension is specified (e.g. \code{parameters = "eta[1]"}) or
#'   if \code{param_expand = FALSE}.
#' @param ... other arguments not used by this method
#'
#' @return A dataframe containing the specified data.
#'
#' @examples
#' \dontrun{
#' # extract full dataset
#' extract_data(simulation)
#'
#' # extract all parameter means, 2.5% & 97.5% percentiles
#' extract_data(simulation, estimates = c("2.5%", "mean", "97.5%"))
#'
#' # extract all Rhat estimates greater than 1.1
#' extract_data(simulation, estimates = "Rhat",
#'              values = function(x) x > 1.1)
#'
#' # extract all "eta" parameters
#' extract_data(simulation, parameters = "eta")
#'
#' # extract all "eta[1]" parameters
#' extract_data(simulation, parameters = "eta[1]",
#'              param_expand = FALSE)
#'
#' # extract all rows for dataset "data_file-12.rds"
#' extract_data(simulation, datasets = "data_file-12.rds")
#' }
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
      stop("datasets argument must be of type character")

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

#-----------------------------------------------------------------
#### extract_data.stansim_collection method ####
#' Extract data from a stansim_collection object
#'
#' @description Applied to an object of type stansim_collection,
#'   \code{extract_data()} will return the object's simulation data as a
#'   dataframe, subject to the filtering specified by the function arguments.
#'
#' @param object An object of S3 class stansim_collection.
#' @param sim_names Either a character vector containing the names of the
#'   \code{stansim_simulation} objects grouped in the collection, or the string
#'   \code{"all"}. The former will only return values for the corresponding
#'   simulations, the latter applies no filtering on stansim simulations.
#' @param datasets Either a character vector containing the names of datasets
#'   (as provided to the original \code{stansim()} call) fitted, or the string
#'   \code{"all"}. The former will only return values for the corresponding
#'   datasets, the latter applies no filtering on datasets
#' @param parameters Either a character vector containing the names of stan
#'   model parameters present in the fitted stan models, or the string
#'   \code{"all"}. The former will only return values for the corresponding
#'   parameters, the latter applies no filtering on parameters. See also the
#'   effect of the \code{param_expand} argument.
#' @param estimates Either a character vector containing the names of parameter
#'   estimates calculated (e.g. c("2.5%", "mean", "97.5%")), or the string
#'   \code{"all"}. The former will only return values for the corresponding
#'   estimates, the latter applies no filtering on estimates
#' @param values Either a function taking a single numeric argument that returns
#'   a Boolean value, or \code{NULL}. The former will only return values for
#'   which the provided function is \code{TRUE}, the latter applies no filtering
#'   on values.
#' @param param_expand If \code{TRUE} then any provided \code{parameters}
#'   arguments, without specified dimension, will be expanded to capture all
#'   dimensions of that parameter. For example, \code{"eta"} becomes
#'   \code{c("eta[1]", "eta[2]", "eta[3]", ...)}. Expansion isn't carried out if
#'   a parameters dimension is specified (e.g. \code{parameters = "eta[1]"}) or
#'   if \code{param_expand = FALSE}.
#' @param ... other arguments not used by this method
#'
#' @return A dataframe containing the specified data.
#'
#' @examples
#' \dontrun{
#' # extract full dataset
#' extract_data(collection)
#'
#' # extract all parameter means, 2.5% & 97.5% percentiles
#' extract_data(collection, estimates = c("2.5%", "mean", "97.5%"))
#'
#' # extract all Rhat estimates greater than 1.1
#' extract_data(collection, estimates = "Rhat",
#'              values = function(x) x > 1.1)
#'
#' # extract all "eta" parameters
#' extract_data(collection, parameters = "eta")
#'
#' # extract all "eta[1]" parameters
#' extract_data(collection, parameters = "eta[1]",
#'              param_expand = FALSE)
#'
#' # extract all rows for dataset "data_file-12.rds"
#' extract_data(collection, datasets = "data_file-12.rds")
#' # extract all rows for sim_names "simulation1"
#' extract_data(collection, sim_names = "simulation1")
#' }
#'
#' @export
extract_data.stansim_collection <-
  function(object,
           sim_names = "all",
           datasets = "all",
           parameters = "all",
           estimates = "all",
           values = NULL,
           param_expand = TRUE,
           ...) {

    ## carry out basic input validation
    if (!is.function(values) & !is.null(values))
      stop("value argument must be NULL or a function")

    if (!is.character(sim_names))
      stop("sim_names argument must be of type character")

    if (!is.character(datasets))
      stop("datasets argument must be of type character")

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

    ## filter on sim_names
    if ("all" %in% sim_names) {
      if (length(sim_names) > 1) {
        stop(paste("if sim_names argument contains \"any\",",
                   "length(sim_names) must be 1"))
      }
    } else {
      data_extract <- data_extract[data_extract$sim_name %in% sim_names, ]
    }


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
