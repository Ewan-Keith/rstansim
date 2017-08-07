#-----------------------------------------------------------------
#### extract_refitted generic method ####
#' Extract identifiers of refitted items from rstansim objects
#'
#' @description Generic function for extracting refitted identifiers from
#'   rstansim objects. Default arguments will return full data as a dataframe,
#'   otherwise rows will be filtered based on provided arguments
#'   (stansim_collections only).
#'
#' @param object An S3 object of class stansim_simulation or stansim_collection.
#' @param ... Arguments for filtering returned refitted records, see specific
#'   methods for further detail.
#' @return The specified refitted identifiers, see specific methods for further
#'   detail.
#'
#' @export
extract_refitted <- function (object, ...) {
  UseMethod("extract_refitted", object)
}

#-----------------------------------------------------------------
#### extract_refitted.stansim_simulation method ####
#' Extract names of refitted datasets from a stansim_simulation object
#'
#' @description Applied to an object of type stansim_simulation,
#'   \code{extract_refitted()} will return a vector of the names of datasets
#'   that have been refitted since the initial simulation.
#'
#' @param object An object of S3 class stansim_simulation.
#' @param ... other arguments not used by this method.
#' @return A character vector of the refitted dataset names.
#' @examples
#' \dontrun{
#' # extract names of refitted datasets
#' extract_refitted(simulation)
#' }
#'
#' @export
extract_refitted.stansim_simulation <- function(object, ...) {
    object$refitted
}

#-----------------------------------------------------------------
#### extract_refitted.stansim_collection method ####
#' Extract details of refitted datasets from a stansim_collection object
#'
#' @description Applied to an object of type stansim_collection,
#'   \code{extract_refitted()} will return a dataframe of simulation-dataset
#'   pairings that have been refitted since the initial simulation.
#'
#' @param object An object of S3 class stansim_collection.
#' @param sim_names Either a character vector containing the names of the
#'   \code{stansim_simulation} objects grouped in the collection, or the string
#'   \code{"all"}. The former will only return values for the corresponding
#'   simulations, the latter applies no filtering on stansim simulations.
#' @param datasets Either a character vector containing the names of datasets
#'   (as provided to the original \code{stansim} call) fitted, or the string
#'   \code{"all"}. The former will only return values for the corresponding
#'   datasets, the latter applies no filtering on stansim datasets.
#' @param ... other arguments not used by this method.
#' @return A dataframe with the simulation titles and dataset names of refitted
#'   datasets.
#'
#' @examples
#' \dontrun{
#' # extract all refitted indicators
#' extract_refitted(collection)
#'
#' # extract only datasets from "simulation1"
#' extract_refitted(collection, sim_names = "simulation1")
#'
#' # extract only indicators for dataset "data-file_12.rds"
#' extract_refitted(collection, datasets = "data-file_12.rds")
#'
#' }
#'
#' @export
extract_refitted.stansim_collection <-
  function(object, sim_names = "all", datasets = "all", ...) {

    ## carry out basic input validation
    if (!is.character(datasets))
      stop("datasets argument must be of type character")

    if (!is.character(sim_names))
      stop("sim_names argument must be of type character")

    ## extract refitted
    refit_extract <- object$refitted

    ## filter on dataset
    if ("all" %in% datasets) {
      if (length(datasets) > 1) {
        stop(paste("if datasets argument contains \"any\",",
                   "length(datasets) must be 1"))
      }
    } else {
      refit_extract <- refit_extract[refit_extract$dataset %in% datasets, ]
    }

    ## filter on dataset
    if ("all" %in% sim_names) {
      if (length(sim_names) > 1) {
        stop(paste("if sim_names argument contains \"any\",",
                   "length(sim_names) must be 1"))
      }
    } else {
      refit_extract <- refit_extract[refit_extract$sim_name %in% sim_names, ]
    }

    # retrun refit
    refit_extract

  }
