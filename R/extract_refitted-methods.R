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
#' Extract names of refitted datafiles from a stansim_simulation object
#'
#' @description Applied to an object of type stansim_simulation,
#'   \code{extract_refitted()} will return a vector of the names of datafiles
#'   that have been refitted since the initial simulation.
#'
#' @param object An object of S3 class stansim_simulation.
#' @param ... other arguments not used by this method.
#' @return A character vector of the refitted datafile names.
#' @examples
#' \dontrun{
#' # extract names of refitted datafiles
#' extract_refitted(simulation)
#' }
#'
#' @export
extract_refitted.stansim_simulation <- function(object, ...) {
    object$refitted
}

#-----------------------------------------------------------------
#### extract_refitted.stansim_collection method ####
#' Extract details of refitted datafiles from a stansim_collection object
#'
#' @description Applied to an object of type stansim_collection,
#'   \code{extract_refitted()} will return a dataframe of simulation-datafile
#'   pairings that have been refitted since the initial simulation.
#'
#' @param object An object of S3 class stansim_collection.
#' @param sim_names Either a character vector containing the names of the
#'   \code{stansim_simulation} objects grouped in the collection, or the string
#'   \code{"all"}. The former will only return values for the corresponding
#'   simulations, the latter applies no filtering on stansim simulations.
#' @param datafiles Either a character vector containing the names of datafiles
#'   (as provided to the original \code{stansim} call) fitted, or the string
#'   \code{"all"}. The former will only return values for the corresponding
#'   datafiles, the latter applies no filtering on stansim datafiles.
#' @param ... other arguments not used by this method.
#' @return A dataframe with the simulation titles and datafile names of refitted
#'   datfiles.
#'
#' @examples
#' \dontrun{
#' # extract all refitted indicators
#' extract_refitted(collection)
#'
#' # extract only datafiles from "simulation1"
#' extract_refitted(collection, sim_names = "simulation1")
#'
#' # extract only indicators for datafile "data-file_12.rds"
#' extract_refitted(collection, datafiles = "data-file_12.rds")
#'
#' }
#'
#' @export
extract_refitted.stansim_collection <-
  function(object, sim_names = "all", datafiles = "all", ...) {

    ## carry out basic input validation
    if (!is.character(datafiles))
      stop("datafiles argument must be of type character")

    if (!is.character(sim_names))
      stop("sim_names argument must be of type character")

    ## extract refitted
    refit_extract <- object$refitted

    ## filter on dataset
    if ("all" %in% datafiles) {
      if (length(datafiles) > 1) {
        stop(paste("if datafiles argument contains \"any\",",
                   "length(datafiles) must be 1"))
      }
    } else {
      refit_extract <- refit_extract[refit_extract$datafile %in% datafiles, ]
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
