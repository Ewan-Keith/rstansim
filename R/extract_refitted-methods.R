#-----------------------------------------------------------------
#### extract_refitted generic method ####
#' Extract identifiers of refitted items from rstansim objects
#'
#' @description Generic function for extracting refitted identifiers from
#' rstansim objects.
#'
#' @param object An object of S3 class stansim_simulation.
#'
#' @export
extract_refitted <- function (object) {
  UseMethod("extract_refitted", object)
}

#-----------------------------------------------------------------
#### extract_refitted.stansim_simulation method ####
#' Extract names of refitted datasets from a stansim_simulation object
#'
#' @description Applied to an object of type stansim_simulation,
#' \code{extract_refitted} will return a vector of names of datasets
#' that have been refitted since the initial simulation.
#'
#' @param object An object of S3 class stansim_simulation.
#'
#' @export
extract_refitted.stansim_simulation <- function(object) {

    ## extract refitted
    object$refitted

  }
