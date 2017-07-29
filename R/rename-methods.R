#-----------------------------------------------------------------
#### rename generic method ####
#' Rename a stansim object
#'
#' @description Generic function for renaming rstansim objects.
#'
#' @param object An S3 object of class stansim_simulation or stansim_collection.
#' @param new_name New object name.
#'
#' @return A stansim S3 object.
#'
#' @export
rename <- function (object, new_name) {
  UseMethod("rename", object)
}

#-----------------------------------------------------------------
#### rename stansim_simulation function####
#' Rename a stansim_simulation object
#'
#' @description Change the sim_name of a \code{"stansim_simulation"} object.
#'
#' @param object An object of S3 class stansim_simulation.
#' @param new_name New object name.
#'
#' @return An S3 object of class \code{stansim_simulation}.
#'
#' @examples
#' \dontrun{
#' # rename a stansim_simulation to "new simulation name"
#' rename(simulation, "new simulation name")
#' }
#'
#' @export
rename.stansim_simulation <- function(object, new_name){

  if(typeof(new_name) != "character")
    stop("new_name must be of type character")

  object$sim_name <- new_name

  object
}

#-----------------------------------------------------------------
#### rename stansim_collection function####
#' Rename a stansim_collection object
#'
#' @description Change the collection_name value of a \
#'   code{"stansim_collection"} object.
#'
#' @param object An object of S3 class stansim_collection.
#' @param new_name New object name. Must be of type character.
#'
#' @return An S3 object of class \code{stansim_collection}.
#'
#' @examples
#' \dontrun{
#' # rename a stansim_collection to "new collection name"
#' rename(collection, "new collection name")
#' }
#'
#' @export
rename.stansim_collection <- function(object, new_name){

  if(typeof(new_name) != "character")
    stop("new_name must be of type character")

  object$collection_name <- new_name

  object
}

