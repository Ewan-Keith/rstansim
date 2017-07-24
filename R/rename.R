#-----------------------------------------------------------------
#### rename generic method ####
#' Rename a stansim object
#'
#' @description Generic function for refitting specified datafiles in
#' rstansim objects. By default refits all datafiles, following a user
#' prompt given the potential computational cost (this can be turned off).
#' Otherwise only datafile names provided will be refit.
#'
#' @param object An object of S3 class stansim_simulation.
#' @param new_name New object name. Must be of type character.
#'
#' @export
rename <- function (object, new_name) {
  UseMethod("rename", object)
}

#-----------------------------------------------------------------
#### rename stansim_simulation function####
#' Rename a stansim_simulation object
#'
#' @description Change the sim_name value of a \code{"stansim_simulation"}
#' object.
#'
#' @param object An object of S3 class stansim_simulation.
#' @param new_name New object name. Must be of type character.
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
#' code{"stansim_collection"} object.
#'
#' @param object An object of S3 class stansim_collection.
#' @param new_name New object name. Must be of type character.
#'
#' @export
rename.stansim_collection <- function(object, new_name){

  if(typeof(new_name) != "character")
    stop("new_name must be of type character")

  object$collection_name <- new_name

  object
}

