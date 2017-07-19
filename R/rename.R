#-----------------------------------------------------------------
#### rename stansim_simulation function####
#' Rename a stansim simulation object
#'
#' @description Change the sim_name value of a \code{"stansim_simulation"}
#' object.
#'
#' @param object An object of S3 class stansim_simulation.
#' @param new_name New object name. Must be of type character.
#'
#' @export
rename <- function(object, new_name){

  if(class(object) != "stansim_simulation")
    stop("object must be of class stansim_simulation")

  if(typeof(new_name) != "character")
    stop("new_name must be of type character")

  object$sim_name <- new_name

  object
}
