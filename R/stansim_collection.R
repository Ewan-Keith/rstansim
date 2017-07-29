#-----------------------------------------------------------------
#### stansim_collection ####
#' Construct an S3 object of type stansim_collection
#'
#' @description A constructor function for creating S3 objects of class
#'   \code{stansim_collection}. \code{stansim_collection} objects are the
#'   preferred means of managing and storing the results of multiple stan
#'   simulations ran using \code{stansim()}.
#'
#' @param collection_name A name attached to the \code{stansim_collection}
#'   object to help identify it. It is strongly recomended that an informative
#'   name is assigned.
#' @param data The tidy dataframe containing data from all grouped simulations.
#' @param refitted A dataframe containing a row for every refitted datafile,
#'   listing the simulation to which it belonged and the datafile name.
#' @param simulations A list of all simulation data other than estimated values
#'   and the list of refitted data.
#'
#' @return An S3 object of class \code{stansim_collection}.
#'
#' @export
stansim_collection <- function(collection_name,
                               data,
                               refitted,
                               simulations){

  # create s3 object
  structure(
    list(
      "collection_name" = collection_name,
      "data" = data,
      "refitted" = refitted,
      "simulations" = simulations
    ),
    class = "stansim_collection"
  )
}
