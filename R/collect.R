#### collect_simulations ####
#' Group stansim objects into a collection
#'
#' @description \code{collect_simulations()} groups together an arbitrary number
#'   of objects with class \code{stansim_simulation} or
#'   \code{stansim_collection} into a single \code{stansim_collection} object.
#'   Allows for multiple simulations to be stored, saved, analysed and managed
#'   in a single object.
#'
#' @param collection_name A name attached to the \code{stansim_collection}
#'   object to help identify it. It is strongly recommended that an informative
#'   and unique name is assigned.
#' @param object An object of class \code{stansim_simulation} or
#'   \code{stansim_collection}. Must be provided.
#' @param ... Any further \code{stansim_simulation} or \code{stansim_collection}
#'   objects to be grouped into a single \code{stansim_collection} object.
#' @return An S3 object of class \code{stansim_simulation} recording relevant
#'   simulation data.
#' @examples
#' \dontrun{
#' # group together stansim_simulation objects
#' collection_basic <- collect_simulations("Linear Regression Study", simulation1,
#'                             simulation2)
#'
#' # group together stansim_simulations and stansim_collections
#' collection_extended <- collect_simulations("Extended Lin Reg Study", collection_basic,
#'                                simulation3)
#'
#' # group together multiple stansim_collections
#' merged_collections <- collect_simulations("merged collections", collection_extended,
#'                               collection_additional)
#' }
#'
#' @export
collect_simulations <- function(collection_name, object, ...) {

  ## -------------------------------------------------
  ## input tests
  if (typeof(collection_name) != "character")
    stop("collection_name must be of type character")

  ## extract all grouping objects
  all_args <- c(as.list(environment()), list(...))
  collect_args <- all_args[-which(names(all_args) == "collection_name")]

  # all group objects must have correct classes
  class_check <- function(obj) {
    if (!(class(obj) %in% c("stansim_simulation", "stansim_collection")))
      stop(
        paste(
          "all arguments except collection_name must be",
          "of class \"stansim_simulation\" or \"stansim_collect\""
        ))
  }
  lapply(collect_args, class_check)

  # no two group object should have the same sim_name or collection_name
  get_name <- function(x){
    if (class(x) == "stansim_simulation") return(x$sim_name)

    if (class(x) == "stansim_collection") return(x$collection_name)
  }
  names <- unlist(lapply(collect_args, get_name))

  if (anyDuplicated(names) != 0)
    stop(paste("The collection_name and simulation_name values of all",
               "arguments must be unique"))

  ## -------------------------------------------------
  ## base condition (length of groups = 1)
  if (length(collect_args) == 1){

    # if only one stansim_simulation is provided
    if (class(object) == "stansim_simulation"){
      # if single simulation, convert to collection
      collect_single_sim_internal(collection_name,
                                  object)

    } else {
      # return renamed collection if one collection provided
      object$collection_name <- collection_name
      object
    }

  } else {

    ## -------------------------------------------------
    ## recursive condition (length of groups > 1)

    # extract the first two objects to merge
    two_collect <- collect_args[1:2]

    ## dispatch to the correct joining function given obj types
    # if both simulation
    if (class(two_collect[[1]]) == "stansim_simulation" &
        class(two_collect[[2]]) == "stansim_simulation") {
      two_grouped <- collect_simulations_internal(collection_name,
                                         two_collect[[1]],
                                         two_collect[[2]])
    } else
      # if both collection
      if (class(two_collect[[1]]) == "stansim_collection" &
          class(two_collect[[2]]) == "stansim_collection") {
        two_grouped <- collect_collections(collection_name,
                                           two_collect[[1]],
                                           two_collect[[2]])
      } else
        # if simulation and collection
        if (class(two_collect[[1]]) == "stansim_simulation" &
            class(two_collect[[2]]) == "stansim_collection") {
          two_grouped <- collect_mixed(collection_name,
                                       two_collect[[1]],
                                       two_collect[[2]])
        } else
          # if collection and simulation
          if (class(two_collect[[1]]) == "stansim_collection" &
              class(two_collect[[2]]) == "stansim_simulation") {
            two_grouped <- collect_mixed(collection_name,
                                         two_collect[[2]],
                                         two_collect[[1]])
          }

    ## dispatch the recursive call
    # set up list of args
    named_recurs_args <- list("collection_name" = collection_name,
                                object = two_grouped)

    collect_recurs_args <- c(named_recurs_args, collect_args[-c(1:2)])

    # recursive call
    do.call(collect_simulations, collect_recurs_args)
  }
}
