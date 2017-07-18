#### collect ####
#' Group stansim objects into a collection
#'
#' @description \code{collect} groups together an arbitrary number of objects
#' with class \code{stansim_simulation} or \code{stansim_collection} into a
#' single \code{stansim_collection} object. Allows for multiple simulations
#' to be stored, saved, analysed and managed in a single object.
#'
#' @param collection_name A name attached to the \code{stansim_collection}
#' object to help identify it. It is strongly recomended that an informative
#' name is assigned.
#' @param object An object of classs \code{stansim_simulation} or
#' \code{stansim_collection}
#' @param ... Any further \code{stansim_simulation} or
#' \code{stansim_collection} objects to be grouped into a single
#' \code{stansim_collection} object.
#' @return An S3 object of class \code{stansim_simulation} recording relevant
#' simulation data.
#'
#' @export
collect <- function(collection_name, object, ...) {

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
  ## COME BACK TO WHEN YOU HAVE WORKING COLLECTION EXAMPLES

  ## -------------------------------------------------
  ## base condition (length of groups = 1)

  if(length(collect_args) == 1){

    # error if only one stansim_simulation is provided
    if(class(object) == "stansim_simulation")
      stop("A single simulation cannot be used to make a collection.")

    # return renamed collection
    object$collection_name <- collection_name
    object

  } else {

    ## -------------------------------------------------
    ## recursive condition (length of groups > 1)

    # extract the first two objects to merge
    two_collect <- collect_args[1:2]

    ## dispatch to the correct joining function given obj types
    # if both simulation
    if (class(two_collect[[1]]) == "stansim_simulation" &
        class(two_collect[[2]]) == "stansim_simulation") {
      two_grouped <- collect_simulations(two_collect[[1]],
                                         two_collect[[2]])
    } else
      # if both collection
      if (class(two_collect[[1]]) == "stansim_collection" &
          class(two_collect[[2]]) == "stansim_collection") {
        two_grouped <- collect_collections(two_collect[[1]],
                                           two_collect[[2]])
      } else
        # if simulation and collection
        if (class(two_collect[[1]]) == "stansim_simulation" &
            class(two_collect[[2]]) == "stansim_collection") {
          two_grouped <- collect_mixed(two_collect[[1]],
                                       two_collect[[2]])
        } else
          # if collection and simulation
          if (class(two_collect[[1]]) == "stansim_collection" &
              class(two_collect[[2]]) == "stansim_simulation") {
            two_grouped <- collect_mixed(two_collect[[2]],
                                         two_collect[[1]])
          }

    ## dispatch the recursive call
    # set up list of args
    named_recurs_args <- list("collection_name" = collection_name,
                                object = two_grouped)

    collect_recurs_args <- c(named_recurs_args, collect_args[-c(1:2)])

    # recursive call
    do.call(collect, collect_recurs_args)
  }
}
