#-----------------------------------------------------------------
#### print.stansim_simulation method ####
#' Print a summary for a stansim_simulation object
#'
#' @description Print basic information regarding a stansim_simulation object,
#'   including simulation title, time/date ran, number of models fitted,
#'   parameters and estimates recorded, and the titles of any datasets that were
#'   refitted.
#'
#' @param x An object of S3 class stansim_simulation.
#' @param ... other arguments not used by this method
#'
#' @seealso S3 class \code{\link{stansim_simulation}}.
#'
#' @examples
#' \dontrun{
#' # print stansim_simulation summary info
#' print(simulation)
#' }
#'
#' @export
print.stansim_simulation <- function(x, ...){

  # stored parameters
  paramaters <- unique(as.character(x$data$parameter))

  # stored estimates
  estimates <- unique(as.character(x$data$estimate))

  cat(paste0("Stan Simulation Title: ", x$sim_name, "\n"))
  cat(paste0("Model Name: ", x$model_name, "\n\n"))
  cat(paste0("Started Running at: ", format(x$start_time), "\n"))
  cat(paste0("Finished Running at: ", format(x$end_time), "\n\n"))

  cat(paste0("Number of Models Fitted: ", length(x$instances), "\n\n"))

  if (length(paramaters) > 50) {
    cat(paste0("Parameters Recorded: ",
        length(paramaters),
        " (first 50 shown)\n"))
  } else {
    cat(paste0("Parameters Recorded: ",
        length(paramaters),
        "\n"))
  }
  print_tidy_matrix(matrix(paramaters[1:50], ncol = 5, byrow = TRUE))

  if (length(estimates) > 50) {
    cat(paste0("\nEstimates Recorded: ",
        length (estimates),
        " (first 50 shown)\n"))
  } else {
    cat(paste0("\nEstimates Recorded: ",
        length (estimates),
        "\n"))
  }
  print_tidy_matrix(matrix(estimates[1:50], ncol = 5, byrow = TRUE))

  if (length(x$refitted) > 5) {
    cat(paste0("\nDatasets Refitted: ",
               length(x$refitted),
               " (first 5 shown)"))
  } else {
    cat(paste0("\nDatasets Refitted: ",
               length(x$refitted)))
  }

  if (length(x$refitted) > 0) {
    for (i in 1:min(c(length(x$refitted), 5))) {
      cat(paste("\n", x$refitted[i]))
    }
  }
}


#-----------------------------------------------------------------
#### print.stansim_collection method ####
#' Print a summary for a stansim_collection object
#'
#' @description Print basic information regarding a stansim_collection object,
#'   including collection title, the simulations within the collection, and any
#'   datasets that have been refitted.
#'
#' @param x An object of S3 class stansim_collection.
#' @param ... other arguments not used by this method
#'
#' @seealso S3 class \code{\link{stansim_collection}}.
#'
#' @examples
#' \dontrun{
#' # print stansim_collection summary info
#' print(collection)
#' }
#'
#' @export
print.stansim_collection <- function(x, ...){

  # collection title
  cat(paste0("Stansim Collection Title: ", x$collection_name, "\n"))

  # print simulation names
  if (length(names(x$simulations)) > 10) {
    cat(paste0("\nSimulations Collected: ",
               length(names(x$simulations)),
               " (first 10 shown)"))
  } else {
    cat(paste0("\nSimulations Collected: ",
               length(names(x$simulations))))
  }

  if (length(names(x$simulations)) > 0) {
    for (i in 1:min(c(length(x$refitted), 10))) {
      cat(paste0("\n  ", names(x$simulations)[i]))
    }
  }


  # print out refitted datasets
  if (nrow(x$refitted) > 5) {
    cat(paste0("\n\nDatasets Refitted: ",
               nrow(x$refitted),
               " (first 5 shown)\n"))
  } else {
    cat(paste0("\n\nDatasets Refitted: ",
               nrow(x$refitted),
               "\n"))
  }

  if (nrow(x$refitted) > 0) {
    print.data.frame(
      x$refitted[1:min(c(nrow(x$refitted), 5)), ], row.names = FALSE
      )
  }

}

#-----------------------------------------------------------------
#### the tidy print matrix internal function for  ####
print_tidy_matrix <- function(m){

  m <- m[rowSums(is.na(m)) != ncol(m), , drop = FALSE]

  m[is.na(m)] <- ""

  utils::write.table(format(m, justify = "left"),
                     row.names = FALSE, col.names = FALSE, quote = FALSE)
}


#-----------------------------------------------------------------
#### print.stansim_data method ####
#' Print a summary for a stansim_data object
#'
#' @description Print basic information regarding a stansim_data object,
#'   including data title, number of datasets, variables recorded, and the name
#'   of the model from which the data was simulated from.
#'
#' @param x An object of S3 class stansim_data.
#' @param ... other arguments not used by this method
#'
#' @seealso S3 class \code{\link{stansim_data}}.
#'
#' @examples
#' \dontrun{
#' # print stansim_data summary info
#' print(simulated_data)
#' }
#'
#' @export
print.stansim_data <- function(x, ...){

  # collection title
  cat(paste0("Stansim Data Title: ", x$data_name, "\n"))

  # number of datasets recorded
  cat(paste0("Number of datasets simulated: ", length(x$datasets), "\n\n"))

  # model simulated from
  cat(paste0("Simulated from model: ", x$model_name))

  # data saved to path [TO ADD]

  ## consider other options to add down the line,
  # will need to change the base object to implement

}
