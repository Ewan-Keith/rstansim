#-----------------------------------------------------------------
#### stansim_data ####
#' Construct an S3 object of type stansim_data
#'
#' @description A constructor function for creating S3 objects of class
#'   \code{stansim_data}. \code{stansim_data} objects are the in-memory option
#'   for storing the results of a call to \code{stansim_simulate()} and feeding
#'   the results to \code{stansim()}.
#'
#' @param data_name A name attached to the \code{stansim_data} object to help
#'   identify it. This also forms the stem of the individual .rds file names
#'   after a call to \code{write_data()}. It is strongly recomended that an
#'   informative name is assigned.
#' @param data A list of lists containing simulated data.
#' @param compiled_model An object of S4 class \code{stanmodel}, this should be
#'   the model provided to \code{stansim_simulate()} to simulate data from.
#'
#' @return An S3 object of class \code{stansim_data}.
#'
#' @export
stansim_data <- function(data_name,
                         data,
                         compiled_model) {
  # create s3 object
  structure(
    list(
      "data_name" = data_name,
      "data" = data,
      "model_name" = compiled_model@model_name,
      "model_code" = compiled_model@model_code
    ),
    class = "stansim_data"
  )
}
