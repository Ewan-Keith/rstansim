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
#'   after a call to \code{write_data()}. It is strongly recommended that an
#'   informative name is assigned.
#' @param datasets A vector of names of simulated datasets.
#' @param compiled_model An object of S4 class \code{stanmodel}, this should be
#'   the model provided to \code{stansim_simulate()} to simulate data from.
#' @param input_data Values for the data field in the provided stan model.
#' @param param_values A list containing the named values for the stan model
#'   parameters used to simulate data.
#' @param vars The names of the stan variables saved.
#'
#' @return An S3 object of class \code{stansim_data}.
#'
#' @export
stansim_data <- function(data_name,
                         datasets,
                         compiled_model,
                         input_data,
                         param_values,
                         vars) {


  # create s3 object
  structure(
    list(
      "data_name" = data_name,
      "datasets" = datasets,
      "model_name" = compiled_model@model_name,
      "model_code" = compiled_model@model_code,
      "input_data" = input_data,
      "param_values" = param_values,
      "vars" = vars
    ),
    class = "stansim_data"
  )
}
