#-----------------------------------------------------------------
#### stansim_uni ####
# constructor for internal object that is returned by each parallel
# run of foreach. Only for internal use and will be combined into
# stansim object before being returned.
stansim_uni <- function(fit, data_name, ran_at, long_data, stan_warnings,
                        cache) {

  structure(
    list(
      "data_name" = data_name,
      "ran_at" = ran_at,
      "elapsed_time" = rstan::get_elapsed_time(fit),
      "stan_inits" = fit@inits,
      "stan_args" = fit@stan_args,
      "seed" = rstan::get_seed(fit),
      "out_data" = long_data,
      "model_name" = fit@model_name,
      "model_code" = fit@stanmodel@model_code,
      "warnings" = stan_warnings
    ),
    class = "stansim_uni"
  )

}

#-----------------------------------------------------------------
#### stansim ####
# method for constructing stansim objects by merging together
# stansim_uni objects stored in a list with other global args.
# It is this object that will be returned to the user.

stansim <-
  function(sim_name, stansim_uni_list, start_time,
           end_time, stansim_seed, stan_warnings) {

    ## extract all individual stan instance level data
    # function for cleaning out simstan_uni elements for storage
    ind_run_clean <- function(single_list){
      single_list$model_code <- NULL
      single_list$model_name <- NULL
      single_list$out_data <- NULL

      unclass(single_list)
    }

    # clean simstan_uni list
    ind_runs <- lapply(stansim_uni_list, ind_run_clean)

    ## extract all simulation level data
    # extract model name and code
    model_code <- stansim_uni_list[[1]]$model_code[[1]]
    model_name <- stansim_uni_list[[1]]$model_name

    # extract long data into own list
    data_list <- lapply(stansim_uni_list, function(i) i$out_data)

    # bind datalist into one object
    longer_data <- do.call(rbind, lapply(data_list, "["))

    # create s3 object
    structure(
      list(
        "sim_name" = sim_name,
        "start_time" = start_time,
        "end_time" = end_time,
        "model_name" = model_name,
        "model_code" = model_code,
        "sim_seed" = stansim_seed,
        "instances" = ind_runs,
        "data" = longer_data
      ),
      class = "stansim"
    )
  }


#-----------------------------------------------------------------
#### print.stansim method ####
#' Print a summary for a stansim simulation object
#'
#' @description Print basic  information regarding a stansim simulation object,
#' including simulation title, time/date ran, number of models fitted, and parameters
#' and estimates recorded.
#'
#' @param x An object of S3 class stansim.
#'
#' @export
print.stansim <- function(x){

  # helper method for clean matrix printing
  print.matrix <- function(m){

    m <- m[rowSums(is.na(m))!=ncol(m), , drop = FALSE]

    m[is.na(m)] <- ""

    write.table(format(m, justify="left"),
                row.names=F, col.names=F, quote=F)
  }

  # stored parameters
  paramaters <- unique(as.character(x$data$parameter))

  # stored estimates
  estimates <- unique(as.character(x$data$estimate))

  cat("Stan Simulation Title:", x$sim_name, "\n")
  cat("Model Name:", x$model_name, "\n\n")
  cat("Started Running at:", format(x$start_time), "\n")
  cat("Finished Running at:", format(x$end_time), "\n\n")

  cat("Number of Models Fitted:", length(x$instances), "\n\n")

  if (length(paramaters) > 50) {
    cat("Parameter Estimates Recorded:",
        length(paramaters),
        "(first 50 shown)\n")
  } else {
    cat("Parameter Estimates Recorded:",
        length(paramaters),
        "\n")
  }
  print.matrix(matrix(paramaters[1:50], ncol = 5, byrow = TRUE))

  if(length(estimates) > 50) {
    cat("\nEstimates Recorded:",
        length (estimates),
        "(first 50 shown)\n")
  } else {
    cat("\nEstimates Recorded:",
        length (estimates),
        "\n")
  }
  print.matrix(matrix(estimates[1:50], ncol = 5, byrow = TRUE))

}

#-----------------------------------------------------------------
#### extract_data generic method ####
#' Extract data from rstansim objects
#'
#' @description Generic functin for extracting data from rstansim objects.
#' Default arguments will return full data as a dataframe, otherwise
#' rows will be filtered based on provided arguments. DONT FORGET TO ADD USAGE CASES FOR DOC
#'
#' @param x An object of S3 class stansim.
#'
#' @export
extract_data <- function (object, ...) {
  UseMethod("extract_data", object)
}

#-----------------------------------------------------------------
#### extract_data.stansim method ####
# method to extract data from a stansim object based on string filtering
# of the fields
#' Extract data from a stansim object
#'
#' @description get data out of a single stansim object
#'
#' @param x An object of S3 class stansim.
#'
#' @export
extract_data.stansim <- function(object, dataset = "all", parameter = "all",
                         estimate = "all", value = NULL, ...) {

  ## carry out basic input validation
  if(!is.function(value) & !is.null(value))
    stop("value argument must be NULL or a function")
print(dataset)
  if(!is.character(dataset))
    stop("dataset argument must be of type character")

  if(!is.character(parameter))
    stop("parameter argument must be of type character")

  if(!is.character(estimate))
    stop("estimate argument must be of type character")

}




