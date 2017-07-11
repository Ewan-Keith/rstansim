#-----------------------------------------------------------------
#### stansim_uni ####
# constructor for internal object that is returned by each parallel
# run of foreach. Only for internal use and will be combined into
# stansim_single object before being returned.
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
#### stansim_single ####
#' Construct an S3 object of type stansim_single
#'
#' @description A constructor function for creating S3 Objects of
#' type \code{stansim_single}. \code{stansim_single} objects are the basic
#' unit of output from calls to the \code{stansim} function and collects
#' the specified data for all stan models fitted.
#'
#' @param sim_name The name to be given to the simulation represented
#' by the \code{stansim_single} object.
#' @param stansim_uni_list A list of objects with S3 class
#' \code{stan_sim_uni}. This is an unexported class used to store the
#' outcomes of individual simulation runs internal to the \code{stansim}
#' function.
#' @param start_time System time when \code{stansim} was called.
#' @param end_time System time when the results from \code{stansim}
#' were returned.
#' @param stansim_seed The global seed for the \code{stansim} call.
#'
#' @return An S3 object of class \code{stansim_single} recording relevant
#' simulation data.
#'
#' @export
stansim_single <-
  function(sim_name, stansim_uni_list, start_time,
           end_time, raw_call, stansim_seed) {

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
        "data" = longer_data,
        "raw_call" = raw_call
      ),
      class = "stansim_single"
    )
  }





