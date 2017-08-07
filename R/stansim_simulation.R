#-----------------------------------------------------------------
#### stansim_simulation ####
#' Construct an S3 object of type stansim_simulation
#'
#' @description A constructor function for creating S3 objects of class
#'   \code{stansim_simulation}. \code{stansim_simulation} objects are the basic
#'   unit of output from calls to the \code{stansim()} function and collects the
#'   specified data for all stan models fitted.
#'
#' @param sim_name The name to be given to the simulation represented by the
#'   \code{stansim_simulation} object.
#' @param stansim_uni_list A list of objects with S3 class \code{stan_sim_uni}.
#'   This is an unexported class used to store the outcomes of individual
#'   simulation runs internal to the \code{stansim()} function.
#' @param start_time System time when \code{stansim()} was called.
#' @param end_time System time when the results from \code{stansim()} were
#'   returned.
#' @param raw_call The values of all arguments provided to \code{stansim()} when
#'   first ran. This is used for any refitting of datasets using the
#'   \code{refit()} method.
#' @param stansim_seed The global seed for the \code{stansim()} call.
#'
#' @return An S3 object of class \code{stansim_simulation} recording relevant
#'   simulation data.
#'
#' @export
stansim_simulation <-
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
        "raw_call" = raw_call,
        "refitted" = c()
      ),
      class = "stansim_simulation"
    )
  }



