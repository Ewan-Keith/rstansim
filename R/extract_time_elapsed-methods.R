#-----------------------------------------------------------------
#### extract_time_elapsed generic method ####
#' Extract time_elapsed from rstansim objects
#'
#' @description Generic function for extracting the time taken to fit rstansim
#'   models Default arguments will return full data as a dataframe, otherwise
#'   rows will be filtered based on provided arguments.
#'
#' @param object An S3 object of class stansim_simulation or stansim_collection.
#' @param ... Arguments for filtering returned data, see specific methods for
#'   further detail.
#' @return A dataframe containing the specified data.
#'
#' @export
extract_time_elapsed <- function (object, ...) {
  UseMethod("extract_time_elapsed", object)
}

#-----------------------------------------------------------------
#### extract_time_elapsed.stansim_simulation method ####
#' Extract time_elapsed from a stansim_simulation object
#'
#' @description Applied to an object of type stansim_simulation,
#'   \code{extract_time_elapsed()} will return the time taken to fit the
#'   models contained within the object a dataframe, subject to the filtering
#'   specified by the function arguments.
#'
#' @param object An object of S3 class stansim_simulation.
#' @param datasets Either a character vector containing the names of datasets
#'   (as provided to the original \code{fit_models()} call) fitted, or the
#'   string \code{"all"}. The former will only return values for the
#'   corresponding datasets, the latter applies no filtering on datasets
#' @param chains Either a character vector containing the numbers of the stan model
#'   chains to return, or the string \code{"all"} The former will only return
#'   values for the corresponding chains, the latter applies no filtering on
#'   chains.
#' @param stages Either a character vector containing the names of model fitting
#'   stages, \code{c("warmup", "sample", "total")}, or the string \code{"all"}.
#'   The former will only return values for the corresponding stages, the latter
#'   applies no filtering on estimates.
#' @param elapsed Either a function taking a single numeric argument that
#'   returns a Boolean value, or \code{NULL}. The former will only return
#'   elapsed times for which the provided function is \code{TRUE}, the latter
#'   applies no filtering on elapsed times
#' @param ... other arguments not used by this method
#'
#' @return A dataframe containing the specified data.
#'
#' @examples
#' \dontrun{
#' # extract full dataset
#' extract_time_elapsed(simulation)
#'
#' # extract all rows for dataset "data_file-12.rds"
#' extract_time_elapsed(simulation, datasets = "data_file-12.rds")
#'
#' # extract results for chains 1 and 3
#' extract_time_elapsed(simulation, chains = c(1, 3))
#'
#' # extract results for only the warmup stage
#' extract_time_elapsed(simulation, stages = "warmup")
#'
#' # extract all elapsed times greater than 60 seconds
#' extract_time_elapsed(simulation,
#'                      elapsed = function(x) x > 60)
#' }
#'
#' @export
extract_time_elapsed.stansim_simulation <-
  function(object,
           datasets = "all",
           chains = "all",
           stages = "all",
           elapsed = NULL,
           ...) {

    ## -------------------------------------------------
    ## carry out basic input validation
    if (!is.function(elapsed) & !is.null(elapsed))
      stop("elapsed argument must be NULL or a function")

    if (!is.character(datasets))
      stop("datasets argument must be of type character")

    if (!is.numeric(chains) &  !("all" %in% chains))
      stop("chains argument must be \"all\" or of type numeric")

    if (!is.character(stages))
      stop("stages argument must be of type character")

    # stages must be warmup, sample or total
    if (sum(!(stages %in% c("all", "warmup", "sample", "total"))) != 0)
      stop("stages must be all, warmup, sample or total")

    ## -------------------------------------------------
    ## pull out raw elapsed times
    raw_times <- lapply(object$instances, "[", "elapsed_time")

    # convert to dataframe
    df_times <- lapply(raw_times, as.data.frame)

    # setup total addition function
    total_col <- function(x){
      x$elapsed_time.total <- rowSums(x)

      x
    }

    # add "total" column to all
    times_total <- lapply(df_times, total_col)

    # function to reshape dfs to long
    times_to_long <- function(x) {
      # raw transform
      raw_long <- stats::reshape(x,
                          direction = "long",
                          varying = names(x),
                          sep = ".")

      # drop rownames
      rownames(raw_long) <- NULL

      #change colnames
      colnames(raw_long) <- c("time", "elapsed", "chain")

      # return
      raw_long
    }

    # reshape all dfs to long format
    long_list <- lapply(times_total, times_to_long)

    # reorder columns
    long_ordered <- lapply(long_list, "[", c(3, 1, 2))

    ## extract dataset names
    dataset_list <- lapply(object$instances, "[", "data_name")

    ## merge dataset name to time data
    # init output list
    merged_time <- vector("list", length(object$instances))

    # merge data
    for(i in seq(length(object$instances))){
      merged_time[[i]] <- cbind(dataset_list[[i]], long_ordered[[i]])
    }

    # rbind all dataframes into 1
    single_time_data <- do.call("rbind", merged_time)

    # rename columns
    named_time <- stats::setNames(single_time_data, c("dataset", "chain",
                                                      "stage", "elapsed"))

    ## -------------------------------------------------
    # carry out filtering from input

    # filter out datasets
    if ("all" %in% datasets){
      if (length(datasets) > 1) {
        stop(paste(
          "if datasets argument contains \"any\",",
          "length(datasets) must be 1"
        ))
      }} else {
        named_time <- named_time[named_time$dataset %in% datasets, ]
      }

    # filter out chains
    if ("all" %in% chains){
      if (length(chains) > 1) {
        stop(paste(
          "if chains argument contains \"any\",",
          "length(chains) must be 1"
        ))
      }}
    else {
      named_time <- named_time[named_time$chain %in% chains, ]
    }

    # filter out stages
    if ("all" %in% stages){
      if (length(stages) > 1) {
        stop(paste(
          "if stages argument contains \"any\",",
          "length(stages) must be 1"
        ))
      }}
    else {
      named_time <- named_time[named_time$stage %in% stages,]
    }

    # filter on elapsed
    if(!is.null(elapsed))
      named_time <- named_time[elapsed(named_time$elapsed), ]


    # convert datasets to character from factor
    named_time$dataset <- as.character(named_time$dataset)

    # return dataframe
    named_time
  }


#-----------------------------------------------------------------
#### extract_time_elapsed.stansim_collection method ####
#' Extract time_elapsed from a stansim_collection object
#'
#' @description Applied to an object of type stansim_collection,
#'   \code{extract_time_elapsed()} will return the time taken to fit the
#'   models contained within the object a dataframe, subject to the filtering
#'   specified by the function arguments.
#'
#' @param object An object of S3 class stansim_collection.
#' @param sim_names Either a character vector containing the names of the
#'   \code{stansim_simulation} objects grouped in the collection, or the string
#'   \code{"all"}. The former will only return values for the corresponding
#'   simulations, the latter applies no filtering on stansim simulations.
#' @param datasets Either a character vector containing the names of datasets
#'   (as provided to the original \code{fit_models()} calls) fitted, or the
#'   string \code{"all"}. The former will only return values for the
#'   corresponding datasets, the latter applies no filtering on datasets
#' @param chains Either a character vector containing the numbers of the stan model
#'   chains to return, or the string \code{"all"} The former will only return
#'   values for the corresponding chains, the latter applies no filtering on
#'   chains.
#' @param stages Either a character vector containing the names of model fitting
#'   stages, \code{c("warmup", "sample", "total")}, or the string \code{"all"}.
#'   The former will only return values for the corresponding stages, the latter
#'   applies no filtering on estimates.
#' @param elapsed Either a function taking a single numeric argument that
#'   returns a Boolean value, or \code{NULL}. The former will only return
#'   elapsed times for which the provided function is \code{TRUE}, the latter
#'   applies no filtering on elapsed times
#' @param ... other arguments not used by this method
#'
#' @return A dataframe containing the specified data.
#'
#' @examples
#' \dontrun{
#' # extract full dataset
#' extract_time_elapsed(simulation)
#'
#' # extract all rows for dataset "data_file-12.rds"
#' extract_time_elapsed(simulation, datasets = "data_file-12.rds")
#'
#' # extract results for chains 1 and 3
#' extract_time_elapsed(simulation, chains = c(1, 3))
#'
#' # extract results for only the warmup stage
#' extract_time_elapsed(simulation, stages = "warmup")
#'
#' # extract all elapsed times greater than 60 seconds
#' extract_time_elapsed(simulation,
#'                      elapsed = function(x) x > 60)
#' }
#'
#' @export
extract_time_elapsed.stansim_collection <-
  function(object,
           sim_names = "all",
           datasets = "all",
           chains = "all",
           stages = "all",
           elapsed = NULL,
           ...) {

    ## -------------------------------------------------
    ## carry out basic input validation
    if (!is.function(elapsed) & !is.null(elapsed))
      stop("elapsed argument must be NULL or a function")

    if (!is.character(sim_names))
      stop("sim_names argument must be of type character")

    if (!is.character(datasets))
      stop("datasets argument must be of type character")

    if (!is.numeric(chains) &  !("all" %in% chains))
      stop("chains argument must be \"all\" or of type numeric")

    if (!is.character(stages))
      stop("stages argument must be of type character")

    # stages must be warmup, sample or total
    if (sum(!(stages %in% c("all", "warmup", "sample", "total"))) != 0)
      stop("stages must be all, warmup, sample or total")

    ## -------------------------------------------------
    ## pull out raw elapsed times
    # prepare output list
    raw_times <- vector(mode = "list", length = 0)

    # loop over simulations and store times
    for (i in seq(length(object$simulations))) {
      raw_times[[i]] <- lapply(object$simulations[[i]]$instances, "[", "elapsed_time")
    }

    ## convert to dataframes
    # prepare df_output
    df_times <- vector(mode = "list", length = 0)

    # loop over simulations and convert to dataframes
    for (i in seq(length(raw_times))) {
      df_times[[i]] <- lapply(raw_times[[i]], as.data.frame)
    }

    ## setup total addition function
    total_col <- function(x){
      x$elapsed_time.total <- rowSums(x)

      x
    }

    ## add "total" column to all
    # prepare total column output
    times_total <- vector(mode = "list", length = 0)

    # loop over simulations and add total column
    for (i in seq(length(df_times))) {
      times_total[[i]] <- lapply(df_times[[i]], total_col)
    }

    ## function to reshape dfs to long
    times_to_long <- function(x) {
      # raw transform
      raw_long <- stats::reshape(x,
                                 direction = "long",
                                 varying = names(x),
                                 sep = ".")

      # drop rownames
      rownames(raw_long) <- NULL

      #change colnames
      colnames(raw_long) <- c("time", "elapsed", "chain")

      # return
      raw_long
    }

    ## reshape all dfs to long format
    # prepare output
    long_list <- vector(mode = "list", length = 0)

    # loop over simulations and reshape all dataframes
    for (i in seq(length(times_total))) {
      long_list[[i]] <- lapply(times_total[[i]], times_to_long)
    }

    ## reorder columns
    # prepare output
    long_ordered <- vector(mode = "list", length = 0)

    # loop over simulations and reorder columns
    for (i in seq(length(long_list))) {
      long_ordered[[i]] <- lapply(long_list[[i]], "[", c(3, 1, 2))
    }

    ## extract dataset names
    # prepare output
    dataset_list <- vector(mode = "list", length = 0)

    # loop over simulations and extract dataset names
    for (i in seq(length(object$simulations))) {
      dataset_list[[i]] <- lapply(object$simulations[[i]]$instances, "[", "data_name")
    }

    ## merge dataset name to time data
    # init output list
    merged_time <- vector("list", length(object$simulations))

    for (i in seq(length(object$simulations))) {
      merged_time[[i]] <-
        rep(list(list()), length(object$simulations[[i]]$instances))
    }

    # merge data
    for (i in seq(length(object$simulations))) {
      for (c in seq(length(object$simulations[[i]]$instances))) {
        merged_time[[i]][[c]] <-
          cbind(dataset_list[[i]][[c]], long_ordered[[i]][[c]])
      }
    }

    ## merge simulation name to time data
    # get simulation names
    get_sim_names  <- names(object$simulations)

    # init output list
    named_time <- vector("list", length(object$simulations))

    for (i in seq(length(object$simulations))) {
      named_time[[i]] <-
        rep(list(list()), length(object$simulations[[i]]$instances))
    }

    # merge data
    for (i in seq(length(object$simulations))) {
      for (c in seq(length(object$simulations[[i]]$instances))) {
        named_time[[i]][[c]] <-
          cbind(get_sim_names[[i]], merged_time[[i]][[c]])
      }
    }

    ## rbind all dataframes into 1
    # collapse into single list
    collapsed_data <- unlist(named_time, recursive = FALSE)

    # rbind list into one dataframe
    single_time_data <- do.call("rbind", collapsed_data)

    ## rename columns
    named_time <- stats::setNames(single_time_data, c("sim_name", "dataset", "chain",
                                                      "stage", "elapsed"))

    ## -------------------------------------------------
    # carry out filtering from input

    # filter out sim_names
    if ("all" %in% sim_names){
      if (length(sim_names) > 1) {
        stop(paste(
          "if sim_names argument contains \"any\",",
          "length(sim_names) must be 1"
        ))
      }} else {
        named_time <- named_time[named_time$sim_name %in% sim_names, ]
      }

    # filter out datasets
    if ("all" %in% datasets){
      if (length(datasets) > 1) {
        stop(paste(
          "if datasets argument contains \"any\",",
          "length(datasets) must be 1"
        ))
      }} else {
        named_time <- named_time[named_time$dataset %in% datasets, ]
      }

    # filter out chains
    if ("all" %in% chains){
      if (length(chains) > 1) {
        stop(paste(
          "if chains argument contains \"any\",",
          "length(chains) must be 1"
        ))
      }}
    else {
      named_time <- named_time[named_time$chain %in% chains, ]
    }

    # filter out stages
    if ("all" %in% stages){
      if (length(stages) > 1) {
        stop(paste(
          "if stages argument contains \"any\",",
          "length(stages) must be 1"
        ))
      }}
    else {
      named_time <- named_time[named_time$stage %in% stages,]
    }

    # filter on elapsed
    if(!is.null(elapsed))
      named_time <- named_time[elapsed(named_time$elapsed), ]

    # convert dataset to character from factor
    named_time$dataset <- as.character(named_time$dataset)

    # convert sim_name to character from factor
    named_time$sim_name <- as.character(named_time$sim_name)

    # return dataframe
    named_time

  }
