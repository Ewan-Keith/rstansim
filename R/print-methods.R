#-----------------------------------------------------------------
#### print.stansim method ####
#' Print a summary for a stansim simulation object
#'
#' @description Print basic  information regarding a stansim simulation object,
#' including simulation title, time/date ran, number of models fitted, and parameters
#' and estimates recorded.
#'
#' @param x An object of S3 class stansim.
#' @param ... other arguments not used by this method
#'
#' @export
print.stansim_simulation <- function(x, ...){

  # helper method for clean matrix printing
  print.matrix <- function(m){

    m <- m[rowSums(is.na(m)) != ncol(m), , drop = FALSE]

    m[is.na(m)] <- ""

    utils::write.table(format(m, justify = "left"),
                       row.names = FALSE, col.names = FALSE, quote = FALSE)
  }

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
  print.matrix(matrix(paramaters[1:50], ncol = 5, byrow = TRUE))

  if (length(estimates) > 50) {
    cat(paste0("\nEstimates Recorded: ",
        length (estimates),
        " (first 50 shown)\n"))
  } else {
    cat(paste0("\nEstimates Recorded: ",
        length (estimates),
        "\n"))
  }
  print.matrix(matrix(estimates[1:50], ncol = 5, byrow = TRUE))

  if (length(x$refitted) > 5) {
    cat(paste0("\nDatasets Refitted: ",
               length(x$refitted),
               " (first 5 shown)\n"))
  } else {
    cat(paste0("\nDatasets Refitted: ",
               length(x$refitted),
               "\n"))
  }

  if (length(x$refitted) > 0) {
    for (i in 1:min(c(length(x$refitted), 5))) {
      cat(x$refitted[i])
    }
  }
}

