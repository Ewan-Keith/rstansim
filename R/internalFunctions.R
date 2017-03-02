#' Fit a stan model to data testing for convergence
#'
#'\code{safeFit} places a wrapper around rstan's code{stan} function allowing
#'for an R-hat convergence criteria to be specified. If after fitting this
#'criteria is not met by all parameters the model will be re-fitted a specified
#'number of time. If convergence is judged to have been achieved before this limit
#'is reached then the standard stan fit object, along with the number of attempts
#'made before convergence was achieved will be returned. If convergence is not
#'achieved in this number of steps then a string convergence failed for X
#'attempts" where X is the number of permitted failures.
#'
#' @param stanModel The file location of a valid stan model
#' @param stanData All necessary stan data stored in a named list
#' @param stanIter The number of iterations to run when fitting the model
#' @param stanIter The number of iterations to run when fitting the model
#' @param stanChains The number of chains to run when fitting the model
#' @param maxRhat The maximum R-hat value that will be accepted for convergence
#' @param maxFailure The maximum number of convergence failures to accept
#' @return Will either return a fitted stan object or the string "convergence
#' failed for X attempts" where X is the maxFailure parameter.
#' @examples
#' safeFit(stanModel = "8schools.stan" stanData = schools_dat,
#' stanIter = 1000, stainChains = 4, maxRhat = 1.05)
#'
#' @export
safeFit <- function(stanModel, stanData, stanIter = 5000,
                    stanChains = 4, maxRhat = 1.05,
                    maxFailure = 5) {

  safeFitRecurs <- function(stanModel, stanData, stanIter,
                            stanChains, maxRhat, maxFailure,
                            count = 1) {

    if (count > maxFailure)
      return(paste("convergence failed for",
                   maxFailure ,
                   "attempts"))

    fit <- rstan::stan(file = stanModel, data = stanData,
                       iter = stanIter,chains = stanChains)

    max_rhat <- max(summary(fit)$summary[, "Rhat"])

    if (max_rhat < 1.05)
      return(list(fit, "attempts" = count))
    else
      safeFitRecurs(stanModel, stanData, stanIter,
                    stanChains, count = count + 1)
  }
  safeFitRecurs(stanModel, stanData, stanIter,
                stanChains, maxRhat, maxFailure)
}


#' Calculate number of multi-chain models that can be fit simultaneously
#'
#'\code{coresCalc} Takes the number of chains to run per stan model, and the
#'total number of cores to be used in the simulation. From these it
#'calculates the number of instances of \code{safeFit} that can be ran
#'concurrently.
#'
#' @param stanChains The number of chains to run in each stan model
#' @param useCores The total number of cores to use in the simulation
#' @return The number of models that can be estimated in parallel
#' @examples
#' coresCalc(4L, 8L)
#'
#' @export
coresCalc <- function(stanChains = 1L, useCores = 1L){

  ## exception handling
  # are both numeric
  if(!is.numeric(stanChains))
    stop("stanChains parameter must be numeric")

  if(!is.numeric(useCores))
    stop("useCores parameter must be numeric")

  # are both round
  if(stanChains %% 1 != 0)
    stop("stanChains parameter must be an integer value")

  if(useCores %% 1 != 0)
    stop("useCores parameter must be an integer value")

  # are both positive
  if(stanChains < 1)
    stop("stanChains parameter must be positive")

  if(useCores < 1)
    stop("useCores parameter must be positive")

  # is useCores less than detected max cores
  if (useCores > parallel::detectCores())
    stop(
      paste0(
        paste0("UseCores parameter must be less than",
        "the number of detected cores [",
        parallel::detectCores(),
        "]")
      )
    )

  ## calculate output
  if(useCores <= stanChains) return(1)
  else {
    simCores <- useCores %/% stanChains

    # let user know if any requested cores will be unused
    if(simCores * stanChains < useCores)
      message(paste(useCores - simCores * stanChains,
                    "cores will go unused with",
                    useCores, "cores available and",
                    stanChains, "chains per run"))

    return(simCores)
  }
}

#' Checks that the user has specified a log_lik object in their stan code
#'
#'\code{log_likCheck} uses a regular expression to test, in a very broad way,
#'whether the users stan model has a lok_lik object specified in the generated
#'quantities block of the code. This is required if LOO statistics are to
#'be calculated. If it's existence can not be confirmed users are prompted
#'whether they wish to continue with the simulation or stop it.
#'
#' @param stanModel TThe file location of a valid stan model
#' @return Either passes nothing if succesful or results in an error
#'
#' @export
Log_likCheck <- function(stanModel){
  regLoglik <- "generated quantities.*\\{.*log_lik.*\\}"

  contMessage <-
    paste(
      "The 'log_lik' generated quantity was not found in the",
      "provided stan model.\nThis property is required to calculate LOO",
      "statistics.\nIf you know it is present in stanModel enter 'Y'",
      "to continue. \nElse enter 'N' to cancel and examine the model",
      "specification.\nsee",
      "https://cran.r-project.org/web/packages/loo/vignettes/loo-example.html"
    )

  if(!grepl(regLoglik, stanModel)) {
    cont <- menu(c("Y", "N"), title = contMessage)

    if (cont == 2)
      stop("Simulation Stopped as 'log_lik'
           generated quantity could not be found")
  }
}



