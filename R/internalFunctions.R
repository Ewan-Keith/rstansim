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
#' @param stanChains The number of chains to run when fitting the model
#' @param maxRhat The maximum R-hat value that will be accepted for convergence
#' @param maxFailure The maximum number of convergence failures to accept
#' @return Will either return a fitted stan object or the string "convergence
#' failed for X attempts" where X is the maxFailure parameter.
#' @examples
#' \dontrun{
#' safeFit(stanModel = "8schools.stan", stanData = schools_dat,
#' stanIter = 1000, stainChains = 4, maxRhat = 1.05)
#'}
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
    cont <- utils::menu(c("Y", "N"), title = contMessage)

    if (cont == 2)
      stop("Simulation Stopped as 'log_lik'
           generated quantity could not be found")
  }
}


paramExtract <- function(params, LOO, estimates){
  # returns a vector of selected parameters, their
  # estimates and, if specified, calculated LOO value
  # (the latter as an all or none deal)
}



#' Validates input for stanSim function.
#'
#' \code{stanSimChecker} runs several tests on input to \code{stanSim()} to check for
#' validity early in the function.
#' @param stanArgs A list of model parameters to be taken by the \code{stan()} function.
#' @param simArgs A list of parameters that control how the instances of \code{stan()}
#' are to be ran.
#' @param returnArgs A list of the parameters and summary statistics to return from all
#' fitted models.
#' @return Returns nothing if valid, stops the function and returns an error if any not.
#'
#' @export
stanSimChecker <- function(stanArgs, simArgs, returnArgs){

  # helper 'is positive integer' function
  isPosInt <- function(val){
    if(is.numeric(val)){
      val %% 1 == 0 && val > 0
    } else FALSE
  }

  ##-------------------------------------------------
  ## stanArgs checks
  # stanModel must be a string (raw model or file location)
  if(typeof(stanArgs$file) != "symbol" &&
     typeof(stanArgs$file) != "character")
    stop("stanArgs$file must be of type 'character' if specified")

  # data must be a list or (by default) type 'language'
  if(!is.list(stanArgs$data) &&
     typeof(stanArgs$data) != "language")
    stop("stanArgs$data must be of type 'list' if specified")

  # iter  must be a positive integer
  if(!isPosInt(stanArgs$iter))
    stop("stanArgs$iter must be a positive integer")

  # chains must be a positive integer
  if(!isPosInt(stanArgs$chains))
    stop("stanArgs$chains must be a positive integer")

  ##-------------------------------------------------
  ## simArgs checks
  # sim data must be specified
  #if(is.null(simArgs$simData))
  #  stop("simArgs$simData must be specified")

  # LOO must be Boolean
  if(!is.logical(simArgs$LOO))
    stop("simArgs$LOO must be Boolean")

  # useCores must be a positive integer
  if(!isPosInt(simArgs$useCores))
    stop("simArgs$useCores must be a positive integer")

  # is useCores less than detected max cores
  if(simArgs$useCores > parallel::detectCores())
    stop(paste0("UseCores parameter must be less than",
                "the number of detected cores [",
                parallel::detectCores(),
                "]"))

  # maxFailures must be a positive integer
  if(!isPosInt(simArgs$maxFailures))
    stop("simArgs$maxFailures must be a positive integer")

  # maxRhat must be numeric
  if(!is.numeric(simArgs$maxRhat))
    stop("simArgs$maxRhat must be numeric")

  # maxRhat must be >= 1
  if(simArgs$maxRhat < 1)
    stop("simArgs$maxRhat must be >= 1")

}

