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
safeFit <- function(maxRhat = 1.05,
                    maxFailure = 5, ...) {

  safeFitRecurs <- function(maxRhat, maxFailureInt,
                            count = 1, ...) {

    if (count > maxFailureInt)
      return(paste("convergence failed for",
                   maxFailureInt ,
                   "attempts"))

    stanInput <- c(...)

    stanInput['...'] <- NULL


    fit <- do.call(rstan::stan, stanInput)

    max_rhat <- max(rstan::summary(fit)$summary[, "Rhat"])

    if (max_rhat < maxRhat)
      return(c(fit, "attempts" = count))
    else
      safeFitRecurs(maxRhat, maxFailureInt,
                    count = count + 1, ...)
  }

  safeFitRecurs(maxRhat = maxRhat, maxFailureInt = maxFailure, count = 1, ...)

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
  regLoglik <- "generated quantities.*\\{.*log_lik"

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

    if (cont == 1) return(TRUE) else return(FALSE)
  }
  else return(TRUE)
}


paramExtract <- function(safe_fit_model, params, LOO, estimates){
  # returns the selected parameters, their
  # estimates and, if specified, calculated LOO value
  # (the latter as an all or none deal).
  # returns dataframe with rows as params and cols as estimates

  # split out attempts and stanfit
  attempts <- setNames(safe_fit_model[[2]], "fit_attempts")
  safe_fit_model <- safe_fit_model[[1]]

  # for the first version, just allow standard summary values
  # rather than custom percentiles, etc. To do later.

  # extract values and estimates
  model_summary <- rstan::summary(safe_fit_model)$summary

  # at the momemnt user will have to use exact regex
  # e.g. 'eta' below to avois also getting 'theta'
  #params <- c("mu", "^eta")
  regexParams <- paste(params, sep = "", collapse = "|")
  rows <- row.names(model_summary)

  paramIndex <- grepl(regexParams, rows)

  selectedParams <- model_summary[paramIndex,]

  ## extract all select estimates
  # by default return 2.5%, 50%, and 97.5%
  #params <- c("2.5%", "50%", "97.5%")

  output <- selectedParams[,estimates]

  ## different function that flattens matrix to properly named row

  rnames <- rownames(output)
  cnames <- colnames(output)
  pasteU <- function(x, y) paste(x, y, sep = "_")
  returnNames <- c(t(outer(rnames, cnames, pasteU)))

  returnVals <- as.vector(t(output))

  returnJoined <- setNames(returnVals, returnNames)

  returnJoined <- c(returnJoined, attempts)

  # if LOO, then calculate loo values and append
  if(LOO){
    log_lik_1 <- loo::extract_log_lik(safe_fit_model)
    loo_1 <- loo::loo(log_lik_1)

    loo_params <-
      c("elpd_loo",
        "se_elpd_loo",
        "p_loo",
        "se_p_loo",
        "looic",
        "se_looic")


    loo_vals <- as.vector(loo_1[loo_params], "numeric")

    loo_output <- setNames(loo_vals, loo_params)

    returnJoined <- c(returnJoined, loo_output)
  }

  # return

  returnJoined

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
  # # stanModel must be a string (raw model or file location)
  # if(typeof(stanArgs$file) != "symbol" &&
  #    typeof(stanArgs$file) != "character")
  #   stop("stanArgs$file must be of type 'character' if specified")
  #
  # # data must be a list or (by default) type 'language'
  # if(!is.list(stanArgs$data) &&
  #    typeof(stanArgs$data) != "language")
  #   stop("stanArgs$data must be of type 'list' if specified")
  #
  # # iter  must be a positive integer
  # if(!isPosInt(stanArgs$iter))
  #   stop("stanArgs$iter must be a positive integer")
  #
  # # chains must be a positive integer
  # if(!isPosInt(stanArgs$chains))
  #   stop("stanArgs$chains must be a positive integer")

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



singleSim <- function(datafile, newStanArgs = list(),
                      newSimArgs = list(),
                      newReturnArgs = list()){

  ##-------------------------------------------------
  ## setup stan data properly

  # Just using 8schools data. That'll do for now, come back later to
  # properly sort how data should be fed in.

  use_data <- readRDS(datafile)

  newStanArgs$data <- utils::modifyList(newStanArgs$data, use_data,
                    keep.null = TRUE)



  # suppressMessages( specific_data <- readr::read_csv(datafile) )

  # use_Data <- list(N = 200,
  #                  P = 15,
  #                  D = 3,
  #                  C = 30,
  #                  X = as.matrix(specific_data)
  # )

  #return(use_Data)

  # currently working input
  # stanSim(simArgs = list("simData" = c(".travis.yml", ".travis.yml", ".travis.yml")))

  ##-------------------------------------------------
  ## fit the model in a convergence safe manner

  fitted_stan <- safeFit(maxRhat = newSimArgs$maxRhat,
                    maxFailure = newSimArgs$maxRhat,
                    newStanArgs)

  extracted <- paramExtract(fitted_stan, newReturnArgs$pars,
                            newSimArgs$LOO, newSimArgs$probs)

  return(extracted)

  ##-------------------------------------------------
  ## extract all param values

  ##-------------------------------------------------
  ## reshape extracted data in to a single row

  ##-------------------------------------------------
  ## return

}
