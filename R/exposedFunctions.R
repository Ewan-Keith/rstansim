
#' Fits a stan model to multiple datasets and returns estimated values
#'
#'\code{stanSim} fits a specified stan model across multiple datasets and
#'collates and returns the estimated parameters for all models. The function
#'takes three sets of parameters (each provided in a seperate list); stan
#'modelling parameters, parameters governing the running of the multiple
#'stan instances, and a list of values to return (stan parameters along with
#'what summaries to include (e.g. percentiles)). The function allows for
#'stan instances to be ran in parallel, reducing run time on multicore
#'machines.
#'
#' @param stanArgs A list of model parameters to be taken by the \code{stan()} function.
#' If not specified then this functions defaults are used.
#' @param simArgs A list of parameters that control how the instances of \code{stan()}
#' are to be ran.
#' @param returnArgs A list of the parameters and summary statistics to return from all
#' fitted models.
#' @return A dataframe of estimated values across all datasets
#'
#' @export
stanSim <- function(stanArgs = list(), simArgs = list(),
                    returnArgs = list()){

  ##-------------------------------------------------
  ## all input must be lists
  if(!is.list(stanArgs))
    stop("stanArgs must be a list of parameters")

  if(!is.list(simArgs))
    stop("simArgs must be a list of parameters")

  if(!is.list(returnArgs))
    stop("returnArgs must be a list of parameters")

  ##----------------------------------------------------
  ## update defaults with new values
  # update stan defaults
  newStanArgs <- utils::modifyList(stanSimDefaults("stanArgsDefault"), stanArgs,
                            keep.null = TRUE)

  # update simulation defaults
  newSimArgs <- utils::modifyList(stanSimDefaults("simArgsDefault"), simArgs,
                           keep.null = TRUE)

  # update return defaults
  newReturnArgs <- utils::modifyList(stanSimDefaults("returnArgsDefault"), simArgs,
                              keep.null = TRUE)
  # drop pars if unspecified
  if(is.null(newReturnArgs$pars)) newReturnArgs["pars"] <- NULL

  ##-------------------------------------------------
  ## error checks
  # carry out basic input validation
  stanSimChecker(stanArgs = newStanArgs,
                 simArgs = newSimArgs,
                 returnArgs = newReturnArgs)

  # if LOO requested, carry out check for log_lik param in model
  if(newSimArgs$LOO == TRUE) Log_likCheck(newStanArgs$file)

  ##-------------------------------------------------
  ## set up for parallel running and run over the datasets
  doParallel::registerDoParallel(newSimArgs$useCores)

  # define %dopar% alias
  `%doparal%` <- foreach::`%dopar%`

  # parallel loop over datasets
  # foreach::foreach(datafile = newSimArgs$simData,
  #                  .combine='rbind') %doparal%
  #   return(
  #     singleSim(datafile, newStanArgs,
  #               newSimArgs, newReturnArgs)
  #   )

}



#' Returns the default values for stanSim's three parameter lists
#'
#' \code{stanSimDefaults} is used to view the default parameters that stanSim uses if not
#' overwritten by the user. All three lists are accessed by default or by entering
#' "All as the one parameter value. Individual default lists can be accessed by
#' specifying "stanArgsDefault", "simArgsDefault", or
#' "returnArgsDefault".
#'
#' @param getDefault Specifies which parameter list to access, defaults to all three
#' lists. Accepts values c("All", "stanArgsDefault", "simArgsDefault", "returnArgsDefault")
#' @return A list of default parameter values
#'
#' @export
stanSimDefaults <- function(getDefault = "All"){

  ##----------------------------------------------------
  ## set deafult lists
  # extract defaults from rstan
  stanArgsDefault <- formals(rstan::stan)

  # set default simArgs
  simArgsDefault <- list("simData" = NULL, "LOO" = FALSE,
                         "useCores" = 1L, "maxFailures" = 5,
                         "maxRhat" = 1.05)

  # set default returnArgs,
  returnArgsDefault <- list("pars" = NULL,
                            "probs" = c(.025, .5, .975))

  ##----------------------------------------------------
  ## return default lists depending on input
  # return three default lists as one
  if(getDefault == "All") return(
    list("stanArgsDefault" = stanArgsDefault,
         "simArgsDefault" = simArgsDefault,
         "returnArgsDefault" = returnArgsDefault))

  # return stan defs
  else if (getDefault == "stanArgsDefault") return(stanArgsDefault)

  # return sim defs
  else if (getDefault == "simArgsDefault") return(simArgsDefault)

  # return return defs
  else if (getDefault == "returnArgsDefault") return(returnArgsDefault)

  # else error
  else stop("Invalid 'getDefault' Parameter, see documentation")
}
