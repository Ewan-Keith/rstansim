% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/refit.R
\name{refit}
\alias{refit}
\title{Refit specified datasets in a stansim_simulation object}
\usage{
refit(object, datasets, stan_args = list(), calc_loo = FALSE,
  use_cores = 1L, cache = TRUE, seed = floor(stats::runif(1, 1, 1e+05)))
}
\arguments{
\item{object}{An object of S3 class stansim_simulation.}

\item{datasets}{The full names of the data files to be refitted. These must
be consistent both with the dataset names stored within the
\code{stansim_simulation} object, and with the copies of the data files
relative to the current working directory. This is best ensured by running
refit from the same working directory as the original \code{fit_models()} call.}

\item{stan_args}{A list of function arguments to be used by the internal
\code{rstan::sampling()} function when fitting the models. If not specified
then the \code{sampling()}  defaults are used.}

\item{calc_loo}{If \code{TRUE} then model fit statistics will be calculated
using the \code{loo} package. If \code{TRUE} there must be a valid log_lik
quantity specified in the generated quantities section of the provided stan
model.}

\item{use_cores}{Number of cores to use when running in parallel. Each stan
model is fitted serially regardless of the number of chains ran as
parallelisation across models is more flexible than within.}

\item{cache}{If \code{TRUE} then the results for each instance are written to
a local, temporary file so that data is not lost should the function not
terminate properly. This temporary data is removed upon the model
terminating as expected. If \code{FALSE} no data is written and results are
only returned upon the correct termination of the whole function. The
default value of \code{TRUE} is recommended unless there are relevant
write-permission restrictions.}

\item{seed}{Set a seed for the \code{fit_models()} function.}
}
\value{
An S3 object of class \code{stansim_simulation} recording relevant
  simulation data.
}
\description{
\code{refit} Takes a \code{stansim_simulation} object and a
  vector of characters corresponding to the names of datasets fitted within
  the \code{stansim_simulation} object, and refits the stan model for each of
  these instances. This allows users to refit any specific models using new
  stan arguments if need be (e.g. if the model fails to converge in the
  original run).
}
\examples{
\dontrun{
# refit datasets "data_file-12.rds" & "data_file-08.rds"
refit(simulation,
      datasets = c("data_file-12.rds", "data_file-08.rds")
      use_cores = 4)

# refit dataset "data_file-12.rds" using a larger number of samples
refit(simulation,
      datasets = "data_file-12.rds",
      stan_args = list(iter = 4000),
      use_cores = 4)
}

}
