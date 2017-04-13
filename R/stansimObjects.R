#-----------------------------------------------------------------
#### stansim_uni ####
# constructor for internal object that is returned by each parallel
# run of foreach. Only for internal use and will be combined into
# stansim object before being returned.
make_stansim_uni <- function(fit, data_name, ran_at, long_data){


  object <- list("data_name" = data_name,
                 "ran_at" = ran_at,
                 "elapsed_time" = rstan::get_elapsed_time(fit),
                 "stan_inits" = fit@inits,
                 "stan_args" = fit@stan_args,
                 "seed" = rstan::get_seed(fit),
                 "long_data" = long_data)

  class(object) <- "stansim_uni"
  return (object)
}

# stansim is class for single run of stan_sim containing:
## date/time of first run
## date/time at end of run
## calculated duration (using above)
## model code
## full param list submitted to stan_sim
## a global seed (setable in stan_sim params)
## full estimated data (Seperate slots for datafile names and parameters)
#
# for each run
##

## dataset ran on #
## start/end time date
## duration
## full stan object stan_args
## full stan objct inits
