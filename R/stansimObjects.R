#-----------------------------------------------------------------
#### stansim_uni ####
# constructor for internal object that is returned by each parallel
# run of foreach. Only for internal use and will be combined into
# stansim object before being returned.
stansim_uni <- function(fit, data_name, ran_at, long_data) {

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
      "model_code" = fit@stanmodel@model_code
    ),
    class = "stansim_uni"
  )

}

#-----------------------------------------------------------------
#### stansim ####
# method for constructing stansim objects by merging together
# stansim_uni objects stored in a list with other global args.
# It is this object that will be returned to the user.

stansim <-
  function(sim_name, stansim_uni_list, start_time,
           end_time, stansim_seed) {

    # set simulation name to date if none specified
    stitle <- if (is.null(sim_name)) {
      paste0("Stansim_", as.Date(start_time))
    } else {
      sim_name
    }

    # function for cleaning out simstan_uni elements for storage
    ind_run_clean <- function(single_list){
      # single_list$model_code <- NULL
      # single_list$model_name <- NULL
      # single_list$stan_args <- NULL

      single_list$out_data <- cbind("data" = single_list$data_name, single_list$out_data)

      single_list

    }

    # clean simstan_uni list
    ind_runs <- lapply(stansim_uni_list, ind_run_clean)

    # extract model name and code
    model_code <- stansim_uni_list[[1]]$model_code
    model_name <- stansim_uni_list[[1]]$model_name
    stan_args <- stansim_uni_list[[1]]$stan_args

    # extract long data into own list
    data_list <- lapply(stansim_uni_list, function(i) i$out_data)

    # bind datalist into one object
    longer_data <- do.call(rbind, lapply(data_list, "["))


    structure(
      list(
        "sim_name" = stitle,
        "stan_args" = stan_args,
        "start_time" = start_time,
        "end_time" = end_time,
        "model_name" = model_name,
        "model_code" = model_code,
        "sim_seed" = stansim_seed,
        "data" = longer_data
      ),
      class = "stansim"
    )

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
