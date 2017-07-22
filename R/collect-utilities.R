#-----------------------------------------------------------------
#### collect_simulations ####
collect_simulations <- function(collection_name, sim1, sim2){

  #### extract and merge data with sim_name added ####
  # extract
  sim1_data <- cbind.data.frame("sim_name" = sim1$sim_name,
                                extract_data(sim1),
                                stringsAsFactors = FALSE)

  sim2_data <- cbind.data.frame("sim_name" = sim2$sim_name,
                                extract_data(sim2),
                                stringsAsFactors = FALSE)

  # merge
  merged_data <- rbind(sim1_data, sim2_data)

  #### extract and reconstruct refitted records ####
  # extract (with specific NULL handling)
  if (length(extract_refitted(sim1)) == 0) {
    sim1_refitted <- NULL
  } else {
    sim1_refitted <- cbind.data.frame(
      "sim_name" = sim1$sim_name,
      "datafile" = extract_refitted(sim1),
      stringsAsFactors = FALSE
    )
  }

  if (length(extract_refitted(sim2)) == 0) {
    sim2_refitted <- NULL
  } else {
    sim2_refitted <- cbind.data.frame(
      "sim_name" = sim2$sim_name,
      "datafile" = extract_refitted(sim2),
      stringsAsFactors = FALSE
    )
  }

  # merge
  merged_refitted <- rbind(sim1_refitted, sim2_refitted)

  #### extract and merge all other simulation data ####
  # remove data fom both
  sim1$data <- NULL
  sim2$data <- NULL

  # remove refitted from both
  sim1$refitted <- NULL
  sim2$refitted <- NULL

  # remove stansim_simulation class
  attributes(sim1)$class <- NULL
  attributes(sim2)$class <- NULL

  ## merge, rename, and remove original names
  merged_list <- list(sim1, sim2)

  # get names vector
  names_vect <- vector(length = 2)
  for(i in 1:2)
    names_vect[i] <- merged_list[[i]]$sim_name

  # rename merged list
  names(merged_list) <- names_vect

  # remove original names
  for(i in 1:2)
    merged_list[[i]]$sim_name <- NULL

  #### call stansim_collector constructor and return ####
  stansim_collection(collection_name = collection_name,
                     data = merged_data,
                     refitted = merged_refitted,
                     simulations = merged_list)
}

#-----------------------------------------------------------------
#### collect_collections ####

#-----------------------------------------------------------------
#### collect_mixed ####
