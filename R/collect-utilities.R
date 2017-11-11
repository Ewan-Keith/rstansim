#-----------------------------------------------------------------
#### collect_simulations_internal ####
collect_simulations_internal <- function(collection_name, sim1, sim2){

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
      "dataset" = extract_refitted(sim1),
      stringsAsFactors = FALSE
    )
  }

  if (length(extract_refitted(sim2)) == 0) {
    sim2_refitted <- NULL
  } else {
    sim2_refitted <- cbind.data.frame(
      "sim_name" = sim2$sim_name,
      "dataset" = extract_refitted(sim2),
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
  for (i in 1:2)
    names_vect[i] <- merged_list[[i]]$sim_name

  # rename merged list
  names(merged_list) <- names_vect

  # remove original names
  for (i in 1:2)
    merged_list[[i]]$sim_name <- NULL

  #### call stansim_collector constructor and return ####
  stansim_collection(collection_name = collection_name,
                     data = merged_data,
                     refitted = merged_refitted,
                     simulations = merged_list)
}

#-----------------------------------------------------------------
#### collect_collections ####
collect_collections <- function(collection_name, coll1, coll2){

  #### extract and merge data ####
  # extract
  coll1_data <- extract_data(coll1)

  coll2_data <- extract_data(coll2)

  # merge
  merged_data <- rbind(coll1_data, coll2_data)

  #### extract and reconstruct refitted records ####
  # extract (with specific NULL handling)
  if (length(extract_refitted(coll1)) == 0) {
    coll1_refitted <- NULL
  } else {
    coll1_refitted <- extract_refitted(coll1)
  }

  if (length(extract_refitted(coll2)) == 0) {
    coll2_refitted <- NULL
  } else {
    coll2_refitted <- extract_refitted(coll2)
  }

  # merge
  merged_refitted <- rbind(coll1_refitted, coll2_refitted)

  #### merge old simulation lists ####
  merged_list <- c(coll1$simulations, coll2$simulations)

  #### call stansim_collector constructor and return ####
  stansim_collection(collection_name = collection_name,
                     data = merged_data,
                     refitted = merged_refitted,
                     simulations = merged_list)


}

#-----------------------------------------------------------------
#### collect_mixed ####
collect_mixed <- function(collection_name, sim1, coll1){

  #### extract and merge data with sim_name added ####
  # extract
  sim1_data <- cbind.data.frame("sim_name" = sim1$sim_name,
                                extract_data(sim1),
                                stringsAsFactors = FALSE)

  coll1_data <- extract_data(coll1)

  # merge
  merged_data <- rbind(sim1_data, coll1_data)

  #### extract and reconstruct refitted records ####
  # extract (with specific NULL handling)
  if (length(extract_refitted(sim1)) == 0) {
    sim1_refitted <- NULL
  } else {
    sim1_refitted <- cbind.data.frame(
      "sim_name" = sim1$sim_name,
      "dataset" = extract_refitted(sim1),
      stringsAsFactors = FALSE
    )
  }

  if (length(extract_refitted(coll1)) == 0) {
    coll1_refitted <- NULL
  } else {
    coll1_refitted <- extract_refitted(coll1)
  }

  # merge
  merged_refitted <- rbind(sim1_refitted, coll1_refitted)

  #### extract and merge all other simulation data ####
  # remove data fom both
  sim1$data <- NULL

  # remove refitted from both
  sim1$refitted <- NULL

  # remove stansim_simulation class
  attributes(sim1)$class <- NULL

  # rename sim1 list
  renamed_sim1 <- list("temp" = sim1)
  names(renamed_sim1) <- sim1$sim_name

  # remove original name
  sim1$sim_name <- NULL

  # merge lists
  merged_list <- c(renamed_sim1, coll1$simulations)

  # remove sim1 original name
  merged_list[[1]]$sim_name <- NULL

  #### call stansim_collector constructor and return ####
  stansim_collection(collection_name = collection_name,
                     data = merged_data,
                     refitted = merged_refitted,
                     simulations = merged_list)
}

#-----------------------------------------------------------------
#### collect_single_sim_internal ####
collect_single_sim_internal <- function(collection_name, object){

  #extract data with sim_name added to tidy data
  sim_to_coll_data <-
    cbind.data.frame("sim_name" = object$sim_name,
                     extract_data(object),
                     stringsAsFactors = FALSE)

  # extract and reconstruct refitted records(with specific NULL handling)
  if (length(extract_refitted(object)) == 0) {
    sim_to_coll_refitted <- NULL
  } else {
    sim_to_coll_refitted <- cbind.data.frame(
      "sim_name" = object$sim_name,
      "dataset" = extract_refitted(object),
      stringsAsFactors = FALSE
    )
  }

  # extract all other simulation data
  # remove data
  object$data <- NULL

  # remove refitted
  object$refitted <- NULL

  # remove stansim_simulation class
  attributes(object)$class <- NULL

  # list the remaining object
  sim_list <- list(object)

  # name single list entry
  names(sim_list) <- object$sim_name

  # remove original name
  sim_list[[1]]$sim_name <- NULL

  # call stansim_collector constructor and return
  stansim_collection(collection_name = collection_name,
                     data = sim_to_coll_data,
                     refitted = sim_to_coll_refitted,
                     simulations = sim_list)

}
