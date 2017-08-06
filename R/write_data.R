#-----------------------------------------------------------------
#### internal write_data function ####
#' Write data from a \code{stansim_data} object to .rds files
#'
#' @description \code{write_data()} takes a \code{stansim_data} object and
#'   writes each of the objects' simulated datasets to an individual .rds file
#'   in a given directory.
#'
#' @param object An object of S3 class stansim_data.
#' @param path The name of the directory to write the .rds datafiles to. If the
#'   directory does not exist it will be created, if NULL then the current
#'   working directory is used. it's recommended that this is the name of an
#'   empty or non-existant directory.
#' @param data_name Controls the name stem for all saved .rds files. If NULL
#'   this will be the \code{data_name} property from the provided
#'   \code{stansim_data} object, otherwise it will be the provided character.
#'   Default value is NULL.
#' @param recursive logical. Should elements of the path other than the last be
#'   created? If true, like the Unix command mkdir -p.
#' @return NULL
#'
#' @examples
#' \dontrun{
#' # write the contents of a stansim_data object
#' write_data(object = data_object, path = "save_to_dir", data_name = "simdata")
#' }
#'
#'
write_data <- function(object, path = NULL, data_name = NULL, recursive = TRUE) {

  #-----------------------------------------------------------------
  #### input checks ####
  # class of object must be stansim_data
  if (class(object) != "stansim_data")
    stop("object must be of class stansim_data")

  # path must be character
  if (typeof(path) != "character")
    stop("path must be of type character")

  # dataname must be character or NULL
  if (typeof(data_name) != "character" & !is.null(data_name))
    stop("data_name must be NULL or of type character")

  # recursive must be logical
  if (!is.logical(recursive))
    stop("recursive must be of type logical")

  #-----------------------------------------------------------------
  #### create directory if doesn't exist ####
  if(!is.null(path) & !dir.exists(path))
    dir.create(path = path, recursive = recursive)

  #-----------------------------------------------------------------
  #### create list of data with datafile name ####
  # extract data files from object
  unnamed_data <- object$data

  # prep name stem
  if(is.null(data_name)){
    name_stem <- object$data_name
  } else {
    name_stem <- data_name
  }

  # write names vector for all objects
  names_vector <- paste0(name_stem, "_", seq(length(unnamed_data)))

  # attach names to all object data
  named_data <- stats::setNames(unnamed_data, names_vector)

  #-----------------------------------------------------------------
  #### write data to rds files ####
  # function to write each file
  write_named_data <- function(name_list, obj_list){
    saveRDS(obj_list[[name_list]], file = paste0(path, "/", name_list, ".rds"))
  }

  catch <- lapply(names(named_data), write_named_data, named_data)

}
