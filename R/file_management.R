#' File Management Module
#'
#' This module provides functions and classes for managing files and directories.
#'
#' Kyra Swanson 2023


#' Find Image/Video Files and Gather exif Data
#
#' @param image_dir folder to search through and find media files
#' @param exif returns date and time information from exif data, defaults to true
#' @param out_file .csv file to save manifest as
#' @param data_timezone timezone in which data was collected
#' @param station_depth integer value indicating folder depth from root image_dir that contains station name
#' @param camera_depth integer value indicating folder depth from root image_dir that contains camera name
#' @param recursive Should directories be scanned recursively? Default TRUE
#'
#' @return files dataframe with or without file dates
#' @export
#'
#' @examples
#' \dontrun{
#' files <- build_file_manifest("C:\\Users\\usr\\Pictures\\")
#' }
build_file_manifest <- function(image_dir,
                                exif=TRUE,
                                out_file=NULL,
                                data_timezone=NULL, 
                                station_depth=NULL,
                                camera_depth=NULL,
                                recursive=TRUE) {
  
  animl_py <- .animl_internal$animl_py
  manifest <- animl_py$build_file_manifest(image_dir,
                                           exif=exif,
                                           out_file=out_file, 
                                           data_timezone=data_timezone,
                                           station_depth=station_depth,
                                           camera_depth=camera_depth,
                                           recursive=recursive)
  #convert to posix
  if (exif){
    manifest$datetime <- as.POSIXct(manifest$datetime, format = "%Y-%m-%d %H:%M:%S")
    manifest$createdate <- as.POSIXct(manifest$createdate, format = "%Y-%m-%d %H:%M:%S")
    manifest$filemodifydate <- as.POSIXct(manifest$filemodifydate, format = "%Y-%m-%d %H:%M:%S")
  }
  return(manifest)
}


#manifest$createdate <- as.POSIXct(manifest$createdate)
#manifest$filemodifydate <- as.POSIXct(manifest$filemodifydate)
#manifest$datetime <- as.POSIXct(manifest$datetime)
#' Set Working Directory and Save File Global Variables
#'
#' @param workingdir local directory that contains data to process
#' @param pkg.env environment to create global variables in
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' WorkingDirectory("/home/kyra/animl/examples",globalenv())
#' }
WorkingDirectory <- function(workingdir, pkg.env) {
  
  if (!dir.exists(workingdir)) { stop("Output directory invalid.\n") }
  if (!endsWith(workingdir, "/")) { workingdir <- paste0(workingdir,"/") }
  
  # Assign specific directory paths
  basedir <- paste0(workingdir, "Animl-Directory/")
  pkg.env$linkdir <- paste0(basedir, "Sorted/")
  pkg.env$visdir <- paste0(basedir, "Plots/")
  
  # Create directories if they do not already exist
  dir.create(pkg.env$linkdir, recursive = T, showWarnings = F)
  dir.create(pkg.env$visdir, recursive = T, showWarnings = F)
  
  # Assign specific file paths
  pkg.env$filemanifest_file <- paste0(basedir, "FileManifest.csv")
  pkg.env$imageframes_file <- paste0(basedir, "ImageFrames.csv")
  pkg.env$results_file <- paste0(basedir, "Results.csv")
  pkg.env$predictions_file <- paste0(basedir, "Predictions.csv")
  pkg.env$detections_file <- paste0(basedir, "Detections.csv")
  pkg.env$mdraw_file <- paste0(basedir, "MD_Raw.json")
}


#' Save Data to Given File
#'
#' @param data the dataframe to be saved
#' @param out_file the full path of the saved file
#' @param prompt if true, prompts the user to confirm overwrite
#'
#' @return none
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#'  save_data(files,"path/to/newfile.csv")
#' }
save_data <- function(data, out_file, prompt=TRUE) {
  if (file.exists(out_file) & prompt == TRUE) {
    if (tolower(readline(prompt = "Output file exists, would you like to overwrite? y/n: ")) == "y") {
      utils::write.csv(data, file = out_file, row.names = F, quote = F)
    }
  } 
  else { utils::write.csv(data, file = out_file, row.names = F, quote = F) }
}


#' Load .csv or .Rdata file 
#'
#' @param file the full path of the file to load
#'
#' @return data extracted from the file
#' @export
#'
#' @examples
#' \dontrun{
#'   load_data("path/to/newfile.csv")
#' }

load_data <- function(file) {
  ext <- strsplit(basename(file), split="\\.")[[1]][-1]
  if (tolower(ext) == "csv") { return(utils::read.csv(file)) }
  else{ stop("Error. Expecting a .csv file.") }
}


#' Check for files existence and prompt user if they want to load
#'
#' @param file the full path of the file to check
#' @param output_type str to specify file name in prompt description
#'
#' @return a boolean indicating wether a file was found 
#'             and the user wants to load or not
#' @importFrom methods is
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#'   check_file("path/to/newfile.csv")
#' }
check_file <- function(file, output_type) {
  if (!is.null(file) && file.exists(file)) {
    date <- file.info(file)$mtime
    date <- strsplit(date, split = " ")[[1]][1]
    prompt = sprintf("%s file already exists and was last modified %s, would you like to load it? y/n: ", output_type, date)
    if (tolower(readline(prompt = prompt) == "y")) {
      return(TRUE)
    }
  }
  FALSE
}


#' Save data to a JSON file.
#'
#' @param data the dictionary to be saved
#' @param out_file full path to save file to
#' @param prompt prompt user to confirm overwrite
#'
#' @returns None
#' @export
#'
#' @examples
#' \dontrun{
#' save_json(mdresults, 'mdraw.json')
#' }
save_json <- function(data, out_file, prompt=TRUE){
  animl_py <- .animl_internal$animl_py
  animl_py$save_json(data, out_file, prompt=prompt)
}


#' Load data from a JSON file.
#'
#' @param file the full path of the file to load
#'
#' @returns loaded json file
#' @export
#'
#' @examples
#' \dontrun{
#' mdraw <- load_json('mdraw.json')
#' }
load_json <- function(file){
  animl_py <- .animl_internal$animl_py
  animl_py$load_json(file)
}


#' Save data to a YAML file
#'
#' @param data the dictionary to be saved
#' @param out_file full path to save file to
#' @param prompt prompt user to confirm overwrite
#'
#' @returns None
#' @export
#'
#' @examples
#' \dontrun{
#' save_yaml(config, 'config.yml')
#' }
save_yaml <- function(data, out_file, prompt=TRUE){
  animl_py <- .animl_internal$animl_py
  animl_py$save_yaml(data, out_file, prompt=prompt)
  
}


#' Load data from a YAML file.
#'
#' @param file the full path of the file to load
#'
#' @returns data extracted from the file, dict form
#' @export
#'
#' @examples
#' \dontrun{
#' config <- load_yaml('config.yml')
#' }
load_yaml <- function(file){
  animl_py <- .animl_internal$animl_py
  animl_py$load_yaml(file)
}


#' Download specified model to the given directory.
#'
#' @param model_url url of the model to download, obtained via the constants above
#' @param out_dir Directory to save the model.
#'
#' @returns None
#' @export
#'
#' @examples
#' \dontrun{
#'   list_models()
#'   download_model("https://models.com/path/to/model.pt", out_dir='models')
#' }
download_model <- function(model_url, out_dir='models'){
  animl_py <- .animl_internal$animl_py
  animl_py$download_model(model_url, out_dir = out_dir)
}

#' List available models for download.
#'
#' @returns printout of models
#' @export
#'
#' @examples
#' \dontrun{
#'   list_models()
#'   download_model("https://models.com/path/to/model.pt", out_dir='models')
#' }
list_models <- function(){
  animl_py <- .animl_internal$animl_py
  animl_py$list_models()
}


#' Get start and stop dates for each camera folder.
#'
#' @param manifest_dir either file manifest or directory of files to analyze
#' @param file_col column in manifest to use for file paths, defaults to "filepath"
#' @param camera_depth directory depth from which to split cameras
#' @param timestamp_col column in manifest to use for datetime information, defaults to "datetime"
#'
#' @returns times dataframe with min and max timestamp per camera
#' @export
#'
#' @examples
#' \dontrun{
#' active_times('path/to/data', recursive=TRUE, camera_depth=2)
#' }
active_times <- function(manifest_dir,
                         file_col='filepath',
                         camera_depth=0,
                         timestamp_col="datetime"){

  animl_py <- .animl_internal$animl_py
  animl_py$active_times(manifest_dir=manifest_dir,
                        file_col=file_col, 
                        camera_depth=camera_depth,
                        timestamp_col=timestamp_col)
}


#' Calculate sequence from timestamps
#'
#' @param manifest dataframe of images with station and timestamp columns
#' @param station_col a column in the animals and empty data frame that indicates the camera or camera station
#' @param sort_columns list of columns to sort by before calculating sequences. Defaults to None, which sorts by station_col and timestamp_col.
#' @param file_col column name representing the file path. Defaults to "filepath".
#' @param timestamp_col column name representing the timestamp in format "%Y-%m-%d %H:%M:%S". Defaults to "datetime".
#' @param maxdiff max time difference in seconds between sequences, default = 60
#'
#' @returns manifest with sequence column, with a unique number associated with each sequence
#' @export
#'
#' @examples
#' \dontrun{
#' manifest <- sequence_calculation(manifest,'station')
#' }
sequence_calculation <- function(manifest,
                                 station_col,
                                 sort_columns = NULL,
                                 file_col = 'filepath',
                                 timestamp_col = "datetime",
                                 maxdiff = 60) {
  # input validation
  if (!is.character(station_col) || nchar(station_col) == 0) {
    stop("'station_col' must be a non-empty string")
  }
  if (!is.numeric(maxdiff) || maxdiff < 0) {
    stop("'maxdiff' must be a number >= 0")
  }
  if (!timestamp_col %in% colnames(manifest)) {
    stop(paste0("DataFrame must contain '", timestamp_col, "' column."))
  }
  # parse datetime
  manifest[[timestamp_col]] <- as.POSIXct(manifest[[timestamp_col]], format = "%Y-%m-%d %H:%M:%S")
  
  # sort
  if (is.null(sort_columns)){
    sort_columns <- c(station_col, timestamp_col)
  }
  
  manifest_sort <- manifest[do.call(order, manifest[, sort_columns, drop = FALSE]), ]
  manifest_sort <- manifest_sort[, , drop = FALSE]
  rownames(manifest_sort) <- NULL
  
  n <- nrow(manifest_sort)
  sequence_placeholder <- integer(n)
  
  i <- 1
  s <- 0
  while (i <= n) {
    rows <- i
    last_index <- i + 1
    
    while (last_index <= n &&
           !is.na(manifest_sort[i, timestamp_col]) &&
           !is.na(manifest_sort[last_index, timestamp_col]) &&
           manifest_sort[[station_col]][last_index] == manifest_sort[[station_col]][i] &&
           as.numeric(difftime(manifest_sort[last_index, timestamp_col],
                               manifest_sort[i, timestamp_col],
                               units = "secs")) <= maxdiff) {
      rows <- c(rows, last_index)
      last_index <- last_index + 1
    }
    
    sequence_placeholder[rows] <- s
    
    i <- last_index
    s <- s + 1
  }
  
  manifest_sort$sequence <- sequence_placeholder
  
  return(manifest_sort)
}