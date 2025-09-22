#' File Management Module
#'
#' This module provides functions and classes for managing files and directories.
#'
#' Kyra Swanson 2023


#' Find Image/Video Files and Gather exif Data
#
#' @param image_dir folder to search through and find media files
#' @param exif returns date and time information from exif data, defaults to true
#' @param offset add offset in hours for videos when using the File Modified date, defaults to 0
#' @param out_file directory to save .csv of manifest to
#' @param recursive Should directories be scanned recursively? Default TRUE
#'
#' @return files dataframe with or without file dates
#' @export
#'
#' @examples
#' \dontrun{
#' files <- build_file_manifest("C:\\Users\\usr\\Pictures\\")
#' }
build_file_manifest <- function(image_dir, exif=TRUE, out_file=NULL, 
                                offset=0, recursive=TRUE) {
  animl_py$build_file_manifest(image_dir, exif=exif, out_file=out_file, offset=offset, recursive=recursive)
}


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
#' WorkingDirectory(/home/kyra/animl/examples)
#' }
WorkingDirectory <- function(workingdir, pkg.env) {
  
  if (!dir.exists(workingdir)) { stop("Output directory invalid.\n") }
  if (!endsWith(workingdir, "/")) { workingdir <- paste0(workingdir,"/") }
  
  # Assign specific directory paths
  basedir <- paste0(workingdir, "Animl-Directory/")
  pkg.env$datadir <- paste0(basedir, "Data/")
  pkg.env$vidfdir <- paste0(basedir, "Frames/")
  pkg.env$linkdir <- paste0(basedir, "Sorted/")
  pkg.env$visdir <- paste0(basedir, "Plots/")
  
  # Create directories if they do not already exist
  dir.create(pkg.env$datadir, recursive = T, showWarnings = F)
  dir.create(pkg.env$vidfdir, recursive = T, showWarnings = F)
  dir.create(pkg.env$linkdir, recursive = T, showWarnings = F)
  
  # Assign specific file paths
  pkg.env$filemanifest <- paste0(pkg.env$datadir, "FileManifest.csv")
  pkg.env$imageframes <- paste0(pkg.env$datadir, "ImageFrames.csv")
  pkg.env$results <- paste0(pkg.env$datadir, "Results.csv")
  pkg.env$predictions <- paste0(pkg.env$datadir, "Predictions.csv")
  pkg.env$detections <- paste0(pkg.env$datadir, "Detections.csv")
  pkg.env$mdraw <- paste0(pkg.env$datadir, "MD_Raw.json")
}


#' Save Data to Given File
#'
#' @param data the dataframe to be saved
#' @param out_file the full path of the saved file
#' @param prompt if true, prompts the user to confirm overwrite
#'
#' @return none
#'
#' @examples
#' \dontrun{
#'  saveData(files,"path/to/newfile.csv")
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
#'   loadData("path/to/newfile.csv")
#' }

load_data <- function(file) {
  ext <- strsplit(basename(file), split="\\.")[[1]][-1]
  if (tolower(ext) == "csv") { return(utils::read.csv(file)) }
  else{ stop("Error. Expecting a .csv file.") }
}


#' Check for files existence and prompt user if they want to load
#'
#' @param file the full path of the file to check
#'
#' @return a boolean indicating wether a file was found 
#'             and the user wants to load or not
#' @importFrom methods is
#'
#' @examples
#' \dontrun{
#'   checkFile("path/to/newfile.csv")
#' }
check_file <- function(file) {
  if (!is.null(file) && file.exists(file)) {
    date <- file.info(file)$mtime
    date <- strsplit(date, split = " ")[[1]][1]
    if (tolower(readline(prompt = sprintf("Output file already exists and was last modified %s, would you like to load it? y/n: ", date)) == "y")) {
      return(TRUE)
    }
  }
  FALSE
}

