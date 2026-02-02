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
#' @param offset add offset in hours for videos when using the File Modified date, defaults to 0
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
  animl_py <- .animl_internal$animl_py
  manifest <- animl_py$build_file_manifest(image_dir, exif=exif, out_file=out_file, offset=offset, recursive=recursive)
  return(manifest)
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
#' @param output_type str to specify file name in prompt description
#'
#' @return a boolean indicating wether a file was found 
#'             and the user wants to load or not
#' @importFrom methods is
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