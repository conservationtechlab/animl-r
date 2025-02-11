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
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' files <- build_file_manifest("C:\\Users\\usr\\Pictures\\")
#' }
build_file_manifest <- function(image_dir, exif=TRUE, out_file=NULL, 
                                offset=0, recursive=TRUE) {
  
  if (check_file(out_file)) { return(load_data(out_file)) }
  
  if (!dir.exists(image_dir)) { stop("The given directory does not exist.") }
  
  # Reads files in directory and extracts their EXIF data
  if (exif) {
    files <- tryCatch( 
      {
        exifr::read_exif(image_dir, recursive = recursive,
                         tags = c("filename", "FileModifyDate", "CreateDate", 
                                  "File:ImageWidth", "File:ImageHeight"))
      },
      error = function(cond) { return(NULL) },
      warning = function(cond) {},
      finally = {}
    )
    if (length(files) == 0) {
      files <- list.files(image_dir, full.names=TRUE, recursive = recursive)
      files <- as.data.frame(files)
    }
    
    colnames(files)[1] <- "FilePath"
    files <- as.data.frame(files)
    colnames(files)[colnames(files) == 'ImageWidth'] <- 'Width'
    colnames(files)[colnames(files) == 'ImageHeight'] <- 'Height'
    
    files$FileModifyDate <- as.POSIXct(files$FileModifyDate, format="%Y:%m:%d %H:%M:%S") + (offset*3600)
    
    # establish datetime
    if ("CreateDate" %in% names(files)){
      files$CreateDate <- as.POSIXct(files$CreateDate, format="%Y:%m:%d %H:%M:%S")
      files <- files %>% dplyr::mutate("DateTime" = dplyr::coalesce(files$CreateDate, 
                                                           files$FileModifyDate))
    }
    # Unable to get CreateDate from exif
    else { files$DateTime = files$FileModifyDate }
  }
  # return simple file list 
  else {
    files <- list.files(image_dir, full.names = TRUE, recursive = recursive)
    files <- as.data.frame(files)
    colnames(files)[1] <- "FilePath"
  }
  
  files$FileName <- sapply(files$FilePath, function(x) basename(x))
  files$Extension <- sapply(files$FilePath, function(x) tolower(tools::file_ext(x)))
  
  # only keep images and videos
  VALID_EXTENSIONS = c('png', 'jpg', 'jpeg', "tiff",
                       "mp4", "avi", "mov", "wmv",
                       "mpg", "mpeg", "asf", "m4v")
  files <- files %>% dplyr::filter(.data[["Extension"]] %in% VALID_EXTENSIONS)
  
  #save output
  if (!is.null(out_file)) { save_data(files, out_file) }
  
  return(files)
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
  
  # Create directories if they do not already exist
  dir.create(pkg.env$datadir, recursive = T, showWarnings = F)
  dir.create(pkg.env$vidfdir, recursive = T, showWarnings = F)
  dir.create(pkg.env$linkdir, recursive = T, showWarnings = F)
  
  # Assign specific file paths
  pkg.env$filemanifest <- paste0(pkg.env$datadir, "FileManifest.csv")
  pkg.env$imageframes <- paste0(pkg.env$datadir, "ImageFrames.csv")
  pkg.env$reults <- paste0(pkg.env$datadir, "Results.csv")
  pkg.env$predictions <- paste0(pkg.env$datadir, "Predictions.csv")
  pkg.env$detections <- paste0(pkg.env$datadir, "Detections.csv")
  pkg.env$mdraw <- paste0(pkg.env$datadir, "MD_Raw.json")
}

# active_times <- function(){}

