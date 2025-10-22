#' Create SymLink Directories and Sort Classified Images
#'
#' @param manifest DataFrame of classified images 
#' @param out_dir Destination directory for symlinks
#' @param out_file if provided, save the manifest to this file
#' @param label_col (str): specify 'prediction' for species or 'category' for megadetector class
#' @param file_col Colun containing file paths
#' @param unique_name Unique image name identifier 
#' @param copy Toggle to determine copy or hard link, defaults to link
#'
#' @return manifest with added link columns
#' @export 
#'
#' @examples
#' \dontrun{
#' manifest <- export_folders(manifest, out_dir)
#' }
export_folders <- function(manifest, out_dir, out_file=NULL, 
                           label_col="prediction", file_col="filepath",
                           unique_name='uniquename', copy=FALSE) {
  animl_py <- get("animl_py", envir = parent.env(environment()))
  manifest <- animl_py$export_folders(manifest, out_dir, out_file,
                                      label_col=label_col, file_col=file_col,
                                      unique_name=unique_name, copy=copy)
  return(manifest)
}


#' Remove Sorted Links
#'
#' @param manifest DataFrame of classified images 
#' @param link_col column in manifest that contains link paths
#'
#' @return manifest without link column
#' @export
#'
#' @examples
#' \dontrun{
#' remove_link(manifest)
#' }
remove_link <- function(manifest, link_col='link'){
  pbapply::pbapply(manifest[link_col], file.remove)
  manifest <- manifest[, !names(manifest) %in% c(link_col)]
  return(manifest)
}


#' Udate Results from File Browser
#'
#' @param manifest dataframe containing file data and predictions
#' @param link_dir directory to sort files into
#' @param unique_name column name indicating a unique file name for each row
#'
#' @return dataframe with new "Species" column that contains the verified species
#' @export
#'
#' @examples
#' \dontrun{
#' results <- update_labels_from_folders(manifest, link_dir)
#' }
update_labels_from_folders <- function(manifest, link_dir, unique_name='uniquename'){
  if (!dir.exists(link_dir)) {stop("The given directory does not exist.")}
  if (!unique_name %in% names(manifest)) {stop("Manifest does not have unique names, cannot match to sorted directories.")}
  
  FilePath <- list.files(link_dir, recursive = TRUE, include.dirs = TRUE)
  files <- data.frame(FilePath)
  
  files[unique_name] <- sapply(files$FilePath,function(x)strsplit(x,"/")[[1]][2])
  files$label <- sapply(files$FilePath,function(x)strsplit(x,"/")[[1]][1])
  
  corrected <- merge(manifest, files, by=unique_name)
  return(corrected)
}


#' Converts the .csv file to the MD-formatted .json file.
#'
#' @param manifest dataframe containing images and associated detections
#' @param output_file path to save the MD formatted file
#' @param detector name of the detector model used
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{export_megadetector(manifest, output_file= 'results.json', detector='MDv6')}
export_megadetector <- function(manifest, output_file=NULL, detector='MegaDetector v5a'){
  animl_py <- get("animl_py", envir = parent.env(environment()))
  animl_py$export_megadetector(manifest, output_file=output_file, detector=detector)
}


#' Converts the Manifests to a csv file that contains columns needed for TimeLapse conversion in later step
#'
#' @param animals a DataFrame that has entries of anuimal classification
#' @param empty a DataFrame that has detection of non-animal objects in images
#' @param imagedir location of root directory where all images are stored (can contain subdirectories)
#' @param only_animal A bool that confirms whether we want only animal detctions or all
#'
#' @returns animals.csv, non-anim.csv, csv_loc
#' @export 
#'
#' @examples
#' \dontrun{export_timelapse(animals, empty, '/path/to/images/')}
export_timelapse <- function(animals, empty, imagedir, only_animal=TRUE){
  animl_py <- get("animl_py", envir = parent.env(environment()))
  animl_py$export_timelapse(animals, empty, imagedir, only_animal=only_animal)
}
