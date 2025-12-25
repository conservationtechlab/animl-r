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
#' @param export_dir directory to sort files into
#' @param unique_name column name indicating a unique file name for each row
#'
#' @return dataframe with new "Species" column that contains the verified species
#' @export
#'
#' @examples
#' \dontrun{
#' results <- update_labels_from_folders(manifest, export_dir)
#' }
update_labels_from_folders <- function(manifest, export_dir, unique_name='uniquename'){
  if (!dir.exists(export_dir)) {stop("The given directory does not exist.")}
  if (!unique_name %in% names(manifest)) {stop("Manifest does not have unique names, cannot match to sorted directories.")}
  
  FilePath <- list.files(export_dir, recursive = TRUE, include.dirs = TRUE)
  files <- data.frame(FilePath)
  
  files[unique_name] <- sapply(files$FilePath,function(x)strsplit(x,"/")[[1]][2])
  files$label <- sapply(files$FilePath,function(x)strsplit(x,"/")[[1]][1])
  
  corrected <- merge(manifest, files, by=unique_name)
  return(corrected)
}


#' Export a manifest to COCO format.
#'
#' @param manifest dataframe containing images and associated predictions
#' @param class_list dataframe containing class names and their corresponding IDs
#' @param out_file path to save the COCO formatted file
#' @param info optional info section of COCO file
#' @param licenses optional licenses section of COCO file
#'
#' @returns coco formatted json file saved to out_file
#' @export
#'
#' @examples
#' \dontrun{
#' export_coco(manifest, classes, "path/to/out.json")
#' }
export_coco <- function(manifest, class_list, out_file, info=NULL, licenses=NULL){
  animl_py <- get("animl_py", envir = parent.env(environment()))
  animl_py$export_coco(manifest, class_list, out_file, info=info, licenses=licenses)
}

#' Converts the .csv file to the MD-formatted .json file.
#'
#' @param manifest dataframe containing images and associated detections
#' @param output_file path to save the MD formatted file
#' @param detector name of the detector model used
#' @param prompt whether to prompt before overwriting existing file
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{export_megadetector(manifest, output_file= 'results.json', detector='MDv6')}
export_megadetector <- function(manifest, output_file=NULL, detector='MegaDetector v5a', prompt=TRUE){
  animl_py <- get("animl_py", envir = parent.env(environment()))
  animl_py$export_megadetector(manifest, output_file=output_file, detector=detector, prompt=prompt)
}


#' Converts the Manifests to a csv file that contains columns needed for TimeLapse conversion in later step
#'
#' @param results a DataFrame that has entries of anuimal classification
#' @param image_dir location of root directory where all images are stored (can contain subdirectories)
#' @param only_animal A bool that confirms whether we want only animal detctions or all
#'
#' @returns animals.csv, non-anim.csv, csv_loc
#' @export 
#'
#' @examples
#' \dontrun{export_timelapse(animals, empty, '/path/to/images/')}
export_timelapse <- function(results, image_dir, only_animal=TRUE){
  animl_py <- get("animl_py", envir = parent.env(environment()))
  animl_py$export_timelapse(results, image_dir, only_animal=only_animal)
}
