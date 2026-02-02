#' Create SymLink Directories and Sort Classified Images
#'
#' @param manifest DataFrame of classified images 
#' @param out_dir Destination directory for species folders
#' @param out_file if provided, save the manifest to this file
#' @param label_col specify 'prediction' for species or 'category' for megadetector class
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
  animl_py <- .animl_internal$animl_py
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
#' @param unique_name column containing unique file names
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


#' Converts the .csv file to a COCO-formatted .json file.
#'
#' @param manifest dataframe containing images and associated detections
#' @param class_list  dataframe containing class names and their corresponding IDs
#' @param out_file path to save the formatted file
#' @param info info section of COCO file, named list
#' @param licenses licenses section of COCO file, array
#'
#' @return coco formated json
#' @export
#'
#' @examples
#' \dontrun{export_megadetector(manifest, output_file= 'results.json', detector='MDv6')}
export_coco <- function(manifest, class_list, out_file, info=NULL, licenses=NULL){
  animl_py <- .animl_internal$animl_py
  animl_py$export_coco(manifest, class_list, out_file, info=info, licenses=licenses)
}



#' Export data into sorted folders organized by station
#'
#' @param manifest dataframe containing images and associated predictions
#' @param out_dir directory to export sorted images
#' @param out_file if provided, save the manifest to this file
#' @param label_col column containing species labels
#' @param file_col column containing source paths
#' @param station_col column containing station names
#' @param unique_name column containing unique file name
#' @param copy if true, hard copy
#'
#' @returns manifest with link column
#' @export
#'
#' @examples
#' \dontrun{manifest <- export_camtrapR(manifest, out_dir, out_file=NULL, label_col='prediction',
#'                                      file_col="filepath", station_col='station', 
#'                                      unique_name='uniquename', copy=FALSE)}
export_camtrapR <- function(manifest, out_dir, out_file=NULL, label_col='prediction',
                            file_col="filepath", station_col='station', 
                            unique_name='uniquename', copy=FALSE){
  animl_py <- .animl_internal$animl_py
  animl_py$export_camtrapR(manifest, out_dir, out_file=out_file, label_col=label_col,
                           file_col=file_col, station_col=station_col,
                           unique_name=unique_name, copy=copy)
}


#' Converts the Manifests to a csv file that contains columns needed for TimeLapse conversion in later step
#'
#' @param manifest a DataFrame that has entries of anuimal classification
#' @param out_dir location of root directory where all images are stored (can contain subdirectories)
#' @param only_animal A bool that confirms whether we want only animal detctions or all
#'
#' @returns animals.csv, non-anim.csv, csv_loc
#' @export 
#'
#' @examples
#' \dontrun{export_timelapse(animals, empty, '/path/to/images/')}
export_timelapse <- function(manifest, out_dir, only_animal=TRUE){
  animl_py <- .animl_internal$animl_py
  animl_py$export_timelapse(manifest, out_dir, only_animal=only_animal)
}


#' Converts the .csv file to the MD-formatted .json file.
#'
#' @param manifest dataframe containing images and associated detections
#' @param out_file path to save the MD formatted file
#' @param detector name of the detector model used
#' @param prompt ask user to overwrite existing file
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{export_megadetector(manifest, output_file= 'results.json', detector='MDv6')}
export_megadetector <- function(manifest, out_file=NULL, 
                                detector='MegaDetector v5a', prompt=TRUE){
  animl_py <- .animl_internal$animl_py
  animl_py$export_megadetector(manifest, out_file=out_file, 
                               detector=detector, prompt=prompt)
}


