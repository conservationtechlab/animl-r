#' Create SymLink Directories and Sort Classified Images
#'
#' @param manifest DataFrame of classified images 
#' @param out_dir Destination directory for symlinks
#' @param out_file 
#' @param file_col Colun containing file paths
#' @param unique_name Unique image name identifier 
#' @param copy Toggle to determine copy or hard link, defaults to link
#'
#' @return manifest with added link columns
#' @export 
#'
#' @examples
#' \dontrun{
#' manifest <- sort_species(manifest, link_dir)
#' }
export_folders <- function(manifest, out_dir, out_file, file_col="FilePath", label_col="prediction",
                           unique_name='UniqueName', copy=FALSE) {
  
  if(reticulate::py_module_available("animl")){ animl_py <- reticulate::import("animl")}
  else{ stop('animl-py environment must be loaded first via reticulate')}
  
  manifest <- animl_py$export_folders(manifest, out_dir, out_file, file_col=file_col, unique_name=unique_name, copy=copy)
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
remove_link <- function(manifest, link_col='Link'){
  pbapply::pbapply(manifest[link_col], file.remove)
  manifest <- manifest[, !names(manifest) %in% c(link_col)]
  manifest
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
#' results <- updateResults(resultsfile, linkdir)
#' }
update_labels <- function(manifest, link_dir, unique_name='UniqueName'){
  if (!dir.exists(link_dir)) {stop("The given directory does not exist.")}
  if (!unique_name %in% names(manifest)) {stop("Manifest does not have unique names, cannot match to sorted directories.")}
  
  FilePath <- list.files(link_dir, recursive = TRUE, include.dirs = TRUE)
  files <- data.frame(FilePath)
  
  files[unique_name] <- sapply(files$FilePath,function(x)strsplit(x,"/")[[1]][2])
  files$label <- sapply(files$FilePath,function(x)strsplit(x,"/")[[1]][1])
  
  corrected <- merge(manifest, files, by=unique_name)
  return(corrected)
}
