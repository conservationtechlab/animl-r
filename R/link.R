#' Create SymLink Directories and Sort Classified Images
#'
#' @param manifest DataFrame of classified images 
#' @param link_dir Destination directory for symlinks
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
sort_species <- function(manifest, link_dir, file_col="FilePath", unique_name='UniqueName', copy=FALSE) {
  
  # create species directories
  for (s in unique(manifest$prediction)) {
    dir.create(paste0(link_dir, s), recursive = TRUE,  showWarnings = FALSE)
  }
  
  if (!unique_name %in% names(manifest)) {
    manifest[unique_name] <- sapply( manifest[[file_col]], function(x) paste0(strsplit(basename(x), ".", fixed = T)[[1]][1],
                                                           "_", sprintf("%05d", round(stats::runif(1, 1, 99999), 0)),
                                                           ".", tools::file_ext(x)))
    }
    
  manifest$Link <- paste0(link_dir, manifest$prediction, "/", manifest[[unique_name]])
  
  # hard copy or link
  if (copy) { mapply(file.copy, manifest[[file_col]], manifest$Link, MoreArgs = list(copy.date=TRUE))}
  else { mapply(file.link, manifest[[file_col]], manifest$Link) }
  
  manifest
}


#' Create SymLink Directories and Sort Classified Images Based on MD Results
#'
#' @param manifest DataFrame of classified images 
#' @param link_dir Destination directory for symlinks
#' @param file_col Colun containing file paths
#' @param unique_name Unique image name identifier 
#' @param copy Toggle to determine copy or hard link, defaults to link
#'
#' @return manifest with added link columns
#' @export
#'
#' @examples
#' \dontrun{
#' sort_MD(manifest, link_dir)
#' }
sort_MD <- function(manifest, link_dir, file_col="FilePath", unique_name='UniqueName', copy=FALSE){

  # create directories
  MDclasses <- c("empty", "animal", "human", "vehicle")
  for (s in MDclasses) {
    dir.create(paste0(link_dir, s), recursive = TRUE,  showWarnings = FALSE)
  }
  
  manifest$MD_prediction <- sapply(manifest$category, function(x) MDclasses[as.integer(x)+1])
  
  if (!unique_name %in% names(manifest)) {
    manifest[unique_name] <- sapply( manifest[[file_col]], function(x) paste0(strsplit(basename(x), ".", fixed = T)[[1]][1],
                                                                              "_", sprintf("%05d", round(stats::runif(1, 1, 99999), 0)),
                                                                              ".", tools::file_ext(x)))
  }
  
  manifest$Link <- paste0(link_dir, manifest$MD_prediction, "/", manifest[[unique_name]])
  
  # hard copy or link
  if (copy) { mapply(file.copy, manifest[[file_col]], manifest$Link, MoreArgs = list(copy.date=TRUE))}
  else { mapply(file.link, manifest[[file_col]], manifest$Link) }
  
  manifest
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
