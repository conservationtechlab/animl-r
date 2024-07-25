#' Create SymLink Directories and Sort Classified Images
#'
#' @param manifest DataFrame of classified images 
#' @param linkdir Destination directory for symlinks
#' @param threshold Confidence threshold for determining uncertain predictions, defaults to 0
#' @param outfile Results file to save to
#' @param copy Toggle to determine copy or hard link, defaults to link
#' 
#'
#' @return manifest with added link columns
#' @export 
#'
#' @examples
#' \dontrun{
#' manifest <- symlinkSpecies(manifest, linkdir)
#' }
symlinkSpecies <- function(manifest, linkdir, threshold = 0, outfile = NULL, copy = FALSE) {

  # place low-confidence images into "Unknown" category
  manifest$prediction[manifest$confidence < threshold &
    !(manifest$prediction %in% c("empty", "Empty", "human", "Human", "vehicle", "Vehicle"))] <- "unknown"

  # create species directories
  for (s in unique(manifest$prediction)) {
    if (!dir.exists(paste0(linkdir, s))) dir.create(paste0(linkdir, s), recursive = T)
  }
  
  if (!("UniqueName" %in% names(manifest))) {
    manifest$UniqueName<-sapply(manifest$FileName, function(x) paste0(strsplit(x, ".", fixed = T)[[1]][1], "_", 
                                                                      sprintf("%05d", round(stats::runif(1, 1, 99999), 0)), ".jpg"))
  }
  
  manifest$Link <- paste0(linkdir, manifest$prediction, "/", manifest$UniqueName)

  # hard copy or link
  if (copy) { mapply(file.copy, manifest$FilePath, manifest$Link) }
  else { mapply(file.link, manifest$FilePath, manifest$Link) }
  
  if (!is.null(outfile)) { saveData(manifest, outfile)}
  manifest
}


#' Create SymLink Directories and Sort Classified Images Based on MD Results
#'
#' @param manifest DataFrame of classified images 
#' @param linkdir Destination directory for symlinks
#' @param outfile Results file to save to
#' @param copy Toggle to determine copy or hard link, defaults to link
#'
#' @return manifest with added link columns
#' @export
#'
#' @examples
#' \dontrun{
#' symlinkMD(manifest, linkdir)
#' }
symlinkMD <- function(manifest, linkdir, outfile = NULL, copy=FALSE){
  if (checkFile(outfile)) { return(loadData(outfile))}
  
  MDclasses <- c("empty","animal","human","vehicle")
  for (s in MDclasses) {
    if (!dir.exists(paste0(linkdir, s))) {
      dir.create(paste0(linkdir, s), recursive = T)
    }
  }
  
  manifest$MDprediction <- sapply(manifest$max_detection_category,function(x) MDclasses[x+1])
  
  if (!("UniqueName" %in% names(manifest))) {
    manifest$UniqueName<-sapply(manifest$FileName, function(x) paste0(strsplit(x, ".", fixed = T)[[1]][1], "_", 
                                                                sprintf("%05d", round(stats::runif(1, 1, 99999), 0)), ".jpg"))
  }
  
  manifest$MDLink <- paste0(linkdir, manifest$MDprediction, "/", manifest$UniqueName)

  # hard copy or link
  if (copy) { 
    pbapply::pb.mapply(x = manifest$FilePath, file.copy, MoreArgs = c(manifest$MDLink)) }
  else { mapply(file.link, manifest$FilePath, manifest$MDLink) }
  
  if (!is.null(outfile)) { saveData(manifest, outfile) }
  manifest
}


#' Remove Symlinks
#'
#' @param manifest DataFrame of classified images 
#' @return manifest without link column
#' @export
#'
#' @examples
#' \dontrun{
#' symlinkMD(manifest, linkdir)
#' }
symUnlink <- function(manifest){
  if ("MDLink" %in% names(manifest)){
    pbapply::pbapply(manifest$MDLink, file.remove)
    manifest <- manifest[, !names(manifest) %in% c("MDLink")]
  }
  if ("Link" %in% names(manifest)){
    pbapply::pbapply(manifest$Link, file.remove)
    manifest <- manifest[, !names(manifest) %in% c("Link")]
  }
  manifest
}