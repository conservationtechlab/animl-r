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
#' workingDirectory(/home/kyra/animl/examples)
#' }
workingDirectory <- function(workingdir,pkg.env) {
  
  if (!dir.exists(workingdir)) { stop("Output directory invalid.\n") }
  if (!endsWith(workingdir, "/")) { workingdir <- paste0(workingdir,"/") }
  
  # Assign specific directory paths
  basedir <- paste0(workingdir, "Animl-Directory/")
  pkg.env$datadir <- paste0(basedir, "Data/")
  pkg.env$vidfdir <- paste0(basedir, "Frames/")
  pkg.env$linkdir <- paste0(basedir, "Sorted/")
  
  # Create directories if they do not already exist
  if (!dir.exists(pkg.env$datadir)) { dir.create(pkg.env$datadir, recursive = T) }
  if (!dir.exists(pkg.env$vidfdir)) { dir.create(pkg.env$vidfdir, recursive = T) }
  if (!dir.exists(pkg.env$linkdir)) { dir.create(pkg.env$linkdir, recursive = T) }
  
  # Assign specific file paths
  pkg.env$filemanifest <- paste0(pkg.env$datadir, "FileManifest.csv")
  pkg.env$imageframes <- paste0(pkg.env$datadir, "ImageFrames.csv")
  pkg.env$resultsfile <- paste0(pkg.env$datadir, "Results.csv")
  pkg.env$predresults <- paste0(pkg.env$datadir, "Predictions.csv")
  pkg.env$detections <- paste0(pkg.env$datadir, "Detections.csv")
  pkg.env$mdraw <- paste0(pkg.env$datadir, "MD_Raw.json")
}


