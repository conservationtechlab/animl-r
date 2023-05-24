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
#' setupDirectory(/home/kyra/animl/examples)
#' }
setupDirectory <- function(workingdir,pkg.env) {
  
  if (!dir.exists(workingdir)) { stop("Output directory invalid.\n") }
  if (!endsWith(workingdir, "/")) { workingdir <- paste0(workingdir,"/") }
  
  # Assign specific directory paths
  basedir <- paste0(workingdir, "Animl-Directory/")
  pkg.env$datadir <- paste0(basedir, "Data/")
  pkg.env$cropdir <- paste0(basedir, "Crops/")
  pkg.env$vidfdir <- paste0(basedir, "Frames/")
  pkg.env$linkdir <- paste0(basedir, "Sorted/")
  
  # Create directories if they do not already exist
  if (!dir.exists(pkg.env$datadir)) { dir.create(pkg.env$datadir, recursive = T) }
  if (!dir.exists(pkg.env$cropdir)) { dir.create(pkg.env$cropdir, recursive = T) }
  if (!dir.exists(pkg.env$vidfdir)) { dir.create(pkg.env$vidfdir, recursive = T) }
  if (!dir.exists(pkg.env$linkdir)) { dir.create(pkg.env$linkdir, recursive = T) }
  
 # utils::globalVariables(c("filemanifest","imageframes","resultsfile","cropfile",
 #                          "predresults","mdresults"))
  # Assign specific file paths
  pkg.env$filemanifest <- paste0(pkg.env$datadir, "FileManifest.csv")
  pkg.env$imageframes <- paste0(pkg.env$datadir, "ImageFrames.csv")
  pkg.env$resultsfile <- paste0(pkg.env$datadir, "Results.csv")
  pkg.env$cropfile <- paste0(pkg.env$datadir, "Crops.csv")
  pkg.env$predresults <- paste0(pkg.env$datadir, "Predictions.csv")
  pkg.env$mdresults <- paste0(pkg.env$datadir, "mdres.RData")
}


