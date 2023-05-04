#' Set Working Directory and Save File Global Variables
#'
#' @param workingdir local directory that contains data to process
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' setupDirectory(/home/kyra/animl/examples)
#' }
setupDirectory <- function(workingdir) {
  if (!dir.exists(workingdir)) { stop("Output directory invalid.\n") }
  if (!endsWith(workingdir, "/")) { workingdir <- paste0(workingdir,"/") }
  
  utils::globalVariables(c("basedir","datadir","cropdir","vidfdir","linkdir"))
  # Assign specific directory paths
  basedir <- paste0(workingdir, "Animl-Directory/")
  datadir <- paste0(basedir, "Data/")
  cropdir <- paste0(basedir, "Crops/")
  vidfdir <- paste0(basedir, "Frames/")
  linkdir <- paste0(basedir, "Sorted/")
  
  # Create directories if they do not already exist
  if (!dir.exists(datadir)) { dir.create(datadir, recursive = T) }
  if (!dir.exists(cropdir)) { dir.create(cropdir, recursive = T) }
  if (!dir.exists(vidfdir)) { dir.create(vidfdir, recursive = T) }
  if (!dir.exists(linkdir)) { dir.create(linkdir, recursive = T) }
  
  utils::globalVariables(c("filemanifest","imageframes","resultsfile","cropfile",
                           "predresults","mdresults"))
  # Assign specific file paths
  filemanifest <- paste0(datadir, "FileManifest.csv")
  imageframes <- paste0(datadir, "ImageFrames.csv")
  resultsfile <- paste0(datadir, "Results.csv")
  cropfile <- paste0(datadir, "Crops.csv")
  predresults <- paste0(datadir, "Predictions.csv")
  mdresults <- paste0(datadir, "mdres.RData")
}
