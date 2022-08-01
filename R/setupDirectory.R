#' Set Working Directory and Save File Global Variables
#'
#' @param imagedir local directory that contains data to process
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' setupDirectory(/home/kyra/animl/examples)
#' }
setupDirectory <- function(imagedir) {
  if (!dir.exists(imagedir)) { stop("Output directory invalid.\n")}

  # Assign's specific directory paths
  basedir <<- paste0(imagedir, "/Animl-Directory/")
  datadir <<- paste0(basedir, "Data/")
  cropdir <<- paste0(basedir, "Crops/")
  vidfdir <<- paste0(basedir, "Frames/")
  linkdir <<- paste0(basedir, "Sorted/")
  
  # Creates directories if they do not already exist
  if (!dir.exists(datadir)) {
    dir.create(datadir, recursive = T)
  }
  if (!dir.exists(cropdir)) {
    dir.create(cropdir, recursive = T)
  }
  if (!dir.exists(vidfdir)) {
    dir.create(vidfdir, recursive = T)
  }
  if (!dir.exists(linkdir)) {
    dir.create(linkdir, recursive = T)
  }
  
  # Assign's specific file paths
  filemanifest <<- paste0(datadir, "FileManifest.csv")
  imageframes <<- paste0(datadir, "ImageFrames.csv")
  resultsfile <<- paste0(datadir, "Results.csv")
  classifiedimages <<- paste0(datadir, "ClassifiedImages.csv")
  cropfile <<- paste0(datadir, "Crops.csv")
  predresults <<- paste0(datadir, "Predictions.csv")
  mdresults <<- paste0(datadir, "mdres.RData")
  
}
