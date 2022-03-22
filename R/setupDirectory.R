#' Title
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
setupDirectory <- function(imagedir){
  if(!dir.exists(imagedir)){stop("Output directory invalid.\n")}

  basedir <<- paste0(imagedir,"/Working-Directory/")

  datadir <<- paste0(basedir,"Data/")
  cropdir <<- paste0(basedir,"Crops/")
  vidfdir <<- paste0(basedir,"Temp/")
  linkdir <<- paste0(basedir,"Link/")

  if(!dir.exists(datadir)){dir.create(datadir,recursive = T)}
  if(!dir.exists(cropdir)){dir.create(cropdir,recursive = T)}
  if(!dir.exists(vidfdir)){dir.create(vidfdir,recursive = T)}
  if(!dir.exists(linkdir)){dir.create(linkdir,recursive = T)}

  filemanifest <<- "FileManifest.csv"
  imageframes <<- "ImageFrames.csv"
  resultsfile <<- "Results.csv"
  cropfile <<- "Crops.csv"
  mdresults <<- "mdres.RData"
  predresults <<- "pred.RData"
}
