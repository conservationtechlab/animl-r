#' Select a Random Image and Run Through MegaDetector
#'
#' @param input dataframe of all images
#' @param mdsession MegaDetector mdsession
#' @param mdversion megadetector version, defaults to 5
#' @param minconf minimum confidence with which to draw boxes, defaults to 0
#'
#' @return Null, plots box on image
#' @export
#'
#' @examples
#' \dontrun{
#' testMD(input, mdsession)
#' }
testMD <- function(input, mdsession, mdversion = 5, minconf = 0) {
  if (is(input, "data.frame")) { 
    sample <- dplyr::slice_sample(input, n = 1) 
    path <- sample$Frame
  }
  else if (is(input, "character")) { path <- input }
  else { stop("Must input a dataframe or image path") }
  
  jpg <- jpeg::readJPEG(path)
  plot(grDevices::as.raster(jpg))
  mdres <- detectObject(mdsession, path, mdversion)
  plotBoxes(mdres, minconf = minconf)
}

