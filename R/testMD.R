#' Select a Random Image and Run Through MegaDetector
#'
#' @param imagesall dataframe of all images
#' @param mdsession MegaDetector mdsession
#'
#' @return Null, plots box on image
#' @export
#'
#' @examples
#' \dontrun{
#' testMD(imagesall, mdsession)
#' }
testMD <- function(input, mdsession, mdversion = 5) {
  if (is(input, "data.frame")) { 
    sample <- dplyr::slice_sample(input, n = 1) 
    path <- sample$Frame
  }
  else if (is(input, "character")) { path <- input }
  else { stop("Must input a dataframe or image path") }
  
  jpg <- jpeg::readJPEG(path)
  plot(as.raster(jpg))
  mdres <- detectObject(mdsession, path, mdversion)
  plotBoxes(mdres)

}

