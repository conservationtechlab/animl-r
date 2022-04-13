#' Select a Random Image and Run Through MegaDetector
#'
#' @param imagesall dataframe of all images
#' @param mdsession MegaDetector tensorflow session
#'
#' @return Null, plots box on image
#' @export
#'
#' @examples
#' \dontrun{
#' testMD(imagesall, mdsession)
#' }
testMD <- function(imagesall, mdsession) {
  if (!is(imagesall, "data.frame")) {
    stop("'imagesall' must be DataFrame")
  }
  if (!("mdsession" %in% class(mdsession))) stop("Expecting a mdsession object.")

  f <- dplyr::sample_n(imagesall, 1)
  jpg <- jpeg::readJPEG(f$Frame)
  plot(as.raster(jpg))
  mdres <- detectObject(mdsession, f$Frame)
  plotBoxes(mdres)
}
