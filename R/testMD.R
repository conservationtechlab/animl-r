#' Title
#'
#' @param imagesall dataframe of all images
#' @param mdsession MegaDetector tensorflow session
#'
#' @return Null, plots box on image
#' @export
#'
#' @examples
#' \dontrun{
#' testMD(imagesall,mdsession)
#' }
testMD <- function(imagesall,mdsession){
  f <- dplyr::sample_n(imagesall,1)
  jpg<-jpeg::readJPEG(f$Frame)
  plot(as.raster(jpg))
  mdres<-classifyImageMD(mdsession,f$Frame)
  plotBoxes(mdres)

}
