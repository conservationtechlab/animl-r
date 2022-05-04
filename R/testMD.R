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
testMD <- function(imagesall, mdsession, frame = NA) {
  if (!is(imagesall, "data.frame")) {stop("'imagesall' must be DataFrame")}
  if (!("mdsession" %in% class(mdsession))) {stop("Expecting a mdsession object.")}
  if(is.na(frame)){ sample <- dplyr::slice_sample(imagesall, n = 1)}
  else{sample <- imagesall[frame,]}
  jpg <- jpeg::readJPEG(sample$Frame)
  plot(as.raster(jpg))
  mdres <- detectObject(mdsession, sample$Frame)
  print(mdres)
  plotBoxes(mdres)
}
testMD(allframes,mdsession,frame=123)


