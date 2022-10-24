#' Plot bounding boxes on image from md results
#'
#' @param image The mdres for the image
#' @param label T/F toggle to plot MD category
#' @param minconf minimum confidence to plot box
#'
#' @return no return value, produces bounding box in plot panel
#' @export
#'
#' @examples
#' \dontrun{
#' mdres <- classifyImageMD(mdsession, images$FilePath[30000])
#' plotBoxes(mdres, minconf = 0.5)
#' }
plotBoxes <- function(image, label = FALSE, minconf = 0) {
  # animal, human, unused, vehicle
  col <- c("green", "red", "blue", "orange")
  
  # process MD output
  if (is.list(image)) {
    jpg <- jpeg::readJPEG(image$FilePath)
    plot(grDevices::as.raster(jpg)) ## where is this from??
    jpgy <- dim(jpg)[1]
    jpgx <- dim(jpg)[2]
    
    if (is.data.frame(image$detections)) { boxes <- image$detections }
  }
  # process image data frame
  else if (is.data.frame(image) & nrow(image) > 0) {
    if (length(unique(image$FilePath)) > 1) {
      stop("Provide data for a single image file.\n")
    }
    # load image
    jpg <- jpeg::readJPEG(image$FilePath[1])
    plot(grDevices::as.raster(jpg))
    jpgy <- dim(jpg)[1]
    jpgx <- dim(jpg)[2]
    boxes <- image
  }
  
  else { stop("Requires a vector or MD list containing bounding boxes") }
  
  if (nrow(boxes) > 0) {
    # plot bounding boxes
    for (j in 1:nrow(boxes)) {
      if (boxes[j, ]$conf >= minconf) {
        graphics::rect(boxes[j, ]$bbox1 * jpgx, 
                       jpgy - boxes[j, ]$bbox2 * jpgy, 
                       (boxes[j, ]$bbox1 + boxes[j, ]$bbox3) * jpgx, 
                       jpgy - (boxes[j, ]$bbox2 + boxes[j, ]$bbox4) * jpgy, 
                       border = col[as.numeric(boxes[j, ]$category)], lwd = 2)
        if (label) {
          graphics::text(x = boxes[j, ]$bbox1 * jpgx, 
                         y = jpgy - (boxes[j, ]$bbox2 + boxes[j, ]$bbox4), 
                         lables = boxes[j, ]$category, )
        }
      }
    }
  }
}
