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
    jpg <- jpeg::readJPEG(image$file)
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

library(magick)
#' Crops a jpg image using a precalculated bounding box
#'
#' @param imagePath The path for the image
#' @param bbox1 bounding box coordinate corresponding to the left edge of the box
#' @param bbox2 bounding box coordinate corresponding to the bottom edge of the box
#' @param bbox3 bounding box coordinate corresponding to the right edge of the box
#' @param bbox4 bounding box coordinate corresponding to the top edge of the box
#'
#' @return no return value, outputs the cropped image
#' @export
#'
#' @examples
#' \dontrun{
#' cropJpg("/image/path/image.jpg", "/output/path/", 0.1, 0.2, 0.6, 0.8)
#' }
cropJpg <- function(imagePath, outputPath, bbox1, bbox2, bbox3, bbox4) {
  image <- image_read(imagePath)
  info <- image_info(image)
  width <- info$width
  height <- info$height
  
  xleft <- bbox1 * width
  ybottom <- height - bbox2 * height
  xright <- (bbox1 + bbox3) * width
  ytop <- height - (bbox2 + bbox4) * height
  
  width <- xright - xleft
  height <- ybottom - ytop
  
  dimensions <- paste(width, height, sep="x")
  dimensions <- paste(dimensions, xleft, ytop, sep="+")
  cropped <- image_crop(image, dimensions)
  
  split_image_path_vector <- strsplit(imagePath, "/")
  image_name <- split_image_path_vector[[1]][length(split_image_path_vector[[1]])]
  output_path <- paste(outputPath, image_name)
  image_write(cropped, path = output_path, format = "png")
}

#' Crops all images from an input file with specific required columns: Frame,
#' bbox1, bbox2, bbox3, and bbox4
#'
#' @param csvFilePath The path for the input csv file
#' @param outputPath The path where generated cropped images should be uploaded
#'
#' @return no return value, outputs the cropped image
#' @export
#'
#' @examples
#' \dontrun{
#' cropImagesFromFile("/image/path/file.csv", "/output/path/")
#' }
cropImagesFromFile <- function(csvFilePath, outputPath) {
  uncropped_data <- read.csv(csvFilePath)
  df = data.frame(uncropped_data)
  df <- as.data.frame(sapply(df, function(x) gsub("\"", "", x))) # Removes extra quotation marks
  
  # for-loop over rows
  for(i in 1:nrow(df)) { 
    cropJpg(df[i,]$Frame,outputPath,as.numeric(df[i,]$bbox1),as.numeric(df[i,]$bbox2),as.numeric(df[i,]$bbox3),as.numeric(df[i,]$bbox4))
  }
}
