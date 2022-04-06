#' @title Tensorflow data generator that crops images to bounding box.
#'
#' @description Creates an image data generator that crops images based on bounding box coordinates.
#'
#' @param files a vector of file names
#' @param boxes a data frame or matrix of bounding box coordinates in the format left, top, width, height.
#' @param resize_height the height the cropped image will be resized to.
#' @param resize_width the width the cropped image will be resized to.
#' @param standardize standardize the image to the range 0 to 1, TRUE or FALSE.
#' @param batch_size the batch size for the image generator.
#'
#' @return A Tensorflow image data generator.
#' @examples
#'
#' @export
#' @import tensorflow
#' 
cropImageGenerator <- function(files, boxes, resize_height = 456, resize_width = 456, standardize = FALSE, batch_size = 32) {
  # create data generator for  training (image/label pair)
  if (!(is.vector(files) & class(files) == "character")) stop("files needs to be a vector of file names.\n")
  if (ncol(boxes) != 4) stop("boxes must have four columns.\n")
  if (sum(apply(boxes, 2, is.numeric)) != 4) stop("boxes must be numeric.\n")
  if (length(files) != nrow(boxes)) stop("boxes must have the same number of rows as the length of files.\n")
  data <- data.frame(file = files, boxes)
  dataset <- tfdatasets::tensor_slices_dataset(data)
  dataset <- tfdatasets::dataset_map_and_batch(dataset, function(x) load_img_resize_crop(x, resize_height, resize_width, standardize), batch_size, num_parallel_calls = tf$data$experimental$AUTOTUNE)
  dataset <- tfdatasets::dataset_prefetch(dataset, buffer_size = tf$data$experimental$AUTOTUNE)
  # dataset <- dataset$apply(tf$data$experimental$ignore_errors())
  dataset <- tfdatasets::as_iterator(dataset)
  dataset
}

  
#' @title Load and resize an image and return an image tensor.
#'
#' @description Load and resize an image and return an image tensor. Internal function to be called by image generator function.
#'
#' @param file path to a JPEG file
#' @param height the height the cropped image will be resized to.
#' @param width the width the cropped image will be resized to.
#' @param standardize standardize the image, TRUE or FALSE.
#'
#' @return An image tensor.
#' @examples
#' @import tensorflow
#'
load_img_resize <- function(file, height = 299, width = 299, standardize = TRUE) {
  size <- as.integer(c(height, width))

  # catch error caused by missing files and zero-length files
  if (!is.null(tryCatch(image <- tf$io$read_file(file), error = function(e) NULL))) {
    image <- tf$cond(
      tf$equal(tf$strings$length(image), as.integer(0)), function() tf$zeros(as.integer(c(height, width, 3)), dtype = tf$float32),
      function() {
        tf$image$decode_jpeg(image, channels = 3, try_recover_truncated = T) %>%
          tf$image$resize(size = size)
      }
    )
    if (standardize) images <- tf$image$convert_image_dtype(image, dtype = tf$float32)
  } else {
    image <- tf$zeros(as.integer(c(height, width, 3)), dtype = tf$float32)
  }
  image
}

#' @title Load image and return a tensor with an image and a corresponding label.
#'
#' @description Load image and return a tensor with an image and a corresponding label. Internal function to be called by image generator function.
#'
#' @param data a list with the first element being an image file path and the second element a label
#' @param height the height the cropped image will be resized to.
#' @param width the width the cropped image will be resized to.
#' @param standardize standardize the image, TRUE or FALSE.
#'
#' @return An image and label tensor.
#' @examples
#'
#'
image_label <- function(data, height = 299, width = 299, standardize = TRUE) {
  image <- load_img_resize(data[[1]], height, width, standardize)
  list(image, data[[2]])
}


#' @title Load, resize and crop an image and return an image tensor.
#'
#' @description Load a JPEG image and crop it to a bounding box. Internal function to be called by image generator function.
#'
#' @param data a list with the first element being a path to an image file and the next four arguments being the bounding box coordinates.
#' @param height the height the cropped image will be resized to.
#' @param width the width the cropped image will be resized to.
#' @param standardize standardize the image, TRUE or FALSE.
#' @param batch_size the batch size for the image generator.
#'
#' @return A Tensorflow image data generator.
#' @examples
#' @import tensorflow
#'
load_img_resize_crop <- function(data, height = 299, width = 299, standardize = TRUE) {

  # catch error caused by missing files and zero-length files
  if (!is.null(tryCatch(image <- tf$io$read_file(data[[1]]), error = function(e) NULL))) {
    image <- tf$cond(
      tf$equal(tf$strings$length(image), as.integer(0)), function() tf$zeros(as.integer(c(height, width, 3)), dtype = tf$float32),
      function() {
        image <- tf$image$decode_jpeg(image, channels = 3, try_recover_truncated = T)
        imgdim <- tf$cast(tf$unstack(tf$shape(image)), tf$float32)
        crop_top <- tf$cast(imgdim[[1]] * data[[2]], tf$int32)
        crop_left <- tf$cast(imgdim[[0]] * data[[3]], tf$int32)
        crop_height <- tf$cast(imgdim[[1]] * data[[4]], tf$int32)
        crop_width <- tf$cast(imgdim[[0]] * data[[5]], tf$int32)
        image <- tf$image$crop_to_bounding_box(image, crop_left, crop_top, crop_width, crop_height) %>%
          tf$image$resize_with_pad(as.integer(height), as.integer(width), method = "bicubic")
      }
    )
    if (standardize) images <- tf$image$convert_image_dtype(image, dtype = tf$float32)
  } else {
    image <- tf$zeros(as.integer(c(height, width, 3)), dtype = tf$float32)
  }
  image
}
