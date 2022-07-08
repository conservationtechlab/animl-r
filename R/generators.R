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
#' \dontrun{}
#' @export
#' @import tensorflow
#'
cropImageGenerator <- function(files, boxes, resize_height = 456, resize_width = 456, standardize = FALSE, batch_size = 32) {
  # create data generator for  training (image/label pair)
  if (!(is.vector(files) && class(files) == "character")) {
    stop("files needs to be a vector of file names.\n")
  }
  if (ncol(boxes) != 4) {
    stop("boxes must have four columns.\n")
  }
  if (sum(apply(boxes, 2, is.numeric)) != 4) {
    stop("boxes must be numeric.\n")
  }
  if (length(files) != nrow(boxes)) {
    stop("boxes must have the same number of rows as the length of files.\n")
  }

  data <- data.frame(file = files, boxes)
  dataset <- tfdatasets::tensor_slices_dataset(data)
  dataset <- tfdatasets::dataset_map_and_batch(dataset, function(x) loadImage_Resize_Crop(x, resize_height, resize_width, standardize), batch_size, num_parallel_calls = tf$data$experimental$AUTOTUNE)
  dataset <- tfdatasets::dataset_prefetch(dataset, buffer_size = tf$data$experimental$AUTOTUNE)
  # dataset<-dataset$apply(tf$data$experimental$ignore_errors())
  dataset <- reticulate::as_iterator(dataset)
  dataset
}

#' @title Tensorflow data generator that resizes images.
#'
#' @description Creates an image data generator that resizes images if requested.
#'
#' @param files a vector of file names
#' @param resize_height the height the cropped image will be resized to. If NULL returns original size images.
#' @param resize_width the width the cropped image will be resized to. If NULL returns original size images..
#' @param standardize standardize the image to the range 0 to 1, TRUE or FALSE.
#' @param batch_size the batch size for the image generator.
#'
#' @return A Tensorflow image data generator.
#' @examples
#' \dontrun{
#' dataset <- ImageGenerator(images, standardize = FALSE, batch_size = batch_size)
#' }
#' @export
#' @import tensorflow
#'
ImageGenerator <- function(files, resize_height = NULL, resize_width = NULL, standardize = FALSE, batch_size = 32) {
  # create data generator for  training (image/label pair)
  if (!(is.vector(files) && class(files) == "character")) {
    stop("Please provide a vector of file names.\n")
  }
  dataset <- tfdatasets::tensor_slices_dataset(files)
  if (is.null(resize_height) || is.null(resize_width)) {
    message("No values were provided for resize, returning full-size images.")
    dataset <- tfdatasets::dataset_map_and_batch(dataset, function(x) loadImage(x, standardize), batch_size, num_parallel_calls = tf$data$experimental$AUTOTUNE)
    # dataset<-dataset$apply(tf$data$experimental$ignore_errors())
  } else {
    dataset <- tfdatasets::dataset_map_and_batch(dataset, function(x) loadImage_Resize(x, resize_height, resize_width, standardize), batch_size, num_parallel_calls = tf$data$experimental$AUTOTUNE)
  }
  dataset <- tfdatasets::dataset_prefetch(dataset, buffer_size = tf$data$experimental$AUTOTUNE)
  dataset <- reticulate::as_iterator(dataset)
  dataset
}


#' @title Tensorflow data generator that resizes images and returns original image size.
#'
#' @description Creates an image data generator that resizes images if requested and also returns the origianl images size needed for MegaDetector.
#'
#' @param files a vector of file names
#' @param resize_height the height the cropped image will be resized to. If NULL returns original size images.
#' @param resize_width the width the cropped image will be resized to. If NULL returns original size images..
#' @param pad pad the image instead of stretching it, TRUE or FALSE.
#' @param standardize standardize the image to the range 0 to 1, TRUE or FALSE.
#' @param batch_size the batch size for the image generator.
#'
#' @return A Tensorflow image data generator.
#' @examples
#' \dontrun{
#' dataset <- ImageGenerator(images, standardize = FALSE, batch_size = batch_size)
#' }
#' @export
#' @import tensorflow
#'
ImageGeneratorSize <- function(files, resize_height = NULL, resize_width = NULL, pad=FALSE, standardize = FALSE, batch_size = 32) {
  # create data generator for  training (image/label pair)
  if (!(is.vector(files) && class(files) == "character")) {
    stop("Please provide a vector of file names.\n")
  }
  dataset <- tfdatasets::tensor_slices_dataset(files)
  if (is.null(resize_height) || is.null(resize_width)) {
    message("No values were provided for resize, returning full-size images.")
    dataset <- tfdatasets::dataset_map_and_batch(dataset, function(x) loadImage(x, standardize), batch_size, num_parallel_calls = tf$data$experimental$AUTOTUNE)
    # dataset<-dataset$apply(tf$data$experimental$ignore_errors())
  } else {
    dataset <- tfdatasets::dataset_map_and_batch(dataset, function(x) loadImage_Resize_Size(x, height=resize_height, width=resize_width, pad=pad,standardize=standardize), batch_size, num_parallel_calls = tf$data$experimental$AUTOTUNE)
    #dataset <- tfdatasets::dataset_map_and_batch(dataset, function(x) loadImage_Resize(x, resize_height, resize_width,standardize=standardize), batch_size, num_parallel_calls = tf$data$experimental$AUTOTUNE)
  }
  dataset <- tfdatasets::dataset_prefetch(dataset, buffer_size = tf$data$experimental$AUTOTUNE)
  dataset <- reticulate::as_iterator(dataset)
  dataset
}


#' @title Load an image and return the full size image as an image tensor.
#'
#' @description Load an image and return the full size an image tensor. Internal function to be called by image generator function.
#'
#' @param file path to a JPEG file
#' @param standardize standardize the image, TRUE or FALSE.
#'
#' @return An image tensor.
#' @examples
#' \dontrun{}
#' @import tensorflow
#'
loadImage <- function(file, standardize = FALSE) {
  # catch error caused by missing files and zero-length files
  if (!is.null(tryCatch(image <- tf$io$read_file(file), error = function(e) NULL))) {
    image <- tf$image$decode_jpeg(image, channels = 3, try_recover_truncated = T)
    if (standardize) images <- tf$image$convert_image_dtype(image, dtype = tf$float32)
  } else {
    image <- tf$zeros(as.integer(c(299, 299, 3)), dtype = tf$float32)
  }
  image
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
#' \dontrun{}
#' @import tensorflow
#'
loadImage_Resize <- function(file, height = 299, width = 299, pad=FALSE,standardize = FALSE) {
  size <- as.integer(c(height, width))
  
  # catch error caused by missing files and zero-length files
  if (!is.null(tryCatch(image <- tf$io$read_file(file), error = function(e) NULL))) {
    image <- tf$cond(
      tf$equal(tf$strings$length(image), as.integer(0)), function() tf$zeros(as.integer(c(height, width, 3)), dtype = tf$float32),
      function() {
        image<-tf$image$decode_jpeg(image, channels = 3, try_recover_truncated = T)
        image <- tf$image$convert_image_dtype(image, dtype = tf$float32)
        if(pad==TRUE){
          image<-tf$image$resize_with_pad(image, as.integer(height), as.integer(width), method = "bicubic")
        }else{
          image<-tf$image$resize(image,size = size)
        }
        if (!standardize) images <- tf$image$convert_image_dtype(image, dtype = tf$uint8)
        image
      }
    )
  } else {
    image <- tf$zeros(as.integer(c(height, width, 3)), dtype = tf$float32)
  }
  image
}


#' @title Load and resize an image and return an image tensor as well as a tensor with the original image size.
#'
#' @description Load and resize an image and return an image tensor as well as a tensor with the original image size. Internal function to be called by image generator function.
#'
#' @param file path to a JPEG file
#' @param height the height the cropped image will be resized to.
#' @param width the width the cropped image will be resized to.
#' @param pad pad the image instead of stretching it, TRUE or FALSE.
#' @param standardize standardize the image, TRUE or FALSE.
#'
#' @return An image tensor.
#' @examples
#' \dontrun{}
#' @import tensorflow
#'
loadImage_Resize_Size <- function(file, height = 299, width = 299, pad=FALSE,standardize = FALSE) {
  # catch error caused by missing files and zero-length files
  if (!is.null(tryCatch(image <- tf$io$read_file(file), error = function(e) NULL))) {
    size <- as.integer(c(height, width))
    image <- tf$cond(
      tf$equal(tf$strings$length(image), as.integer(0)), function() list(image = tf$zeros(as.integer(c(height, width, 3)), dtype = tf$float32),width=as.integer(width),height=as.integer(height)),
      function() {
        image<-tf$image$decode_jpeg(image, channels = 3, try_recover_truncated = T)
        imgdim <- tf$cast(tf$unstack(tf$shape(image)), tf$float32)
        img_height<- tf$cast(imgdim[[0]], tf$int32)
        img_width <- tf$cast(imgdim[[1]], tf$int32)
        image <- tf$image$convert_image_dtype(image, dtype = tf$float32)
        if(pad==TRUE){
          image<-tf$image$resize_with_pad(image, as.integer(height), as.integer(width), method = "bicubic")
        }else{
          image2<-tf$image$resize(image,size = size)
        }
        if (!standardize) image <- tf$image$convert_image_dtype(image, dtype = tf$uint8)
        list(image=image,width=img_width,height=img_height)
        #image
      }
    )
  } else {
    image<-list(image = tf$zeros(as.integer(c(height, width, 3)), dtype = tf$float32),width=width,height=height)
  }
  image
}


#' @title Load, resize and crop an image and return an image tensor.
#'
#' @description Load a JPEG image and crop it to a bounding box. Internal function to be called by image generator function.
#'
#' @param data a list with the first element being a path to an image file and the next four arguments being the bounding box coordinates.
#' @param height the height the cropped image will be resized to.
#' @param width the width the cropped image will be resized to.
#' @param standardize standardize the image, TRUE or FALSE.
#'
#' @return A Tensorflow image data generator.
#' @examples 
#' \dontrun{}
#' @import tensorflow
#'
loadImage_Resize_Crop <- function(data, height = 299, width = 299, standardize = FALSE) {
  # catch error caused by missing files and zero-length files
  if (!is.null(tryCatch(image <- tf$io$read_file(data[[1]]), error = function(e) NULL))) {
    image <- tf$cond(
      tf$equal(tf$strings$length(image), as.integer(0)), function() tf$zeros(as.integer(c(height, width, 3)), dtype = tf$float32),
      function() {
        image <- tf$image$decode_jpeg(image, channels = 3, try_recover_truncated = T)
        imgdim <- tf$cast(tf$unstack(tf$shape(image)), tf$float32)
        img_height<- tf$cast(imgdim[[1]], tf$int32)
        img_width <- tf$cast(imgdim[[0]], tf$int32)
        crop_top <- tf$cast(imgdim[[1]] * data[[2]], tf$int32)
        crop_left <- tf$cast(imgdim[[0]] * data[[3]], tf$int32)
        crop_height <- tf$cast(imgdim[[1]] * data[[4]], tf$int32)
        crop_width <- tf$cast(imgdim[[0]] * data[[5]], tf$int32)
        crop_height <- tf$cond(tf$greater((crop_top+crop_height),img_height),function()tf$cast(img_height-crop_top, tf$int32),function()crop_height)
        crop_width  <- tf$cond(tf$greater((crop_left+crop_width),img_width),function()crop_height<-tf$cast(img_width-crop_left, tf$int32),function()crop_width)
        crop_height <- tf$cond(tf$equal(crop_height,as.integer(0)),function()tf$cast(1, tf$int32),function()crop_height)
        crop_width  <- tf$cond(tf$equal(crop_width,as.integer(0)),function()crop_height<-tf$cast(1, tf$int32),function()crop_width)
        image <- tf$image$convert_image_dtype(image, dtype = tf$float32)
        image <- tf$image$crop_to_bounding_box(image, crop_left, crop_top, crop_width, crop_height)
        image <- tf$image$resize_with_pad(image, as.integer(height), as.integer(width), method = "bicubic")
      }
    )
    if (!standardize) images <- tf$image$convert_image_dtype(image, dtype = tf$uint8)
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
image_label <- function(data, height = 299, width = 299, standardize = FALSE) {
  image <- loadImage_Resize(data[[1]], height, width, standardize)
  list(image, data[[2]])
}

