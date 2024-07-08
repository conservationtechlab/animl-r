#' @title Tensorflow data generator that crops images to bounding box.
#'
#' @description Creates an image data generator that crops images based on bounding box coordinates.
#'
#' @param files a vector of file names
#' @param boxes a data frame or matrix of bounding box coordinates in the format left, top, width, height.
#' @param resize_height the height the cropped image will be resized to.
#' @param resize_width the width the cropped image will be resized to.
#' @param standardize standardize the image to the range 0 to 1, TRUE or FALSE.
#' @param batch the batch size for the image generator.
#'
#' @return A Tensorflow image data generator.
#' @export
#' @import tensorflow
#'
#' @examples
#' \dontrun{#' dataset <- cropImageGenerator(images, boxes, standardize = FALSE, batch = batch)}
cropImageGenerator <- function(files, boxes, resize_height = 456, resize_width = 456, standardize = FALSE, batch = 32) {
  # create data generator for  training (image/label pair)
  if (!(is.vector(files) && inherits(files,"character"))) {
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
  dataset <- tfdatasets::dataset_map(dataset, function(x) loadImageResizeCrop(x, resize_height, resize_width, standardize),num_parallel_calls = tf$data$experimental$AUTOTUNE)
  dataset <- tfdatasets::dataset_batch(dataset, batch, num_parallel_calls = tf$data$experimental$AUTOTUNE,deterministic=TRUE)
  dataset <- tfdatasets::dataset_prefetch(dataset, buffer_size = tf$data$experimental$AUTOTUNE)
  # dataset<-dataset$apply(tf$data$experimental$ignore_errors())
  dataset <- reticulate::as_iterator(dataset)
  dataset
}


#' @title Tensorflow data generator for training that crops images to bounding box.
#'
#' @description Creates an image data generator that crops images based on bounding box coordinates and returnes an image/label pair.
#'
#' @param files a vector of file names
#' @param boxes a data frame or matrix of bounding box coordinates in the format left, top, width, height.
#' @param label a vector of labels
#' @param classes a vector of all classes for the active model
#' @param resize_height the height the cropped image will be resized to.
#' @param resize_width the width the cropped image will be resized to.
#' @param standardize standardize the image to the range 0 to 1, TRUE or FALSE.
#' @param augmentation_color use data augmentation to change the color, TRUE or FALSE.
#' @param augmentation_geometry use data augmentation to change the geometry of the images, TRUE or FALSE.
#' @param shuffle return data pairas in random order, TRUE or FALSE.
#' @param cache use caching to reduce reading from disk, TRUE or FALSE.
#' @param cache_dir directory used for caching, if none provided chaching will be done in memory.
#' @param return_iterator Should an iterator be returned? If RALSE a tfdataset will be returned.
#' @param batch the batch size for the image generator.
#'
#' @return A Tensorflow image data generator.
#' @export
#' @import tensorflow
#'
#' @examples
#' \dontrun{
#' dataset <- cropImageTrainGenerator(images, standardize = FALSE, batch = batch)}
cropImageTrainGenerator <- function(files, boxes, label, classes,
                                    resize_height = 456, resize_width = 456, 
                                    standardize = FALSE, augmentation_color=FALSE,
                                    augmentation_geometry=FALSE, shuffle=FALSE,
                                    cache=FALSE, cache_dir=NULL, return_iterator=FALSE, batch = 32) {
  # create data generator for  training (image/label pair)
  if (!(is.vector(files) && inherits(files,"character"))) {
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
  if(!is.null(cache_dir) && !dir.exists(cache_dir)){
    stop("cache directory does not exist.\n")
  }
  data <- data.frame(file = files, boxes,label)
  rng<-tf$random$Generator$from_seed(as.integer(123),alg='philox')
  auggeo<-imageAugmentationGeometry()
  dataset <- tfdatasets::tensor_slices_dataset(data)
  if(shuffle) 
    dataset <- tfdatasets::dataset_shuffle(dataset,buffer_size=nrow(data), reshuffle_each_iteration=TRUE)
  dataset <- tfdatasets::dataset_map(dataset, function(x) imageLabelCrop(x, classes,resize_height, resize_width, standardize),num_parallel_calls = tf$data$experimental$AUTOTUNE)
  if(cache){
    if(is.null(cache_dir)){
      dataset <- tfdatasets::dataset_cache(dataset)
    }else{
      dataset <- tfdatasets::dataset_cache(dataset,cache_dir)
    }
  }
  dataset <- tfdatasets::dataset_repeat(dataset)
  if(augmentation_geometry) 
    dataset <- tfdatasets::dataset_map(dataset,function(x,y)list(auggeo(x,training=TRUE),y),num_parallel_calls = tf$data$experimental$AUTOTUNE)
  if(augmentation_color) 
    dataset <- tfdatasets::dataset_map(dataset,function(x,y)imageAugmentationColor(x,y,rng),num_parallel_calls = tf$data$experimental$AUTOTUNE)
  dataset <- tfdatasets::dataset_batch(dataset, batch, num_parallel_calls = tf$data$experimental$AUTOTUNE,deterministic=TRUE)
  # dataset<-dataset$apply(tf$data$experimental$ignore_errors())
  dataset <- tfdatasets::dataset_prefetch(dataset, buffer_size = tf$data$experimental$AUTOTUNE)
  if(return_iterator)
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
#' @param batch the batch size for the image generator.
#'
#' @return A Tensorflow image data generator.
#' @export
#' @import tensorflow
#'
#' @examples
#' \dontrun{
#' dataset <- ImageGenerator(images, standardize = FALSE, batch = batch)
#' }
ImageGenerator <- function(files, resize_height = NULL, resize_width = NULL, standardize = FALSE, batch = 1) {
  # create data generator for  training (image/label pair)
  if (!(is.vector(files) && inherits(files,"character"))) {
    stop("files needs to be a vector of file names.\n")
  }
  
  data <- data.frame(file=files)
  dataset <- tfdatasets::tensor_slices_dataset(files)
  
  if (is.null(resize_height) || is.null(resize_width)) {
    message("No values were provided for resize, returning full-size images.")
    dataset <- tfdatasets::dataset_map(dataset, function(x) loadImage(x, standardize=standardize), num_parallel_calls = tf$data$experimental$AUTOTUNE)
    dataset <- tfdatasets::dataset_batch(dataset, batch, num_parallel_calls = tf$data$experimental$AUTOTUNE,deterministic=TRUE)
  } 
  else {
    dataset <- tfdatasets::dataset_map(dataset, function(x) loadImageResize(x, resize_height, resize_width, standardize=standardize),num_parallel_calls = tf$data$experimental$AUTOTUNE)
    dataset <- tfdatasets::dataset_batch(dataset, batch, num_parallel_calls = tf$data$experimental$AUTOTUNE,deterministic=TRUE)
  }
  dataset <- tfdatasets::dataset_prefetch(dataset, buffer_size = tf$data$experimental$AUTOTUNE)
  dataset <- reticulate::as_iterator(dataset)
  dataset
}


#' @title Tensorflow data generator that resizes images and returns original image size.
#'
#' @description Creates an image data generator that resizes images if requested and also returns the original images size needed for MegaDetector.
#'
#' @param files a vector of file names
#' @param resize_height the height the cropped image will be resized to. If NULL returns original size images.
#' @param resize_width the width the cropped image will be resized to. If NULL returns original size images..
#' @param pad pad the image instead of stretching it, TRUE or FALSE.
#' @param standardize standardize the image to the range 0 to 1, TRUE or FALSE.
#' @param batch the batch size for the image generator.
#'
#' @return A Tensorflow image data generator.
#' @export
#' @import tensorflow
#' 
#' @examples
#' \dontrun{
#' dataset <- ImageGenerator(images, standardize = FALSE, batch = batch)
#' }
ImageGeneratorSize <- function(files, resize_height = NULL, resize_width = NULL, pad=FALSE, standardize = FALSE, batch = 1) {
  # create data generator for  training (image/label pair)
  if (!(is.vector(files) && inherits(files,"character"))) {
    stop("Please provide a vector of file names.\n")
  }
  dataset <- tfdatasets::tensor_slices_dataset(files)
  if (is.null(resize_height) || is.null(resize_width)) {
    message("No values were provided for resize, returning full-size images.")
    dataset <- tfdatasets::dataset_map(dataset, function(x) loadImage(x, standardize),num_parallel_calls = tf$data$experimental$AUTOTUNE)
    dataset<-dataset$apply(tf$data$experimental$ignore_errors())
    dataset <- tfdatasets::dataset_batch(dataset, batch, num_parallel_calls = tf$data$experimental$AUTOTUNE,deterministic=TRUE)
  } else {
    dataset <- tfdatasets::dataset_map(dataset, function(x) loadImageResizeSize(x, height=resize_height, width=resize_width, pad=pad,standardize=standardize),num_parallel_calls = tf$data$experimental$AUTOTUNE)
    dataset<-dataset$apply(tf$data$experimental$ignore_errors())
    dataset <- tfdatasets::dataset_batch(dataset, batch, num_parallel_calls = tf$data$experimental$AUTOTUNE,deterministic=TRUE)
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
#' @import tensorflow
loadImage <- function(file, standardize = FALSE) {
  # catch error caused by missing files and zero-length files
  if (is.null(tryCatch({image <- tf$io$read_file(file);
      image <- tf$image$decode_jpeg(image, channels = 3, try_recover_truncated = T)},silent=T, error = function(e) NULL))) {
      image <- tf$zeros(as.integer(c(299, 299, 3)), dtype = tf$uint8)
  }
  if (!standardize) image <- tf$image$convert_image_dtype(image, dtype = tf$uint8)
  if (standardize) image <- tf$image$convert_image_dtype(image, dtype = tf$float32)
  image
}

#' @title Load and resize an image and return an image tensor.
#'
#' @description Load and resize an image and return an image tensor. Internal function to be called by image generator function.
#'
#' @param file path to a JPEG file
#' @param height the height the cropped image will be resized to.
#' @param width the width the cropped image will be resized to.
#' @param pad logical indicating whether the images should be padded or streched.
#' @param standardize standardize the image, TRUE or FALSE.
#'
#' @return An image tensor.
#' @import tensorflow
loadImageResize <- function(file, height = 299, width = 299, pad=FALSE,standardize = FALSE) {
  size <- as.integer(c(height, width))
  
  # catch error caused by missing files and zero-length files
  if (!is.null(tryCatch({image <- tf$io$read_file(file);
                        image <- tf$image$decode_jpeg(image, channels = 3, try_recover_truncated = T)},silent=T, error = function(e) NULL))) {
    image <- tf$image$convert_image_dtype(image, dtype = tf$float32)
    if(pad==TRUE){
      image<-tf$image$resize_with_pad(image, as.integer(height), as.integer(width), method = "bilinear")
    }else{
      image<-tf$image$resize(image,size = size)
    }
  } else {
    image <- tf$zeros(as.integer(c(height, width, 3)), dtype = tf$float32)
  }
  if (!standardize) image <- tf$image$convert_image_dtype(image, dtype = tf$uint8)
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
#' @import tensorflow
loadImageResizeSize <- function(file, height = 299, width = 299, pad=FALSE,standardize = FALSE) {
  # catch error caused by missing files and zero-length files
  if (!is.null(tryCatch({image <- tf$io$read_file(file);
                        image <- tf$image$decode_jpeg(image, channels = 3, try_recover_truncated = T)},silent=T, error = function(e) NULL))) {
    size <- as.integer(c(height, width))
    imgdim <- tf$cast(tf$unstack(tf$shape(image)), tf$float32)
    img_height<- tf$cast(imgdim[[0]], tf$int32)
    img_width <- tf$cast(imgdim[[1]], tf$int32)
    image <- tf$image$convert_image_dtype(image, dtype = tf$float32)
    if(pad==TRUE){
      image<-tf$image$resize_with_pad(image, as.integer(height), as.integer(width), method = "bilinear")
    }else{
      image<-tf$image$resize(image,size = size)
    }
    if (!standardize) image <- tf$image$convert_image_dtype(image, dtype = tf$uint8)
    image<-list(image=image,width=img_width,height=img_height,file=file)
  } else {
    image<-list(image = tf$zeros(as.integer(c(height, width, 3)), dtype = tf$float32),width=tf$cast(width, tf$int32),height=tf$cast(height, tf$int32),file=file)
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
#' @import tensorflow
loadImageResizeCrop <- function(data, height = 299, width = 299, standardize = FALSE) {
  # catch error caused by missing files and zero-length files
  if (!is.null(tryCatch({image <- tf$io$read_file(data[[1]]);
  image <- tf$image$decode_jpeg(image, channels = 3, try_recover_truncated = T)},silent=T, error = function(e) NULL))) {
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
    image <- tf$image$resize_with_pad(image, as.integer(height), as.integer(width), method = "bilinear")
  } else {
    image <- tf$zeros(as.integer(c(height, width, 3)), dtype = tf$float32)
  }
  if (!standardize) image <- tf$image$convert_image_dtype(image, dtype = tf$uint8)
  image
}


#' @title Load image and return a tensor with an image and a corresponding label.
#'
#' @description Load image and return a tensor with an image and a corresponding label. Internal function to be called by image generator function.
#'
#' @param data a list with the first element being an image file path and the second element a label.
#' @param classes list of classes 
#' @param height the height the cropped image will be resized to.
#' @param width the width the cropped image will be resized to.
#' @param standardize standardize the image, TRUE or FALSE.
#'
#' @return An image and label tensor.
#' @import tensorflow
imageLabel <- function(data, classes, height = 299, width = 299, standardize = FALSE) {
  image <- loadImageResize(data[[1]], height, width, standardize)
  list(image, tf$cast(classes==data[[6]],tf$int16))
}



#' @title Load image, crop and return a tensor with an image and a corresponding label.
#'
#' @description Load image, crop and return a tensor with an image and a corresponding label. Internal function to be called by image generator function.
#'
#' @param data a list with the first element being an image file path, the next four elements being the bounding box coordinates and the last element a label
#' @param classes list of classes 
#' @param height the height the cropped image will be resized to.
#' @param width the width the cropped image will be resized to.
#' @param standardize standardize the image, TRUE or FALSE.
#'
#' @return An image and label tensor.
#' @import tensorflow
imageLabelCrop <- function(data, classes, height = 299, width = 299, standardize = FALSE) {
  image <- loadImageResizeCrop(list(data[[1]],data[[2]],data[[3]],data[[4]], data[[5]]), height, width, standardize)
  list(image, tf$cast(classes==data[[6]],tf$int16))
}


#' @title Perform image augmentation through random color adjustments on an image/label pair.
#'
#' @description Performs image augmentation on a image/label pair for training. Uses random brightness,contrast,saturation, and hue.
#'
#' @param image an image tensor.
#' @param label a label tensor.
#' @param rng a random number generator use to generate a random seed.
#'
#' @return An image and label tensor.
#' @import tensorflow
imageAugmentationColor<-function(image,label,rng){
  seed=rng$make_seeds(as.integer(2))
  seed=seed[1,]
  image<-tf$image$stateless_random_brightness(image,0.2,seed)
  image<-tf$image$stateless_random_contrast(image,0.3,0.7,seed)
  image<-tf$image$stateless_random_saturation(image,0.5,2,seed)
  image<-tf$image$stateless_random_hue(image,0.2,seed)
  list(image,label)
}

#' @title Perform random geometric transformations on an image.
#'
#' @description Returns a keras model that performs random geometric transformations on an image.
#'
#' @return A keras model.
#' @import keras
imageAugmentationGeometry<-function(){
  model<-keras_model_sequential()
  model<-layer_random_flip(model,mode="horizontal")
  model<-layer_random_zoom(model,c(0,-.2),c(0,-0.2),fill_mode="constant")
  model<-layer_random_rotation(model,c(-0.1,0.1),fill_mode="constant")
  model
}