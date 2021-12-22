#############################################
#' Load image with TF 35722 and resize for MD
#'
#' @param image_filepath the filepath of the image
#' @param height resize height, defaults to 299 px
#' @param width resize width, defualts to 299 px
#' @export
#'
#' @examples
#'
#' decode_img_resize(images[1]$FilePath)
decode_img_resize <- function(image_filepath, height = 299, width = 299) {

  size <- as.integer(c(height, width))

  image_filepath %>%
    tf$io$read_file() %>%
    tf$image$decode_jpeg(channels = 3,try_recover_truncated = T) %>%
    tf$image$convert_image_dtype(dtype = tf$float32) %>%
    tf$image$resize(size = size)
}

#############################################
#' Load image with TF, full size for MD
#'
#' @param image_filepath the file path of the image
#' @export
#'
#' @examples
#'
#' decode_img_full(images[1]$FilePath)
#'
decode_img_full <- function(image_filepath) {
  image_filepath %>%
    tf$io$read_file() %>%
    tf$io$decode_jpeg(channels = 3,try_recover_truncated = T)
}
