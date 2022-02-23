#############################################
#' Load image with TF 35722 and resize for MD
#'
#' @param image_filepath the filepath of the image
#' @param height resize height, defaults to 299 px
#' @param width resize width, defualts to 299 px
#' @export
#' @importFrom dplyr "%>%"
#'
#' @examples
#' \dontrun{
#'   dataset<-tfdatasets::dataset_map_and_batch(dataset,decode_img_resize,batch_size)
#' }
decode_img_resize <- function(image_filepath, height = 299, width = 299) {
  tf<-reticulate::import("tensorflow")
  size <- as.integer(c(height, width))

  image_filepath %>%
    tf$io$read_file() %>%
    tf$image$decode_jpeg(channels = 3,try_recover_truncated = T) %>%
    tf$image$convert_image_dtype(dtype = tf$float32) %>%
    tf$image$resize(size = size)
}

#' Load image with TF, full size for MD
#'
#' @param image_filepath the file path of the image
#' @export
#'
#' @examples
#' \dontrun{
#'   dataset<-tfdatasets::dataset_map_and_batch(dataset,decode_img_full,batch_size)
#' }
decode_img_full <- function(image_filepath) {
  tf<-reticulate::import("tensorflow")
  image_filepath %>%
    tf$io$read_file() %>%
    tf$io$decode_jpeg(channels = 3,try_recover_truncated = T)
}
