#' Return MD empty, vehicle and human images in a dataframe
#'
#' @param manifest all megadetector frames
#'
#' @return list of empty/human/vehicle allframes with md classification
#' @export
#'
#' @examples
#' \dontrun{
#' empty <- get_empty(imagesall)
#' }
get_empty <- function(manifest) {
  if (!is(manifest, "data.frame")) { stop("'manifest' must be Data Frame")}
  
  empty <- manifest[manifest$category != 1, ]

  if (nrow(empty) == 0) {
    empty <- data.frame(matrix(ncol = ncol(manifest), nrow = 0))
    colnames(empty) <- names(manifest)
    return(empty)
  }
  empty$prediction <- NA
  empty$confidence <- NA

  categories <- unique(manifest$category)
  if (0 %in% categories) {
    empty[empty$category == 0, ]$prediction <- "empty"
    empty$confidence[empty$category == 0] <- 1
  }
  if (2 %in% categories) {
    empty[empty$category == 2, ]$prediction <- "human"
    empty$confidence[empty$category == 2] <- empty$conf[empty$category == 2]
  }
  if (3 %in% categories) {
    empty[empty$category == 3, ]$prediction <- "vehicle"
    empty$confidence[empty$category == 3] <- empty$conf[empty$category == 3]
  }
  return(empty)
}


#' Return a dataframe of only MD animals
#'
#' @param manifest all megadetector frames
#'
#' @return animal frames classified by MD
#' @export
#'
#' @examples
#' \dontrun{
#' animals <- get_animals(imagesall)
#' }
get_animals <- function(manifest){
  if (!is(manifest, "data.frame")) { stop("'manifest' must be Data Frame")}
  return(manifest[manifest$category==1,])
}


#' Splits the manifest into training validation and test datasets for training
#'
#' @param manifest list of files to split for training
#' @param out_dir location to save split lists to
#' @param label_col column name containing class labels
#' @param file_col column containing file paths
#' @param percentage fraction of data dedicated to train-val-test
#' @param seed RNG seed, if none will pick one at random within [0,100]
#'
#' @return train manifest, validate manifest, test manifest, stats file
#' @export
#'
#' @examples
#' \dontrun{
#'   output <- train_val_test(manifest)
#' }
train_val_test <- function(manifest, out_dir=NULL, label_col="class",
                           file_col='filepath', percentage=c(0.7, 0.2, 0.1),
                           seed=NULL){
  animl_py <- load_animl_py()
  animl_py$train_val_test(manifest, out_dir=out_dir, label_col=label_col,
                          file_col=file_col, percentage=percentage, seed=NULL)
}
