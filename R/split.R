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
    empty[which(empty$category == 0), "prediction"] <- "empty"
    empty[which(empty$category == 0), "confidence"] <- 1
  }
  if (2 %in% categories) {
    empty[which(empty$category == 2), "prediction"] <- "human"
    empty[which(empty$category == 2), "confidence"] <- empty$conf[which(empty$category == 2)]
  }
  if (3 %in% categories) {
    empty[which(empty$category == 3), "prediction"] <- "vehicle"
    empty[which(empty$category == 3), "confidence"] <- empty$conf[which(empty$category == 3)]
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
#' @param label_col column name containing class labels
#' @param file_col column containing file paths
#' @param conf_col column containing prediction confidence
#' @param out_dir location to save split lists to
#' @param val_size fraction of data dedicated to validation
#' @param test_size fraction of data dedicated to testing
#' @param random_state RNG seed for reproducibility
#'
#' @return train manifest, validate manifest, test manifest
#' @export
#'
#' @examples
#' \dontrun{
#'   output <- train_val_test(manifest)
#' }
train_val_test <- function(manifest, label_col="class", file_col='filepath', 
                           conf_col = 'confidence', out_dir=NULL,
                           val_size= 0.1, test_size = 0.1, random_state=42){
  animl_py <- get("animl_py", envir = parent.env(environment()))
  animl_py$train_val_test(manifest, label_col="class", file_col='filepath', 
                          conf_col = 'confidence', out_dir=NULL,
                          val_size= 0.1, test_size = 0.1, random_state=42)
}
