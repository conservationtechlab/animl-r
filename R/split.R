#' Return MD empty, vehicle and human images in a dataframe
#'
#' @param manifest all megadetector frames
#'
#' @return list of empty/human/vehicle allframes with md classification
#' @export
#'
#' @examples
#' \dontrun{
#' empty <- getEmpty(imagesall)
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
#' animals <- getAnimals(imagesall)
#' }
get_animals <- function(manifest){
  if (!is(manifest, "data.frame")) { stop("'manifest' must be Data Frame")}
  return(manifest[manifest$category==1,])
}
