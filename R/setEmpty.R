#' Title
#'
#' @param allframes all megadetector allframes
#'
#' @return list of empty/human/vehicle allframes with md classification
#' @export
#'
#' @examples
#' \dontrun{
#' setEmpty(imagesall)
#' }
<<<<<<< HEAD
setEmpty <- function(crops) {
  empty <- crops[crops$max_detection_category != 1, ]
  if (nrow(empty) == 0) {
    empty <- data.frame(matrix(ncol = ncol(crops), nrow = 0))
    colnames(empty) <- names(crops)
=======
setEmpty <- function(allframes){
  if(!is(allframes,"data.frame"){stop("'allframes' must be Data Frame")}
  empty <- allframes[allframes$max_detection_category!=1,]
  if(nrow(empty) == 0){
    empty <- data.frame(matrix(ncol = ncol(allframes), nrow = 0))
    colnames(empty) <- names(allframes)
>>>>>>> origin
    return(empty)
  }
  empty$prediction <- NA
  empty$confidence <- NA

<<<<<<< HEAD
  categories <- unique(crops$max_detection_category)
  if (0 %in% categories) {
    empty[empty$max_detection_category == 0, ]$prediction <- "empty"
    empty$confidence[empty$max_detection_category == 0] <- 1 - empty$max_conf[empty$max_detection_category == 0]
=======
  categories <- unique(allframes$max_detection_category)
  if(0 %in% categories){
    empty[empty$max_detection_category==0,]$prediction = "empty"
    empty$confidence[empty$max_detection_category==0] <- 1-empty$max_conf[empty$max_detection_category ==0]
>>>>>>> origin
  }
  if (2 %in% categories) {
    empty[empty$max_detection_category == 2, ]$prediction <- "human"
    empty$confidence[empty$max_detection_category == 2] <- empty$conf[empty$max_detection_category == 2]
  }
  if (3 %in% categories) {
    empty[empty$max_detection_category == 3, ]$prediction <- "vehicle"
    empty$confidence[empty$max_detection_category == 3] <- empty$conf[empty$max_detection_category == 3]
  }
  empty
}
