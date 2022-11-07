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
setEmpty <- function(allframes) {
  if (!is(allframes, "data.frame")) {
    stop("'allframes' must be Data Frame")
  }
  empty <- allframes[allframes$MaxCategory != 1, ]
  if (nrow(empty) == 0) {
    empty <- data.frame(matrix(ncol = ncol(allframes), nrow = 0))
    colnames(empty) <- names(allframes)
    return(empty)
  }
  empty$prediction <- NA
  empty$confidence <- NA

  categories <- unique(allframes$MaxCategory)
  if (0 %in% categories) {
    empty[empty$MaxCategory == 0, ]$prediction <- "empty"
    empty$confidence[empty$MaxCategory == 0] <- 1 - empty$MaxMDConfidence[empty$MaxCategory == 0]
  }
  if (2 %in% categories) {
    empty[empty$MaxCategory == 2, ]$prediction <- "human"
    empty$confidence[empty$MaxCategory == 2] <- empty$conf[empty$MaxCategory == 2]
  }
  if (3 %in% categories) {
    empty[empty$MaxCategory == 3, ]$prediction <- "vehicle"
    empty$confidence[empty$MaxCategory == 3] <- empty$conf[empty$MaxCategory == 3]
  }
  empty
}
