#' Title
#'
#' @param crops all megadetector crops
#'
#' @return list of empty/human/vehicle crops with md classification
#' @export
#'
#' @examples
setEmpty <- function(crops){
  empty <- crops[crops$max_detection_category!=1,]
  empty$prediction <-NA
  empty$confidence <-NA
  empty[empty$max_detection_category==0,]$prediction = "empty"
  empty[empty$max_detection_category==2,]$prediction = "human"
  empty[empty$max_detection_category==3,]$prediction = "vehicle"
  empty$confidence[empty$max_detection_category==0] <- 1-empty$max_detection_conf[empty$max_detection_category ==0]
  empty$confidence[empty$max_detection_category==2] <- empty$md_confidence[empty$max_detection_category ==2]
  empty$confidence[empty$max_detection_category==3] <- empty$md_confidence[empty$max_detection_category ==3]
  empty
}
