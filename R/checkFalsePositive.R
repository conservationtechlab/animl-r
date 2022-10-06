#' Leverage sequences to classify images
#'
#' Images must be within maxdiff seconds of each other to be considered in sequence.
#' This function retains "Empty" classification even if other images within the
#' sequence are predicted to contain animals.
#' Classification confidence is weighted by MD confidence.
#'
#' @param imagesallanimal subselection of all images that contain MD animals
#' @param mlpredictions classifier predictions
#' @param classes list of all possible classes
#' @param emptycol integer that represents the empty class
#' @param maxdiff maximum difference between images in seconds to be included in a sequence, defaults to 60
#'
#' @return reclassified imagesallanimal dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' }
checkFalsePositive <- function(positives, buffer = 10){

  stations <- unique(positives$Station)
  #loop through all images

  system.time(for(s in stations){

    subset <- positives[positives$Station == s,]

    for (crop in nrow(subset)){
      
      similar = []
      
    }
    #only one image in the sequence

  })
}
checkFalsePositive(animals)
