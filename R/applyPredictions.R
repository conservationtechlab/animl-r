#' Apply Classifier Predictions and Merge DataFrames
#'
#' @param detections Dataframe of MD detections
#' @param class.file .txt file containing common names for species classes
#' @param pred Classifier predictions for animal crops
#' @param counts Returns a table of all predictions, defaults to FALSE
#'
#' @return fully merged dataframe with Species predictions
#' @export
#'
#' @examples
#' \dontrun{
#' alldata <- applyPredictions(animals,empty,classfile,pred,counts = FALSE)
#' }
applyPredictions <- function(detections, pred, class.file, counts = FALSE) {
  if (!is(detections, "data.frame")) { 
    stop("Must supply a dataframe.")
  }
  if (!file.exists(class.file)) { 
    stop("The given class file does not exist.")
  }
  # read in class list (ASSUMES 'x' VARIABLE FROM TF OUTPUT)
  classes <- read.table(class.file, strings.as.factors = F)$x

  detections$prediction <- classes[apply(pred, 1, which.max)]
  detections$confidence <- apply(pred, 1, max) * detections$conf

  if (counts) { 
    table(classes[apply(pred, 1, which.max)])
  }

  detections
}
