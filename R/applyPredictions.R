#' Apply Classifier Predictions and Merge DataFrames
#'
#' @param animals Set of animal crops
#' @param empty Set of empty, vehicle and human crops
#' @param classfile .txt file containing common names for species classes
#' @param pred Classifier predictions for animal crops
#' @param outfile File to which results are saved
#' @param counts Returns a table of all predictions, defaults to FALSE
#'
#' @return fully merged dataframe with Species predictions
#' @export
#'
#' @examples
#' \dontrun{
#' alldata <- applyPredictions(animals,empty,classfile,pred,counts = FALSE)
#' }
applyPredictions <- function(animals, empty, classfile, pred, outfile = NULL, counts = FALSE) {
  if (!is(animals, "data.frame")) {
    stop("'animals' must be DataFrame.")
  }
  if (!is(empty, "data.frame")) {
    stop("'empty' must be DataFrame.")
  }
  if (!file.exists(classfile)) {
    stop("The given class file does not exist.")
  }
  if (!is(pred, "data.frame")) {
    stop("'pred' must be DataFrame.")
  }

  classes <- read.table(classfile, stringsAsFactors = F)$x

  animals$prediction <- classes[apply(pred, 1, which.max)]
  animals$confidence <- apply(pred, 1, max) * animals$conf

  if (counts) {
    table(classes[apply(pred, 1, which.max)])
  }
  # merge with empty data
  alldata <- rbind(animals, empty)

  # save data
  saveData(alldata, outfile)

  alldata
}
