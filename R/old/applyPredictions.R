#' Apply Classifier Predictions and Merge DataFrames
#'
#' @param animals Set of animal crops/images
#' @param pred Classifier predictions for animal crops/images
#' @param classfile .txt file containing common names for species classes
#' @param outfile File to which results are saved
#' @param counts Returns a table of all predictions, defaults to FALSE
#'
#' @return fully merged dataframe with Species predictions and confidence weighted by MD conf
#' @importFrom methods is
#' @export
#'
#' @examples
#' \dontrun{
#' alldata <- applyPredictions(animals,empty,classfile,pred,counts = FALSE)
#' }
applyPredictions <- function(animals, pred, classfile, outfile = NULL, counts = FALSE) {
  if (checkFile(outfile)) { return(loadData(outfile))}
  if (!is(animals, "data.frame")) { stop("'animals' must be DataFrame.")}
  if (!file.exists(classfile)) { stop("The given class file does not exist.")}

  classes <- utils::read.table(classfile, stringsAsFactors = F)$x

  animals$prediction <- classes[apply(pred, 1, which.max)]
  animals$confidence <- apply(pred, 1, max) * animals$conf

  if (counts) { table(classes[apply(pred, 1, which.max)])}

  # save data
  if(!is.null(outfile)) { saveData(animals, outfile)}
  
  animals
}