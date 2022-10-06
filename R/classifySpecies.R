#' Classifies Crops Using Specified Models
#'
#' @param mdresults flattened mdresults dataframe
#' @param model models with which to classify species
#' @param resize resize images before classification, defaults to 299x299px
#' @param standardize standardize images, defaults to FALSE
#' @param batch_size number of images processed in each batch (keep small)
#' @param workers number of cores
#'
#' @return a matrix of likelihoods for each class for each image
#' @export
#'
#' @examples
#' \dontrun{
#' pred <- classifySpecies(imagesallanimal, paste0(modelfile, ".h5"),
#'                       resize = 456, standardize = FALSE, batch_size = 64, workers = 8)
#' }
classifySpecies <- function(mdresults, model, resize = 299, crop=TRUE, standardize = TRUE, batch_size = 32, workers = 1) {
  if (!file.exists(model)) {
    stop("The given model file does not exist. Check path.")
  }

  model <- keras::load_model_hdf5(model)
  predict_steps <- ceiling(nrow(mdresults) / batch_size)
  
  if(crop){
    if (!is(mdresults, "data.frame")) {
      stop("'mdresults' must be DataFrame.")
    }
    filecol <- which(colnames(mdresults) %in% c("file", "Frame"))[1]
    
    dataset <- cropImageGenerator(mdresults[, filecol], mdresults[, c("bbox1", "bbox2", "bbox3", "bbox4")],
                                  resize_height = resize, resize_width = resize,
                                  standardize = standardize, batch_size = batch_size
    )
  }
  else{
    #classifySpeciesFull
  }
  predict(model, dataset, step = predict_steps, workers = workers, verbose = 1)
}



