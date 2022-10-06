#' Classifies Crops Using Specified Models
#'
#' @param detections dataframe with md boxes or, if crop=F vector of file paths
#' @param model models with which to classify species
#' @param crop toggles the use of cropped images, defaults to TRUE
#' @param resize resize images before classification, defaults to 299x299px
#' @param standardize standardize images, defaults to FALSE
#' @param batch number of images processed in each batch (keep small)
#' @param workers number of cores
#'
#' @return a matrix of likelihoods for each class for each image
#' @export
#'
#' @examples
#' \dontrun{
#' pred <- classifySpecies(imagesallanimal, paste0(modelfile, ".h5"),
#'                       resize = 456, standardize = FALSE, batch = 64, workers = 8)
#' }
classifySpecies <- function(detections, model, crop = TRUE, resize = 299,
                            standardize = TRUE, batch = 32, workers = 1) {
  if (!file.exists(model)) {
    stop("The given model file does not exist. Check path.")
  }

  model <- keras::load_model_hdf5(model)
  predict.steps <- ceiling(nrow(detections) / batch)
  
  if(crop){
    if (!is(detections, "data.frame")) {
      stop("'detections' must be data frame.")
    }
    
    dataset <- imageGeneratorCrop(detections[, 'frame'], 
                                  detections[, c("bbox1", "bbox2", "bbox3", "bbox4")],
                                  resize.height = resize, resize.width = resize,
                                  standardize = standardize, batch = batch)
  }
  else{
    if (!is(detections, "vector")) {
      stop("'detections' must be vector of file paths.")
    }
    dataset <- imageGenerator(detections, resize.height = resize, 
                              resize.width = resize, standardize = standardize, 
                              batch = batch) 
  }
  
  #make predictions
  predict(model, dataset, step = predict.steps, workers = workers, verbose = 1)
}



