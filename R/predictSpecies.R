#' Classifies Crops Using Specified Models
#'
#' @param input either dataframe with MD crops or list of filenames
#' @param model models with which to classify species
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
#'                       resize = 456, standardize = FALSE, batch_size = 64, workers = 8)
#' }
predictSpecies <- function(input, model, resize = 456, standardize = FALSE, 
                           batch = 1, workers = 1) {
  
  if (!file.exists(model)) { stop("The given model file does not exist.") }
  
  model <- keras::load_model_hdf5(model)
  
  #crops
  if(is(input, "data.frame")){
    steps <- ceiling(nrow(input) / batch)
    filecol <- which(colnames(input) %in% c("file", "Frame"))[1]
    if("bbox1" %in% colnames(input)){
      dataset <- cropImageGenerator(input[, filecol], input[, c("bbox1", "bbox2", "bbox3", "bbox4")],
                                    resize_height = resize, resize_width = resize,
                                    standardize = standardize, batch = batch)
    }else{
      dataset <- ImageGenerator(input[, filecol], resize_height = resize, resize_width = resize, 
                                standardize = standardize, batch = batch)       
    }
    
  }
  else if (is.vector(input)) {
    steps <- ceiling(length(input) / batch)
    dataset <- ImageGenerator(input, resize_height = resize, resize_width = resize, 
                              standardize = standardize, batch = batch) 
  }
  else { stop("Input must be a data frame of crops or vector of file names.") }
  
  stats::predict(model, dataset, step = steps, workers = workers, verbose = 1)
}