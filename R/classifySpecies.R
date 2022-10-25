#' Classifies Crops Using Specified Models
#'
#' @param input either dataframe with MD crops or list of filenames
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
classifySpecies <- function(input, model, resize = 299, standardize = TRUE, batch = 32, workers = 1) {
  if (!file.exists(model)) { stop("The given model file does not exist.") }

  model <- keras::load_model_hdf5(model)
  steps <- ceiling(nrow(input) / batch)

  #crops
  if(is(input, "data.frame")){
    filecol <- which(colnames(input) %in% c("file", "Frame"))[1]
    dataset <- cropImageGenerator(input[, filecol], input[, c("bbox1", "bbox2", "bbox3", "bbox4")],
                                  resize_height = resize, resize_width = resize,
                                  standardize = standardize, batch = batch)
    
  }
  else if (is(input, "list")) {
    dataset <- ImageGenerator(input, resize_height = resize, resize_width = resize, 
                              standardize = standardize, batch = 32) 
  }
  else { stop("Input must be a data frame of crops or list of file names.") }
 
  predict(model, dataset, step = steps, workers = workers, verbose = 1)
}
