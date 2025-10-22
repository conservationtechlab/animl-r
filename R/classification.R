#' Load a Classifier Model with animl-py
#'
#' @param model_path path to model
#' @param class_file path to class list
#' @param device send model to the specified device
#' @param architecture model architecture
#'
#' @return list of c(classifier, class_list)
#' @export
#'
#' @examples
#' \dontrun{andes <- loadModel('andes_v1.pt','andes_classes.csv')}
load_model <- function(model_path, classes, device=NULL, architecture="CTL"){
  if(reticulate::py_module_available("animl")){
    animl_py <- reticulate::import("animl")
  }
  else{ stop('animl-py environment must be loaded first via reticulate') }
  
  animl_py$load_model(model_path, as.integer(classes), device=device, architecture=architecture)
}


#' Infer Species for Given Detections
#'
#' @param detections manifest of animal detections
#' @param model loaded classifier model
#' @param classes data.frame of classes
#' @param device send model to the specified device
#' @param out_file path to csv to save results to
#' @param raw output raw logits in addition to manifest
#' @param file_col column in manifest containing file paths
#' @param crop use bbox to crop images before feeding into model
#' @param resize_width image width input size
#' @param resize_height image height input size
#' @param normalize normalize the tensor before inference
#' @param batch_size batch size for generator 
#' @param workers number of processes 
#'
#' @return detection manifest with added prediction and confidence columns
#' @export
#'
#' @examples
#' \dontrun{animals <- predictSpecies(animals, classifier[[1]], classifier[[2]], raw=FALSE)}
  predict_species <- function(detections, model, device=NULL, out_file=NULL,
                           file_col='Frame', crop=TRUE, resize_width=299, resize_height=299,
                           normalize=TRUE, batch_size=1, workers=1){
  
  # check if animl-py is available
  if(reticulate::py_module_available("animl")){ animl_py <- reticulate::import("animl")}
  else{ stop('animl-py environment must be loaded first via reticulate')}
  
  animl_py$predict_species(detections, model, device=device, out_file=out_file,
                           file_col=file_col, crop=crop, resize_width=resize_width, resize_height=resize_height, 
                           normalize=normalize, batch_size=as.integer(batch_size), workers=as.integer(workers))
}


#' Get Maximum likelihood label for each Detection
#'
#' @param detections manifest of animal detections 
#' @param predictions_raw softmaxed likelihoods from predict_species
#' @param class_list list of class labels
#'
#' @returns dataframe with prediction and confidence columns
#' @export
#'
#' @examples
#' \dontrun{animals <- single_classification(animals, pred_raw, class_list)}
single_classification <- function(detections, predictions_raw, class_list){
  if(reticulate::py_module_available("animl")){ animl_py <- reticulate::import("animl")}
  else{ stop('animl-py environment must be loaded first via reticulate')}

  animl_py$single_classification(detections, predictions_raw, class_list)
}
