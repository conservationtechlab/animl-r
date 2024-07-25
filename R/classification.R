#' Load a Classifier Model with animl-py
#'
#' @param model_path path to model
#' @param class_file path to class list
#' @param device send model to the specified device
#'
#' @return list of c(classifier, class_list)
#' @export
#'
#' @examples
#' \dontrun{andes <- loadModel('andes_v1.pt','andes_classes.csv')}
loadModel <- function(model_path, class_file, device=NULL){
  if(reticulate::py_module_available("animl")){
    animl_py <- reticulate::import("animl")
  }
  else{ stop('animl-py environment must be loaded first via reticulate') }
  
  animl_py$load_model(model_path, class_file, device=device)
}


#' Infer Species for Given Detections
#'
#' @param detections manifest of animal detections
#' @param model loaded classifier model
#' @param classes data.frame of classes
#' @param device send model to the specified device
#' @param out_file path to csv to save results to
#' @param file_col column in manifest containing file paths
#' @param crop use bbox to crop images before feeding into model
#' @param resize model expected dimensions
#' @param standardize standardize color
#' @param batch_size batch size for generator 
#' @param workers number of processes 
#' @param channel_last change matrix to BxWxHxC
#' @param raw output raw logits in addition to manifest
#'
#' @return detection manifest with added prediction and confidence columns
#' @export
#'
#' @examples
#' \dontrun{animals <- predictSpecies(animals, classifier[[1]], classifier[[2]], raw=FALSE)}
predictSpecies <- function(detections, model, classes, device=NULL, out_file=NULL,
                           file_col='Frame', crop=TRUE, resize=as.integer(299), standardize=TRUE,
                           batch_size=1, workers=1, channel_last=FALSE, raw=FALSE){
  
  # check if animl-py is available
  if(reticulate::py_module_available("animl")){ animl_py <- reticulate::import("animl")}
  else{ stop('animl-py environment must be loaded first via reticulate')}
  
  animl_py$predict_species(detections, model, classes, device=device, out_file=out_file,
                           file_col=file_col, crop=crop, resize=resize, standardize=standardize,
                           batch_size=as.integer(batch_size), workers=as.integer(workers), 
                           channel_last=channel_last, raw=raw)
}
