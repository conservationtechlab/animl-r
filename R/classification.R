#' Load a Classifier Model with animl-py
#'
#' @param model_path path to model
#' @param len_classes path to class list
#' @param device send model to the specified device
#' @param architecture model architecture
#'
#' @return list of c(classifier, class_list)
#' @export
#'
#' @examples
#' \dontrun{andes <- loadModel('andes_v1.pt','andes_classes.csv')}
load_classifier <- function(model_path, len_classes, device=NULL, architecture="CTL"){
  if(reticulate::py_module_available("animl")){
    animl_py <- reticulate::import("animl")
  }
  else{ stop('animl-py environment must be loaded first via reticulate') }
  
  animl_py$load_classifier(model_path, as.integer(len_classes), device=device, architecture=architecture)
}


#' Save model state weights
#'
#' @param model 
#' @param out_dir 
#' @param epoch 
#' @param stats 
#' @param optimizer 
#' @param scheduler 
#'
#' @returns None
#' @export
#'
#' @examples
#' \dontrun{save_classifier(model, 'models/', 10, {'acc': 0.85})}
save_classifier <- function(model, out_dir, epoch, stats, optimizer=NULL, scheduler=NULL){
  if(reticulate::py_module_available("animl")){
    animl_py <- reticulate::import("animl")
  }
  else{ stop('animl-py environment must be loaded first via reticulate') }
  
  animl_py$save_classifier(model_path, out_dir, epoch, stats, optimizer=optimizer, scheduler=scheduler)
}

#' Load class list .csv file
#'
#' @param classlist_file path to class list
#'
#' @returns dataframe version of csv
#' @export
#'
#' @examples
load_class_list <- function(classlist_file){
  read.csv(classlist_file)
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
  classify <- function(model, detections, device=NULL, out_file=NULL,
                       file_col='Frame', crop=TRUE, normalize=TRUE,
                       resize_width=480, resize_height=480,
                       batch_size=1, workers=1){
  
  # check if animl-py is available
  if(reticulate::py_module_available("animl")){ animl_py <- reticulate::import("animl")}
  else{ stop('animl-py environment must be loaded first via reticulate')}
  
  animl_py$classify(model, detections, device=device, out_file=out_file,
                    file_col=file_col, crop=crop, normalize=normalize, 
                    resize_width=resize_width, resize_height=resize_height,
                    batch_size=as.integer(batch_size), num_workers=as.integer(workers))
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
single_classification <- function(animals, predictions_raw, class_list){
  if(reticulate::py_module_available("animl")){ animl_py <- reticulate::import("animl")}
  else{ stop('animl-py environment must be loaded first via reticulate')}

  animl_py$individual_classification(animals, predictions_raw, class_list)
}
