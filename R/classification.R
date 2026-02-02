#' Load a Classifier Model and Class_list
#'
#' @param model_path path to model
#' @param classes path to class list or loaded class list
#' @param device send model to the specified device
#' @param architecture model architecture
#'
#' @return classifier model, class list
#' @export
#'
#' @examples
#' \dontrun{
#' classes <- load_class_list('sdzwa_andes_v1_classes.csv')
#' andes <- load_classifier('andes_v1.pt', nrow(classes))}
load_classifier <- function(model_path, classes, device=NULL, architecture="CTL"){
  animl_py <- .animl_internal$animl_py
  if(is.numeric(classes)){ classes = as.integer(classes)}
  animl_py$load_classifier(model_path, classes, device=device, architecture=architecture)
}


#' Save model state weights
#'
#' @param model pytorch model
#' @param out_dir directory to save model to
#' @param epoch  current training epoch
#' @param stats performance metrics of current epoch
#' @param optimizer pytorch optimizer (optional)
#' @param scheduler pytorch scheduler (optional)
#'
#' @returns None
#' @export
#'
#' @examples
#' \dontrun{save_classifier(model, 'models/', 10, list(acc = 0.85))}
save_classifier <- function(model, out_dir, epoch, stats, optimizer=NULL, scheduler=NULL){
  animl_py <- .animl_internal$animl_py
  animl_py$save_classifier(model, out_dir, epoch, reticulate::r_to_py(stats), optimizer=optimizer, scheduler=scheduler)
}

#' Load class list .csv file
#'
#' @param classlist_file path to class list
#'
#' @returns dataframe version of csv
#' @export
#'
#' @examples
#' \dontrun{classes <- load_class_list('andes_classes.csv')}
load_class_list <- function(classlist_file){
  utils::read.csv(classlist_file)
}


#' Infer Species for Given Detections
#'
#' @param model loaded classifier model
#' @param detections manifest of animal detections
#' @param resize_width image width input size
#' @param resize_height image height input size
#' @param file_col column in manifest containing file paths
#' @param crop use bbox to crop images before feeding into model
#' @param normalize normalize the tensor before inference
#' @param batch_size batch size for generator 
#' @param num_workers number of processes
#' @param device send model to the specified device
#' @param out_file path to csv to save results to
#'
#' @return detection manifest with added prediction and confidence columns
#' @export
#'
#' @examples
#' \dontrun{animals <- classify(classifier, animals, file_col='filepath')}
classify <- function(model, detections, 
                       resize_width=480, resize_height=480,
                       file_col='filepath', crop=TRUE, normalize=TRUE,
                       batch_size=1, num_workers=1,
                       device=NULL, out_file=NULL){
  
    animl_py <- .animl_internal$animl_py
    animl_py$classify(model, detections,
                      resize_width=as.integer(resize_width),
                      resize_height=as.integer(resize_height), 
                      file_col=file_col, crop=crop, normalize=normalize,
                      batch_size=as.integer(batch_size),
                      num_workers=as.integer(num_workers),
                      device=device, out_file=out_file)
}


#' Get Maximum likelihood label for each Detection
#'
#' @param animals manifest of animal detections 
#' @param empty manifest of md human, vehicle and empty images
#' @param predictions_raw softmaxed likelihoods from predict_species
#' @param class_list list of class labels
#'
#' @returns dataframe with prediction and confidence columns
#' @export
#'
#' @examples
#' \dontrun{animals <- single_classification(animals, empty, pred_raw, class_list)}
single_classification <- function(animals, empty, predictions_raw, class_list){
  animl_py <- .animl_internal$animl_py
  animl_py$single_classification(animals, empty, predictions_raw, class_list)
}
