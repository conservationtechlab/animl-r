#' Load a Classifier Model with animl-py
#'
#' @param model_path path to model
#' @param len_classes path to class list
#' @param device send model to the specified device
#' @param architecture model architecture
#'
#' @return classifier model
#' @export
#'
#' @examples
#' \dontrun{andes <- load_classifier('andes_v1.pt')}
load_classifier <- function(model_path, len_classes, device=NULL, architecture="CTL"){
  animl_py <- load_animl_py()
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
  animl_py <- load_animl_py()
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
#' \dontrun{classes <- load_class_list('andes_classes.csv')}
load_class_list <- function(classlist_file){
  read.csv(classlist_file)
}


#' Infer Species for Given Detections
#'
#' @param model loaded classifier model
#' @param detections manifest of animal detections
#' @param device send model to the specified device
#' @param out_file path to csv to save results to
#' @param file_col column in manifest containing file paths
#' @param crop use bbox to crop images before feeding into model
#' @param normalize normalize the tensor before inference
#' @param resize_width image width input size
#' @param resize_height image height input size
#' @param batch_size batch size for generator 
#' @param workers number of processes 
#'
#' @return detection manifest with added prediction and confidence columns
#' @export
#'
#' @examples
#' \dontrun{animals <- classify(classifier, animals, file_col='filepath')}
  classify <- function(model, detections, device=NULL, out_file=NULL,
                       file_col='frame', crop=TRUE, normalize=TRUE,
                       resize_width=480, resize_height=480,
                       batch_size=1, workers=1){
  animl_py <- load_animl_py()
  animl_py$classify(model, detections, device=device, out_file=out_file,
                    file_col=file_col, crop=crop, normalize=normalize, 
                    resize_width=as.integer(resize_width), resize_height=as.integer(resize_height),
                    batch_size=as.integer(batch_size), num_workers=as.integer(workers))
}


#' Get Maximum likelihood label for each Detection
#'
#' @param animals manifest of animal detections 
#' @param predictions_raw softmaxed likelihoods from predict_species
#' @param class_list list of class labels
#'
#' @returns dataframe with prediction and confidence columns
#' @export
#'
#' @examples
#' \dontrun{animals <- single_classification(animals, pred_raw, class_list)}
single_classification <- function(animals, empty, predictions_raw, class_list){
  animl_py <- load_animl_py()
  animl_py$single_classification(animals, empty, predictions_raw, class_list)
}
