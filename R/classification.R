#' Title
#'
#' @param model_path path to model
#' @param class_file path to class list
#' @param device send model to the specified device
#'
#' @return
#' @export
#'
#' @examples
loadModel <- function(model_path, class_file, device="gpu"){
  if(py_module_available("animl")){
    animl_py <- import("animl")
  }
  else{
    stop('animl-py environment must be loaded first via reticulate')
  }
  animl_py$load_model(model_path, class_file, device=device)
}


#' Title
#'
#' @param detections 
#' @param model 
#' @param classes 
#' @param device 
#' @param out_file 
#' @param file_col 
#' @param crop 
#' @param resize 
#' @param standardize 
#' @param batch_size 
#' @param workers 
#' @param channel_last 
#' @param raw 
#'
#' @return
#' @export
#'
#' @examples
predictSpecies <- function(detections, model, classes, device='gpu', out_file=NULL,
                           file_col='Frame', crop=TRUE, resize=299, standardize=TRUE,
                           batch_size=1, workers=1, channel_last=FALSE, raw=FALSE){
  
  # check if animl-py is available
  if(py_module_available("animl")){ animl_py <- import("animl")}
  else{ stop('animl-py environment must be loaded first via reticulate')}
  
  animl_py$predict_species(detections, model, classes, device=device, out_file=out_file,
                           file_col=file_col, crop=crop, resize=resize, standardize=standardize,
                           batch_size=as.integer(batch_size), workers=as.integer(workers), 
                           channel_last=channel_last, raw=raw)
  
}
