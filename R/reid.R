#' Title
#'
#' @param file_path 
#' @param deivce 
#'
#' @returns
#' @export
#'
#' @examples
load_miewid <- function(file_path, deivce=NULL){
  if(reticulate::py_module_available("animl")){animl_py <- reticulate::import("animl")}
  else{ stop('animl-py environment must be loaded first via reticulate') }
}


extract_embeddings <- function(manifest, miew_model, file_col="FilePath", batch_size=1, workers=1){
  if(reticulate::py_module_available("animl")){animl_py <- reticulate::import("animl")}
  else{ stop('animl-py environment must be loaded first via reticulate') }
}