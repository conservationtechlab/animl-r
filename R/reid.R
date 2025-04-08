#' Title
#'
#' @param file_path 
#' @param device 
#'
#' @returns
#' @export
#'
#' @examples
load_miewid <- function(file_path, device=NULL){
  if(reticulate::py_module_available("animl")){animl_py <- reticulate::import("animl")}
  else{ stop('animl-py environment must be loaded first via reticulate') }
  
  animl_py$load_miew(file_path, device=device)
}


extract_embeddings <- function(manifest, miew_model, file_col="FilePath", batch_size=1, workers=1){
  if(reticulate::py_module_available("animl")){animl_py <- reticulate::import("animl")}
  else{ stop('animl-py environment must be loaded first via reticulate') }
  
  animl_py$extract_embeddings(manifest, miew_model, file_col=file_col, 
                              batch_size=as.integer(batch_size), workers=as.integer(workers))
}