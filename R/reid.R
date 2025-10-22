#' Load MiewID model
#'
#' @param file_path path to model weights
#' @param device toggle cpu or gpu
#'
#' @returns meiwid model
#' @export
#'
#' @examples
#' \dontrun{miew = load_miewid("/home/kyra/matchypatchy/Models/miewid_v3.bin")}
load_miewid <- function(file_path, device=NULL){
  if(reticulate::py_module_available("animl")){animl_py <- reticulate::import("animl")}
  else{ stop('animl-py environment must be loaded first via reticulate') }
  
  animl_py$load_miew(file_path, device=device)
}


#' Extract Embeddings from MiewID
#'
#' @param manifest list of files 
#' @param miew_model loaded miewid model
#' @param file_col column name containing file paths
#' @param batch_size batch size for generator
#' @param workers number of workers for generator
#'
#' @returns matrix of embeddings
#' @export
#'
#' @examples
#' \dontrun{embeddings = extract_embeddings(manifest, miew)}
extract_embeddings <- function(manifest, miew_model, file_col="FilePath", batch_size=1, workers=1){
  if(reticulate::py_module_available("animl")){animl_py <- reticulate::import("animl")}
  else{ stop('animl-py environment must be loaded first via reticulate') }
  
  animl_py$extract_embeddings(manifest, miew_model, file_col=file_col, 
                              batch_size=as.integer(batch_size), workers=as.integer(workers))
}
