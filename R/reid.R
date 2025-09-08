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
load_miew <- function(file_path, device=NULL){
  animl_py <- load_animl_py()
  animl_py$load_miew(file_path, device=device)
}


#' Extract Embeddings from MiewID
#'
#' @param miew_model loaded miewid model
#' @param manifest list of files 
#' @param file_col column name containing file paths
#' @param batch_size batch size for generator
#' @param num_workers number of workers for generator
#' @param device device to run model on
#'
#' @returns matrix of embeddings
#' @export
#'
#' @examples
#' \dontrun{embeddings = extract_embeddings(manifest, miew)}
extract_miew_embeddings <- function(miew_model, manifest, file_col="filepath", 
                                    batch_size=1, num_workers=1, device=NULL){
  animl_py <- load_animl_py()
  animl_py$extract_miew_embeddings(miew_model, manifest, file_col=file_col,
                                   batch_size=as.integer(batch_size), 
                                   num_workers=as.integer(num_workers),
                                   device=device)
}
