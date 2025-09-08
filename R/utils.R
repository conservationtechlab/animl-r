#' Load animl-py if available
#'
#' @return animl-py module
#' @export
#'
#' @examples
#' \dontrun{animl_py <- load_animl_py()}
load_animl_py <- function() {
  if(reticulate::py_module_available("animl")){animl_py <- reticulate::import("animl")}
  else{ stop('animl-py environment must be loaded first via reticulate') }
  return(animl_py)
}


#' Download a given model
#'
#' @param model_url url of the model to download
#' @param out_dir Directory to save the model.
#'
#' @return
#' @export
#'
#' @examples
download_model <- function(model_url, out_dir='models'){
  animl_py <- load_animl_py()
  animl_py$download_model(model_url, outdir=out_dir)
  
}


#' List models available for download
#'
#' @return
#' @export
#'
#' @examples
list_models <- function(){
  animl_py <- load_animl_py()
  animl_py$list_models
}
