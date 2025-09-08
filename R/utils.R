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
#' @return None
#' @export
#'
#' @examples
#' \dontrun{download_model('animl_py$CLASSIFIER$get('SDZWA_savanna_v3')')}
download_model <- function(model_url, out_dir='models'){
  animl_py <- load_animl_py()
  animl_py$download_model(model_url, outdir=out_dir)
  
}


#' List models available for download
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{list_models()}
list_models <- function(){
  animl_py <- load_animl_py()
  animl_py$list_models
}
