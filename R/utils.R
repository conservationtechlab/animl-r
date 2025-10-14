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
  animl_py <- get("animl_py", envir = parent.env(environment()))
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
  animl_py <- get("animl_py", envir = parent.env(environment()))
  animl_py$list_models
}
