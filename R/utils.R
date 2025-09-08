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
