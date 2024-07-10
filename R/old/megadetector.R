#############################################
#' Load MegaDetector model file from directory or file
#'
#' @param modelfile .pb file or directory obtained from megaDetector
#'
#' @return a tfsession containing the MD model
#' @export
#' @import tensorflow
#'
#' @examples
#' \dontrun{
#' mdmodel <- "megadetector_v4.1.pb"
#' mdsession <- MegaDetector(mdmodel)
#' }
MegaDetector <- function(model_path) {
  # converted keras folder
  if (dir.exists(modelfile) && file.exists(paste0(modelfile, "/saved_model.pb"))) {
    model <- tf$keras$models$load_model(modelfile)
    class(model) <- append(class(model), "mdmodel")
    model
  }
  else {
    if (!file.exists(modelfile)) {
      stop("The given MD model does not exist. Check path.")
    }
    tfsession <- tf$compat$v1$Session()
    f <- tf$io$gfile$GFile(modelfile, "rb")
    tfgraphdef <- tf$compat$v1$GraphDef()
    tfgraphdef$ParseFromString(f$read())
    tfsession$graph$as_default()
    tf$import_graph_def(tfgraphdef, name = "")
    class(tfsession) <- append(class(tfsession), "mdsession")
    tfsession
  }
}