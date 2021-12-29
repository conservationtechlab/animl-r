#############################################
#load MegaDetector tensorflow model from .pb file
#' Title
#'
#' @param modelfile .pb file obtained from megaDetector
#'
#' @return
#' @export
loadMDModel<-function(modelfile){
  tf <- reticulate::import("tensorflow")
  if(strsplit(tf$version$VERSION,".",fixed=T)[[1]][1]==1){
    #Tensorflow 1.x
    tfsession<-tf$Session()
    f<-tf$io$gfile$GFile(modelfile,"rb")
    tfgraphdef<-tf$compat$v1$GraphDef()
    tfgraphdef$ParseFromString(f$read())
    tfsession$graph$as_default()
    tf$import_graph_def(tfgraphdef,name="")
  }else{
    #Tensorflow 2.x
    tfsession<-tf$compat$v1$Session()
    f<-tf$io$gfile$GFile(modelfile,"rb")
    tfgraphdef<-tf$compat$v1$GraphDef()
    tfgraphdef$ParseFromString(f$read())
    tfsession$graph$as_default()
    tf$import_graph_def(tfgraphdef,name="")
  }
  tfsession
}
#'
#' #' Load saved MD model, NOT CURRENTLY IN USE
#' #'
#' #' @param modelpath
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' loadMDSavedModel<-function(modelpath){
#'   tfmodel<-load_model_tf(modelpath)
#'   tfsession<-tf$compat$v1$Session(graph=tfmodel$graph)
#'   tfsession
#' }
