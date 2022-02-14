#############################################
#load MegaDetector tensorflow model from .pb file
#' Title
#'
#' @param modelfile .pb file obtained from megaDetector
#'
#' @return a tfsession containing the MD model
#' @export
#'
#' @examples
#' \dontrun{
#' mdmodel<-"megadetector_v4.1.pb"
#' mdsession<-loadMDModel(mdmodel)
#' }
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
