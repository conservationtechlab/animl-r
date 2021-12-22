#' #' Initialize Directories
#' #'
#' #' @param basedir The directory in which all data and temporary files will be saved.
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' initiateOrbweaver <- function(basedir){
#'   datadir <- paste0(basedir,"Data/")
#'   cropdir <- paste0(basedir,"Crops/")
#'   tempdir <- paste0(basedir,"Tmp/")
#'   symlinkdir <- paste0(basedir,"Link/")
#'
#'   if(!dir.exists(datadir)){dir.create(datadir,recursive = T)}
#'   if(!dir.exists(cropdir)){dir.create(cropdir,recursive = T)}
#'   if(!dir.exists(tempdir)){dir.create(tempdir,recursive = T)}
#'   if(!dir.exists(symlinkdir)){dir.create(symlinkdir,recursive = T)}
#'
#'   imagefile<-"Images.csv"
#'   videoframefile<-"VideoFrames.csv"
#'   resultsfile<-"Results.csv"
#'   cropfile<-"Crops.csv"
#'   mdresults<-"mdres.RData"
#'   predresults<-"pred.RData"
#' }
