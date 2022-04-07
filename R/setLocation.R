#' Set Region/Site/Camera Information for Each Image
#'
#' @param files file manifest data frame
#' @param basedir directory from which files are obtained
#' @param adjust adjust starting position in folder hierarchy, defaults to 0
#' @param rename create a new unique name from region/site/camera/date, defaults to true
#' @param region general region of camera deployment, can be inputted manually or pulled from directory
#' @param site site of camera deployment, can be inputted manually or pulled from directory
#' @param camera camera name, can be inputted manually or pulled from directory
#'
#' @return file manifest with additional columns for region/site/camera
#' @export
#'
#' @examples
#' \dontrun{
#' setLocation(files,basedir)
#' }
setLocation <- function(files,basedir,adjust = 0,rename=TRUE,region=NA,site=NA,camera=NA){
  basedepth=length(strsplit(basedir,split="/")[[1]]) + adjust
  if(is.na(region)){
    files$Region<-sapply(files$Directory,function(x)strsplit(x,"/")[[1]][basedepth])
  }else{files$Region<-region}
  if(is.na(site)){files$Site<-sapply(files$Directory,function(x)strsplit(x,"/")[[1]][basedepth+1])
  }else{files$Site<-site}
  if(is.na(camera)){files$Camera<-sapply(files$Directory,function(x)strsplit(x,"/")[[1]][basedepth+2])
  }else{files$Camera<-camera}

  if(rename){
    files$NewName=paste(files$Region,files$Site,format(files$DateTimeOriginal,format="%Y%m%d_%H%M%S"),files$FileName,sep="_")
  }
  else{
    files$NewName=files$FileName
  }
  files
}




