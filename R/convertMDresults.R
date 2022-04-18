#' Convert the MD output into a data frame
#'
#' Each row is a MD bounding box, there can be multiple bounding boxes per image.
#'
#' @param mdresults list of bounding boxes for each image
#'
#' @return A dataframe with one entry for each bounding box
#' @export
#'
#' @examples
#' \dontrun{
#'  images<-read_exif(imagedir,tags=c("filename","directory","DateTimeOriginal","FileModifyDate"),
#'                    recursive = TRUE)
#'  colnames(images)[1]<-"FilePath"
#'  mdsession<-loadMDModel(mdmodel)
#'  mdres<-classifyImagesBatchMD(mdsession,images$FilePath,
#'                               resultsfile=mdresultsfile,checkpoint = 2500)
#'  mdresflat<-convertMDresults(mdres)
#' }
convertMDresults <- function(mdresults){
  images_flat<-data.frame(image_path=character(),md_class=numeric(),md_confidence=numeric(),
                          pixelx=numeric(),pixely=numeric(),
                          x1=numeric(),x2=numeric(),y1=numeric(),y2=numeric(),
                          xmin=numeric(),xmax=numeric(),ymin=numeric(),ymax=numeric())

  pb <- utils::txtProgressBar(min = 0, max = length(mdresults), style = 3)
  for(i in 1:length(mdresults)){
    #load image
    jpg<-jpeg::readJPEG(mdresults[[i]]$file)
    jpgy<-dim(jpg)[1]
    jpgx<-dim(jpg)[2]

    #get bounding boxes
    #get bounding boxes
    if(is.data.frame(mdresults[[i]]$detections)){
      s<-mdresults[[i]]$detections

      #extract bounding box
      for(j in 1:length(s)){
        xmin<-max(0,round(s[j,]$bbox1*jpgx,0))
        xmax<-min(jpgx,round((s[j,]$bbox1+s[j,]$bbox3)*jpgx,0))
        ymin<-max(0,round(s[j,]$bbox2,0))
        ymax<-min(jpgy,round((s[j,]$bbox2+s[j,]$bbox4),0))

        xminb<-max(0,round(s[j,]$bbox1*jpgx,0))
        xmaxb<-min(jpgx,round((s[j,]$bbox1+s[j,]$bbox3)*jpgx,0))
        yminb<-max(0,round(s[j,]$bbox2*jpgy,0))
        ymaxb<-min(jpgy,round((s[j,]$bbox2+s[j,]$bbox4)*jpgy,0))
        if(length(dim(jpg))==2)dim(jpg)<-c(dim(jpg)[1],dim(jpg)[2],1)

        line<-data.frame(image_path=mdresults[[i]]$file,md_class=s[j,]$category,md_confidence=s[j,]$conf,
                         pixelx=jpgx,pixely=jpgy,
                         x1=s[j,]$bbox1,x2=s[j,]$bbox2,y1=s[j,]$bbox3,y2=s[j,]$bbox4,
                         xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax)
        images_flat<-rbind(images_flat,line)
      }
    }else{
      line<-data.frame(image_path=mdresults[[i]]$file,md_class=0,md_confidence=mdresults[[i]]$max_detection_conf,
                       pixelx=jpgx,pixely=jpgy,
                       x1=NA,x2=NA,y1=NA,y2=NA,
                       xmin=NA,xmax=NA,ymin=NA,ymax=NA)
      images_flat<-rbind(images_flat,line)
    }
    utils::setTxtProgressBar(pb, i)
  }
  images_flat
}


#' Quickly flatten output from MegaDetector
#'
#' Returns a dataframe where each row is a MD bounding box, does not take into consideration
#' potential collisions with the outer edge of the image.
#'
#' @param mdresults list of bounding boxes for each image
#'
#' @return A dataframe with one entry for each bounding box
#' @export
#'
#' @examples
#' \dontrun{
#'  images<-read_exif(imagedir,tags=c("filename","directory","DateTimeOriginal","FileModifyDate"),
#'                    recursive = TRUE)
#'  colnames(images)[1]<-"FilePath"
#'  mdsession<-loadMDModel(mdmodel)
#'  mdres<-classifyImagesBatchMD(mdsession,images$FilePath,
#'                               resultsfile=mdresultsfile,checkpoint = 2500)
#'  mdresflat<-convertMDResultsSimple(mdres)
#' }
convertMDResultsSimple<-function(mdresults){
  f<-function(data){
    if(nrow(data$detections)>0){data.frame(file=data$file,max_detection_conf=data$max_detection_conf,
                                           max_detection_category=data$max_detection_category,data$detections,stringsAsFactors = F)
    }else{data.frame(file=data$file,max_detection_conf=data$max_detection_conf,
                     max_detection_category=data$max_detection_category,category=0,conf=NA,
                     bbox1=NA,bbox2=NA,bbox3=NA,bbox4=NA,stringsAsFactors = F)}
  }
  results<-do.call(rbind.data.frame,sapply(mdresults,f,simplify = F))
  colnames(results)[4:5]<-c("md_class","md_confidence")
  results
}
