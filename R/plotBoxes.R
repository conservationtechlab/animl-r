#' Plot bounding boxes on image from md results
#'
#' @param image The mdres for the image
#' @param label T/F toggle to plot MD category
#' @param minconf minimum confidence to plot box
#'
#' @return no return value, produces bounding box in plot panel
#' @export
#'
#' @examples
#' \dontrun{
#' mdres<-classifyImageMD(mdsession,images$FilePath[30000])
#' plotBoxes(mdres,minconf = 0.5)
#' }
plotBoxes<-function(image,label=FALSE,minconf=0){
  jpg<-jpeg::readJPEG(image$FilePath)
  plot(grDevices::as.raster(jpg)) ## where is this from??
  jpgy<-dim(jpg)[1]
  jpgx<-dim(jpg)[2]

  #get bounding boxes
  if(is.data.frame(image$detections)){
    s<-image$detections
    if(nrow(s)>0){
      #plot bounding boxes
      col=c("green","red","blue","orange")
      for(j in 1:nrow(s)){
        if(s[j,]$conf>=minconf){
          graphics::rect(s[j,]$bbox1*jpgx,jpgy-s[j,]$bbox2*jpgy,(s[j,]$bbox1+s[j,]$bbox3)*jpgx,jpgy-(s[j,]$bbox2+s[j,]$bbox4)*jpgy,border=col[as.numeric(s[j,]$category)],lwd = 2)
          if(label){

            graphics::text(x=s[j,]$bbox1*jpgx,y=jpgy-(s[j,]$bbox2+s[j,]$bbox4),lables = s[j,]$category,)
          }

        }

      }
    }
  }
}


#' Plot bounding boxes from MD flat data frame
#'
#' @param image The mdres for the image
#' @param label Toggle to plot MD category, defaults to FALSE
#' @param minconf minimum confidence to plot box
#'
#' @return no return value, produces bounding box in plot panel
#' @export
#'
#' @examples
#' \dontrun{
#' mdres<-classifyImageMD(mdsession,images$FilePath[30000])
#' mdresflat<-flattenBoxesMDSimple(mdres)
#' plotBoxesFromFlat(mdresflat,minconf = 0.5)
#' }
plotBoxesFromFlat<-function(image,label=FALSE,minconf=0){
  if(is.data.frame(image) & nrow(image)>0){
    if(length(unique(image$FilePath))>1)stop("Provide data for a single image file.\n")
    #load image
    image$FilePath[1]
    jpg<-jpeg::readJPEG(image$FilePath[1])
    plot(grDevices::as.raster(jpg))
    jpgy<-dim(jpg)[1]
    jpgx<-dim(jpg)[2]

    #get bounding boxes
    if(nrow(image)>0){
      #plot bounding boxes
      col=c("green","red","blue","orange")
      for(j in 1:nrow(image)){
        if(image[j,]$md_confidence>=minconf & !is.na(image[j,]$bbox1)){
          graphics::rect(image[j,]$bbox1*jpgx,jpgy-image[j,]$bbox2*jpgy,(image[j,]$bbox1+image[j,]$bbox3)*jpgx,jpgy-(image[j,]$bbox2+image[j,]$bbox4)*jpgy,border=col[as.numeric(image[j,]$md_class)],lwd = 2)}
      }
    }
  }
}
