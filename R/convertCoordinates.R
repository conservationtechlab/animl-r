#' Convert bbox from Relative to Absolute Coordinates
#'
#' Each row is a MD bounding box, there can be multiple bounding boxes per image.
#'
#' @param results list of bounding boxes for each image
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
#'                               resultsfile=resultsfile,checkpoint = 2500)
#'  mdresflat<-convertresults(mdres)
#' }
convertCoordinates <- function(results){
  
  images <- data.frame(image_path=character(),md_class=numeric(),md_confidence=numeric(),
                          pixelx=numeric(),pixely=numeric(),
                          x1=numeric(),x2=numeric(),y1=numeric(),y2=numeric(),
                          xmin=numeric(),xmax=numeric(),ymin=numeric(),ymax=numeric())

  
  for (i in 1:length(results)) {
    #load image
    jpg<-jpeg::readJPEG(results[[i]]$file)
    jpgy<-dim(jpg)[1]
    jpgx<-dim(jpg)[2]

    xmin<-max(0, round(results[[i]]$bbox1 * jpgx, 0))
    xmax<-min(jpgx,round((results[[i]]$bbox1 + results[[i]]$bbox3)*jpgx, 0))
    ymin<-max(0, round(results[[i]]$bbox2, 0))
    ymax<-min(jpgy, round((results[[i]]$bbox2 + results[[i]]$bbox4), 0))

    xminb<-max(0, round(results[[i]]$bbox1*jpgx, 0))
    xmaxb<-min(jpgx, round((results[[i]]$bbox1+results[[i]]$bbox3)*jpgx, 0))
    yminb<-max(0, round(results[[i]]$bbox2*jpgy, 0))
    ymaxb<-min(jpgy, round((results[[i]]$bbox2+results[[i]]$bbox4)*jpgy, 0))
    
    if (length(dim(jpg)) == 2) dim(jpg) <- c(dim(jpg)[1], dim(jpg)[2],1)

    line <- data.frame(image_path = results[[i]]$file, 
                       md_class = results[[i]]$category,
                       md_confidence = results[[i]]$conf,
                       pixelx = jpgx, pixely = jpgy,
                       x1 = results[[i]]$bbox1, x2 = results[[i]]$bbox2,
                       y1 = results[[i]]$bbox3, y2 = results[[i]]$bbox4,
                       xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    images<-rbind(images,line)
  }
  images
}


