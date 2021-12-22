#plot boxes from md results
plotBoxes<-function(image,minconf=0){
  #load image
  image$file
  jpg<-readJPEG(image$file)
  plot(as.raster(jpg))
  jpgy<-dim(jpg)[1]
  jpgx<-dim(jpg)[2]

  #get bounding boxes
  if(class(image$detections)=="data.frame"){
    s<-image$detections
    if(nrow(s)>0){
      #plot bounding boxes
      col=c("green","red","blue","orange")
      for(j in 1:nrow(s)){
        if(s[j,]$conf>=minconf){
          rect(s[j,]$bbox1*jpgx,jpgy-s[j,]$bbox2*jpgy,(s[j,]$bbox1+s[j,]$bbox3)*jpgx,jpgy-(s[j,]$bbox2+s[j,]$bbox4)*jpgy,border=col[as.numeric(s[j,]$category)],lwd = 2)}
      }
    }
  }
}

######################
#plot bounding boxes from MD flat data frame
plotBoxesFromFlat<-function(image,minconf=0){
  if(class(image)=="data.frame" & nrow(image)>0){
    if(length(unique(image$FilePath))>1)stop("Provide data for a single image file.\n")
    #load image
    image$FilePath[1]
    jpg<-readJPEG(image$FilePath[1])
    plot(as.raster(jpg))
    jpgy<-dim(jpg)[1]
    jpgx<-dim(jpg)[2]

    #get bounding boxes
    if(nrow(image)>0){
      #plot bounding boxes
      col=c("green","red","blue","orange")
      for(j in 1:nrow(image)){
        if(image[j,]$md_confidence>=minconf & !is.na(image[j,]$bbox1)){
          rect(image[j,]$bbox1*jpgx,jpgy-image[j,]$bbox2*jpgy,(image[j,]$bbox1+image[j,]$bbox3)*jpgx,jpgy-(image[j,]$bbox2+image[j,]$bbox4)*jpgy,border=col[as.numeric(image[j,]$md_class)],lwd = 2)}
      }
    }
  }
}
