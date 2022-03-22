#' Extract bounding boxes for a single image and save as new images
#'
#' Requires the unflattened raw MD output
#'
#' @param image single image, raw MD output
#' @param min_conf Confidence threshold (defaults to 0, not in use)
#' @param buffer Adds a buffer to the MD bounding box, defaults to 2px
#' @param plot Toggle to plot each crop in the plot window, defaults to TRUE
#' @param return.crops Toggle to return list of cropped images, defaults to FALSE
#' @param save Toggle to save output cropped, defaults to FALSE
#' @param resize Size in pixels to resize cropped images, NA if images are not resized, defaults to NA
#' @param outdir Directory in which output cropped images will be saved
#' @param quality Compression level of output cropped image, defaults to 0.8
#'
#' @return a flattened dataframe containing crop information
#' @export
#'
#' @examples
#' \dontrun{
#' images<-read_exif(imagedir,tags=c("filename","directory"), recursive = TRUE)
#' crops <- extractBoxes(images[1,],plot=TRUE,return.crops=TRUE,save=TRUE)
#' }
extractBoxes<-function(image,min_conf=0,buffer=2,plot=TRUE,return.crops=FALSE,save=FALSE,resize=NA,outdir="",quality=0.8){
  if(save & !dir.exists(outdir))stop("Output directory invalid.\n")
  images_flat<-data.frame(image_path=character(),md_class=numeric(),md_confidence=numeric(),pixelx=numeric(),pixely=numeric(),
                          x1=numeric(),x2=numeric(),y1=numeric(),y2=numeric(),
                          xmin=numeric(),xmax=numeric(),ymin=numeric(),ymax=numeric(),crop_path=character(),stringsAsFactors = FALSE)

  #load image
  jpg<-jpeg::readJPEG(image$file)
  jpgy<-dim(jpg)[1]
  jpgx<-dim(jpg)[2]

  #get bounding boxes
  #get bounding boxes
  if(is.data.frame(image$detections)){
    s<-image$detections

    #extract bounding box
    if(return.crops)crops<-list()
    c=1
    for(j in 1:nrow(s)){
      xmin<-max(0,round(s[j,]$bbox1*jpgx,0))
      xmax<-min(jpgx,round(s[j,]$bbox1*jpgx+max(1,s[j,]$bbox3*jpgx),0))
      ymin<-max(0,round(s[j,]$bbox2*jpgy,0))
      ymax<-min(jpgy,round(s[j,]$bbox2*jpgy+max(1,s[j,]$bbox4*jpgy),0))
      buffer2=max(xmax-xmin,ymax-ymin)*buffer

      xminb<-max(0,xmin-buffer2)
      xmaxb<-min(jpgx,xmax+buffer2)
      yminb<-max(0,ymin-buffer2)
      ymaxb<-min(jpgy,ymax+buffer2)
      if(length(dim(jpg))==2)dim(jpg)<-c(dim(jpg)[1],dim(jpg)[2],1)
      crop<-jpg[yminb:ymaxb,xminb:xmaxb,]

      #resize and pad if requested
      if(!is.na(resize))crop<-resize_pad(crop,resize)

      #if we return crops save crop in list
      if(return.crops)crops[[c]]<-crop

      #plot cropped image if requested
      if(plot)plot(grDevices::as.raster(crop)) ## not sure where raster comes in

      #save image if requested
      imgname<-""
      if(save){
        imgname<-paste0(outdir,image$rel_path)
        if(nrow(s)>1){
          imgbase<-strsplit(basename(imgname),"[.]")[[1]][1]
          imgext<-strsplit(basename(imgname),"[.]")[[1]][2]
          imgname<-paste0(dirname(imgname),"/",imgbase,"_c",j,".",imgext)
        }
        if(!dir.exists(dirname(imgname)))dir.create(dirname(imgname),recursive=TRUE)
        jpeg::writeJPEG(crop,imgname,quality=quality)
      }
      line<-data.frame(image_path=image$file,md_class=as.numeric(s[j,]$category),md_confidence=s[j,]$conf,pixelx=jpgx,pixely=jpgy,
                       x1=s[j,]$bbox1,x2=s[j,]$bbox2,y1=s[j,]$bbox3,y2=s[j,]$bbox4,
                       xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,crop_path=imgname,stringsAsFactors = FALSE)
      images_flat<-rbind(images_flat,line)
    }
  }else{
    line<-data.frame(image_path=image$file,md_class=0,md_confidence=image$max_detection_conf,pixelx=jpgx,pixely=jpgy,
                     x1=NA,x2=NA,y1=NA,y2=NA,
                     xmin=NA,xmax=NA,ymin=NA,ymax=NA,crop_path="",stringsAsFactors = FALSE)
    images_flat<-rbind(images_flat,line)
  }
  if(return.crops)list(crops=crops,data=images_flat)
  else{images_flat}
}


#'  Extract bounding boxes and save as new image from a batch of images
#'
#' @param images list of images, raw MD output
#' @param min_conf Confidence threshold (defaults to 0, not in use)
#' @param buffer Adds a buffer to the MD bounding box, defaults to 2px
#' @param save Toggle to save output cropped, defaults to FALSE
#' @param resize Size in pixels to resize cropped images, NA if images are not resized, defaults to NA
#' @param outdir Directory in which output cropped images will be saved
#' @param quality Compression level of output cropped image, defaults to 0.8
#' @param parallel Toggle to enable parallel processing, defaults to FALSE
#' @param nproc Number of workers if parallel = TRUE, defaults to output of detectCores()
#'
#' @return a flattened dataframe containing crop information
#' @export
#'
#' @examples
#' \dontrun{
#' images<-read_exif(imagedir,tags=c("filename","directory"), recursive = TRUE)
#' crops <- extractAllBoxes(images,save=TRUE,out)
#' }
extractAllBoxes<-function(images,min_conf=0,buffer=2,save=FALSE,resize=NA,outdir="",quality=0.8,parallel=FALSE,nproc=parallel::detectCores()){
  if(outdir!="" & !dir.exists(outdir)){
    if(!dir.create(outdir,recursive = TRUE))
      stop("Output directory invalid.\n")}

  #define processing function
  run.parallel<-function(i){if(file.exists(images[[i]]$file)){extractBoxes(images[[i]],min_conf=min_conf,buffer=buffer,resize=resize,save=save,outdir=outdir,plot=F,quality=quality)}else{NA}}
  opb<-pbapply::pboptions(char = "=")
  if(parallel){
    type="PSOCK"

    cl <- parallel::makeCluster(min(parallel::detectCores(),nproc),type=type)
    #clusterExport(cl,expList <- as.list(objects(pos = globalenv())))
    parallel::clusterExport(cl,list("buffer","resize","quality","outdir","images","extractBoxes","resize_pad"),
                  envir=environment())
    #set random number generator for cluster
    parallel::clusterSetRNGStream(cl)

    results<-pbapply::pblapply(1:length(images),function(x){run.parallel(x)},cl=cl)
    parallel::stopCluster(cl)

  }else{
    results<-pbapply::pblapply(1:length(images),function(x){run.parallel(x)})
  }
  results<-do.call(rbind,results)
  results
}


#' Extract crops from a single image represented by a processed dataframe
#'
#' @param image dataframe containing MD output (assumes single row)
#' @param min_conf Confidence threshold (defaults to 0, not in use)
#' @param buffer Adds a buffer to the MD bounding box, defaults to 2px
#' @param plot Toggle to plot each crop in the plot window, defaults to TRUE
#' @param save Toggle to save output cropped, defaults to FALSE
#' @param resize Size in pixels to resize cropped images, NA if images are not resized, defaults to NA
#' @param outdir Directory in which output cropped images will be saved
#' @param quality Compression level of output cropped image, defaults to 0.8
#'
#' @return A dataframe containing image and crop paths
#' @export
#'
#' @examples
#' \dontrun{
#' crops <- extractBoxesFromFlat(mdresflat[1,],save=TRUE,out)
#' }
extractBoxesFromFlat<-function(image,min_conf=0,buffer=0,plot=TRUE,save=FALSE,resize=NA,outdir="",quality=0.8){
  if(save & !dir.exists(outdir))stop("Output directory invalid.\n")
  #load image
  jpg<-jpeg::readJPEG(image$image_path)
  jpgy<-dim(jpg)[1]
  jpgx<-dim(jpg)[2]

  #get bounding boxes
  #get bounding boxes
  xmin<-max(0,round(image$x1*jpgx,0))
  xmax<-min(jpgx,round(image$x1*jpgx+max(1,image$y1*jpgx),0))
  ymin<-max(0,round(image$x2*jpgy,0))
  ymax<-min(jpgy,round(image$x2*jpgy+max(1,image$y2*jpgy),0))
  buffer2=max(xmax-xmin,ymax-ymin)*buffer

  xminb<-max(0,xmin-buffer2)
  xmaxb<-min(jpgx,xmax+buffer2)
  yminb<-max(0,ymin-buffer2)
  ymaxb<-min(jpgy,ymax+buffer2)
  if(length(dim(jpg))==2)dim(jpg)<-c(dim(jpg)[1],dim(jpg)[2],1)
  crop<-jpg[yminb:ymaxb,xminb:xmaxb,]

  #resize and pad if requested
  if(!is.na(resize))crop<-resize_pad(crop,resize)


  #plot cropped image if requested
  if(plot)plot(grDevices::as.raster(crop))

  #save image if requested
  if(save){
    imgname<-paste0(outdir,image$crop_rel_path)
    if(!dir.exists(dirname(imgname)))dir.create(dirname(imgname),recursive=T)
    jpeg::writeJPEG(crop,imgname,quality=quality)
  }
}


#' Extract ,cropped images from a processed dataframe
#'
#' @param images dataframe containing MD output (assumes single row)
#' @param buffer Adds a buffer to the MD bounding box, defaults to 2px
#' @param save Toggle to save output cropped, defaults to FALSE
#' @param resize Size in pixels to resize cropped images, NA if images are not resized, defaults to NA
#' @param outdir Directory in which output cropped images will be saved
#' @param quality Compression level of output cropped image, defaults to 0.8
#' @param overwrite Toggle to overwrite output cropped images if they already exis, defaults to TRUE
#' @param parallel Toggle to enable parallel processing, defaults to FALSE
#' @param nproc Number of workers if parallel = TRUE, defaults to output of detectCores()
#'
#' @return A dataframe containing image and crop paths
#' @export
#'
#' @examples
#' \dontrun{
#' crops <- extractBoxesFromFlat(mdresflat,save=TRUE,out)
#' }
extractAllBoxesFromFlat<-function(images,buffer=0,resize=NA,quality=0.8,outdir="",save=FALSE,overwrite=TRUE,parallel=FALSE,nproc=parallel::detectCores()){
  if(outdir!="" & !dir.exists(outdir)){
    if(!dir.create(outdir,recursive = T))
      stop("Output directory invalid.\n")}

  #define processing function
  if(overwrite==T){
    run.parallel<-function(i){if(file.exists(images[i,]$image_path)){extractBoxesFromFlat(images[i,],buffer=buffer,resize=resize,save=save,outdir=outdir,plot=F,quality=quality)}else{NA}}
  }else{
    run.parallel<-function(i){if(file.exists(images[i,]$image_path) & !file.exists(paste0(outdir,images[i,]$crop_rel_path))){extractBoxesFromFlat(images[i,],buffer=buffer,resize=resize,save=save,outdir=outdir,plot=F,quality=quality)}else{NA}}
  }
  opb<-pbapply::pboptions(char = "=")
  if(parallel){
    type="PSOCK"

    cl <- parallel::makeCluster(min(parallel::detectCores(),nproc),type=type)
    #clusterExport(cl,expList <- as.list(objects(pos = globalenv())))
    parallel::clusterExport(cl,list("buffer","resize","quality","outdir","images","extractBoxesFromFlat","resize_pad"),
                  envir=environment())
    #set random number generator for cluster
    parallel::clusterSetRNGStream(cl)

    results<-pbapply::pblapply(1:nrow(images),function(x){run.parallel(x)},cl=cl)
    parallel::stopCluster(cl)

  }else{
    results<-pbapply::pblapply(1:nrow(images),function(x){run.parallel(x)})
  }
  results<-do.call(rbind,results)
  results
}
