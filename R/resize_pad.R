#' Resize an image with padding
#'
#' @param img  the image, read by jpeg library
#' @param size new size
#'
#' @return returns resized jpeg image
#' @export
#'
#' @examples
#' \dontrun{
#' crop<-resize_pad(cropped_image_path,256)
#' }
resize_pad<-function(img,size=256){
  if(dim(img)[0] == 0 || dim(img)[1] == 0){return(img)}
  imgpad<-array(0,c(max(dim(img)),max(dim(img)),3))
  xstart<-max(1,floor((dim(imgpad)[2]-dim(img)[2])/2))
  ystart<-max(1,floor((dim(imgpad)[1]-dim(img)[1])/2))
  imgpad[ystart:(ystart+dim(img)[1]-1),xstart:(xstart+dim(img)[2]-1),]<-img
  imgres<-imager::resize(imager::as.cimg(imgpad),size_x=size,size_y=size,interpolation_type = 3)
  imgres[,,1,]
}
