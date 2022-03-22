#' Title
#'
#' @param x
#' @param boxes
#' @param resize
#' @param buffer
#' @param batch_size
#' @param standardize
#'
#' @return
#' @export
#'
#' @examples
#'    crop_generator <- function(x, boxes, resize=299,buffer=2,batch_size=32,standardize=TRUE){
#' #'    num_samples = nrow(x)
#'    #offset=0
#'    while(TRUE){
#'      for(offset in 1:num_samples, batch_size){
#'        # Initialise X_train and y_train arrays for this batch
#'        X_eval = []
#'
#'        for(i in offset:min(num_samples,offset+batch_size)){
#'          # Load image (X) and label (y)
#'          jpg<-jpeg::readJPEG(image$image_path)
#'          jpgy<-dim(jpg)[1]
#'          jpgx<-dim(jpg)[2]
#'
#'          #get bounding boxes
#'          #get bounding boxes
#'          xmin<-max(0,round(image$x1*jpgx,0))
#'          xmax<-min(jpgx,round(image$x1*jpgx+max(1,image$y1*jpgx),0))
#'          ymin<-max(0,round(image$x2*jpgy,0))
#'          ymax<-min(jpgy,round(image$x2*jpgy+max(1,image$y2*jpgy),0))
#'          buffer2=max(xmax-xmin,ymax-ymin)*buffer
#'
#'          xminb<-max(0,xmin-buffer2)
#'          xmaxb<-min(jpgx,xmax+buffer2)
#'          yminb<-max(0,ymin-buffer2)
#'          ymaxb<-min(jpgy,ymax+buffer2)
#'          if(length(dim(jpg))==2)dim(jpg)<-c(dim(jpg)[1],dim(jpg)[2],1)
#'
#'
#'          crop <- jpg[yminb:ymaxb,xminb:xmaxb,]
#'          img <- resize_pad(img, resize)
#'
#'          # Add example to arrays
#'          if(standardize){X_eval.append(np.array(img)/255)}
#'          else{X_eval.append(img)}
#'
#'          # Make sure they're numpy arrays (as opposed to lists)
#'          X_eval = np.array(X_eval)
#'
#'          # The generator-y part: yield the next training batch
#'          yield(X_eval)
#'          #offset+=batch_size
#'        }
#'      }
#' #'    }
#'    }

