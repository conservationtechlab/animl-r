#' Leverage sequences to classify images
#'
#' This function applies image classifications at a sequence level by leveraging 
#' information from multiple images. A sequence is defined as all images at the same
#' camera/station where the time between consecutive images is <=maxdiff. This can improve
#' classification accuracy, but assumes that only one species is present in each sequence.
#' If you regularly expect multiple species to occur in an image or sequence don't use this function.
#' 
#' This function retains "Empty" classification even if other images within the
#' sequence are predicted to contain animals.
#' Classification confidence is weighted by MD confidence.
#'
#' @param animals sub-selection of all images that contain MD animals
#' @param empty optional, data frame non-animal images (empty, human and vehicle) that will be merged back with animal imagages
#' @param predictions data frame of prediction probabilities from the classifySpecies function
#' @param classes a vector or species corresponding to the columns of 'predictions'
#' @param emptyclass a string indicating the class that should be considered 'Empty'
#' @param stationcolumn a column in the animals and empty data frame that indicates the camera or camera station
#' @param sort optional sort order. The default is 'stationcolumn' and DateTime.
#' @param maxdiff maximum difference between images in seconds to be included in a sequence, defaults to 60
#' @param 
#' @return data frame with predictions and confidence values for animals and empty images
#' @export
#'
#' @examples
#' \dontrun{
#' predictions <-classifyCropsSpecies(images,modelfile,resize=456)
#' animals <- allframes[allframes$max_detection_category==1,]
#' empty <- setEmpty(allframes)
#' animals <- sequenceClassification(animals,empty,predictions,classes,emptyclass = "Empty",stationcolumn="StationID",maxdiff=60)
#' }
sequenceClassification<-function(animals,empty=NULL,predictions,classes,emptyclass="",stationcolumn,sort=NULL,maxdiff=60){
  if (!is(animals, "data.frame")) {
    stop("'animals' must be Data Frame.")
  }  
  if (!is(pred, "data.frame")) {
    stop("'predictions' must be Data Frame")
  }
  if(nrow(animals)!=nrow(predictions)){
    stop("'animals' and 'predictions' must have the same number of rows")
  }
  if(!is.null(sort) & sum(sort %in% colnames(animals))!=length(sort)){
    stop("not all sort columns are present in the 'animals' data.frame")
  }
  if(!setequal(colnames(animals)[!colnames(animals) %in%c("prediction","confidence")],colnames(empty)[!colnames(empty) %in% c("prediction","confidence")])){
    stop("column names for animals and empty must be the same")
  }
  if (length(emptyclass)>1) {
    stop("'emptyclass' must be a vector of length 1")
  }
  if(!is.numeric(maxdiff) | maxdiff<0)
  {
    stop("'maxdiff' must be a number >=0")
  }
  if(length(classes)!=ncol(pred))
  {
    stop("'classes' must have the same length as the number or columns in 'predictions'")
  }
  if(is.null(stationcol) | length(stationcol)>1){
    stop("please provide a single character values for 'stationcol'")
  }

  
  #sort animals and predictions
  if(is.null(sort)){
    sort<-do.call(order,animals[,c(stationcol,"DateTime")])
  }else{
    sort<-do.call(order,animals[,sortcol])
  }

  animals<-animals[sort,]
  predsort<-predictions[sort,]
  
  #define which class is empty  
  if(emptyclass>""){
    emptycol<-which(classes == emptyclass)
  }
  
  i=1
  c=nrow(animals)/100
  
  #loop over all animals rows
  cat("Classifying animal images..\n")
  opb <- pbapply::pboptions(char = "=")
  pb <- pbapply::startpb(1, nrow(animals))
  
  conf=numeric(nrow(animals))
  predict=character(nrow(animals))
  
  while(i<=nrow(animals)){
    if(i > c){
      pbapply::setpb(pb, i) 
      c=c+nrow(animals)/100
    }
    rows<-i
    j=i+1
    
    while(!is.na(animals$DateTime[j]) & !is.na(animals$DateTime[i]) & j<nrow(animals) & animals[j,stationcol]==animals[i,stationcol] & difftime(animals$DateTime[j],animals$DateTime[i],units="secs")<=maxdiff){
      rows<-c(rows,j)
      j=j+1
    }
    #check if there are multiple crops in a sequence
    if(length(rows)>1){ #multiple boxes in the sequence
      predclass<-apply(predsort[rows,],1,which.max)
      if(length(emptycol)==0 | !(emptycol %in% predclass) | length(which(predclass %in% emptycol))==length(rows)){
        predsort2<-predsort[rows,]*animals$conf[rows]
        predbest<-colMeans(predsort2)
        conf[rows]<-max(predsort2[,which.max(predbest)])
        predict[rows]<-classes[which.max(predbest)]
      }else{ #process sequences with some empty
        #select images for which all boxes are empty
        sel<-tapply(predclass==emptycol,animals$FilePath[rows],sum)==
          tapply(predclass==emptycol,animals$FilePath[rows],length)
        
        #classify images with species
        #boxes that are animals
        sel2<-which(animals$FilePath[rows] %in% names(sel[!sel]) & !(predclass %in% emptycol))
        sel3<-which(animals$FilePath[rows] %in% names(sel[!sel]))
        predsort2<-predsort[rows[sel2],]*animals$conf[rows[sel2]]
        predbest<-colMeans(predsort2)
        conf[rows[sel3]]<-max(predsort2[,which.max(predbest)])
        predict[rows[sel3]]<-classes[which.max(predbest)]
        
        #classify empty images
        for(s in names(sel[sel])){
          sel2<-which(animals$FilePath[rows] %in% s)
          predbest<-colMeans(predsort[rows[sel2],]*animals$conf[rows[sel2]])
          conf[rows[sel2]]<-max(predbest) 
          predict[rows[sel2]]<-classes[which.max(predbest)]
        }
      }
    }else{ #only one box in the sequence
      predbest<-predsort[rows,]
      conf[rows]<-max(predbest*animals$conf[rows])
      predict[rows]<-classes[which.max(predbest)]
    }
    i=j
  }
  animals$confidence<-conf
  animals$prediction=predict
  
  pbapply::setpb(pb, nrow(animals))
  pbapply::closepb(pb)
  
  if(!is.null(empty)){
    cat("Classifying empty, human and vehicle images..\n")
    #set common name to the same for all boxes in non-animal images
    #select files with multiple boxes
    t<-tapply(1:nrow(empty),empty$FilePath,function(x)x)
    t2<-lapply(t,length)
    t3<-which(t2>1)
    
    #setup progress bar
    cmax=length(unlist(t[t3]))
    opb <- pbapply::pboptions(char = "=")
    pb <- pbapply::startpb(1, cmax)
    pbapply::setpb(pb, 1)
    
    c=1
    conf=numeric(cmax)
    predict=character(cmax)
    rowsel<-numeric(cmax)
    #loop over all files with multiple boxes
    for(s in t3){
      sel<-t[s][[1]]
      rowsel[c:(c+length(sel)-1)]<-sel
      maxconf=which.max(empty$confidence[sel])
      conf[c:(c+length(sel)-1)]<-rep(empty$confidence[sel][maxconf],length(sel))
      predict[c:(c+length(sel)-1)]<-rep(empty$prediction[sel][maxconf],length(sel))
      
      if((c %% round(cmax/1000))==0){
        pbapply::setpb(pb, c)
      }
      c=c+length(sel)
    }
    pbapply::setpb(pb, cmax)
    pbapply::closepb(pb)
    
    empty[rowsel,]$confidence<-conf
    empty[rowsel,]$prediction<-predict
    
    #combine animal and empty images
    alldata<-rbind(empty,animals)
    alldata[do.call(order,alldata[,sortcol]),]
  }else{
    animals[do.call(order,animals[,sortcol]),]
  }
  
}
