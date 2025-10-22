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
#' @param predictions_raw data frame of prediction probabilities from the classifySpecies function
#' @param classes class list associated with classifier model
#' @param station_col a column in the animals and empty data frame that indicates the camera or camera station
#' @param empty_class a string indicating the class that should be considered 'Empty'
#' @param human_class a string indicating the class that should be considered 'Human'
#' @param vehicle_class a string indicating the class that should be considered 'Vehicle'
#' @param sort_columns optional sort order. The default is 'station_column' and datetime.
#' @param file_col a field indicating a single record. The default is FilePath for single images/videos.
#' @param maxdiff maximum difference between images in seconds to be included in a sequence, defaults to 60
#'
#' @return data frame with predictions and confidence values for animals and empty images
#' @export
#'
#' @examples
#' \dontrun{
#' predictions_raw <-classify(classifier, images, resize_width=456, resize_height=456)
#' animals <- get_animals(images)
#' empty <- get_empty(images)
#' animals <- sequence_classification(animals, empty, predictions_raw, classes,
#'                                    station_column="StationID",
#'                                    empty_class = "Empty",
#'                                    sort_columns = c("StationID", "DateTime"),
#'                                    maxdiff=60)
#' }
sequence_classification<-function(animals, empty, predictions_raw, classes,
                                  station_col="station",
                                  empty_class="",
                                  human_class="",
                                  vehicle_class="",
                                  sort_columns=NULL, 
                                  file_col="filepath", 
                                  maxdiff=60){
  # typechecking
  if (!is(animals, "data.frame")) { stop("'animals' must be a Data Frame.") }  
  if (!is(predictions_raw, "matrix")) { stop("'predictions_raw' must be a matrix") }
  if(nrow(animals)!=nrow(predictions_raw)){ stop("'animals' and 'predictions_raw' must have the same number of rows")}
  if(!is.null(sort_columns) && sum(sort_columns %in% colnames(animals))!=length(sort_columns)){
    stop("not all sort columns are present in the 'animals' data.frame")
  }
  if(!is.null(empty) && (!setequal(colnames(animals)[!colnames(animals) %in%c("predictions_raw","confidence")],colnames(empty)[!colnames(empty) %in% c("prediction","confidence")]))){
    stop("column names for animals and empty must be the same")
  }
  if (length(empty_class) > 1) { stop("'empty_class' must be a vector of length 1") }
  if (length(human_class) > 1) { stop("'human_class' must be a vector of length 1") }
  if (length(vehicle_class) > 1) { stop("'vehicle_class' must be a vector of length 1") }
  if(!is.numeric(maxdiff) | maxdiff<0){ stop("'maxdiff' must be a number >=0") }
  if(length(classes)!=ncol(predictions_raw)){ stop("'classes' must have the same length as the number or columns in 'predictions_raw'") }
  if(is.null(station_col) | length(station_col)>1){ stop("please provide a single character values for 'station_col'") }
  
  #if column conf does not exist add it as 1s
  if(!("conf" %in% colnames(animals))){
    animals$conf=1
  }
  
  #define which class is empty  
  if(empty_class>""){
    empty_col<-which(classes == empty_class)
  }
  
  #define which class is human  
  if(human_class>""){
    human_col<-which(classes == human_class)
  }
  
  #define which class is vehicle  
  if(vehicle_class>""){
    vehicle_col<-which(classes == vehicle_class)
  }
  
  nclasses<-length(classes)
  
  if(!is.null(empty)){
    empty$ID<-1:nrow(empty)
    
    
    #create extended prediction matrix for empty, vehicles and human
    predempty <- stats::reshape(empty[,c("ID","prediction","confidence")],direction="wide",idvar="ID",timevar="prediction")
    predempty[is.na(predempty)] <- 0
    predempty <- cbind(matrix(0, nrow=nrow(empty), ncol=dim(predictions_raw)[2]), predempty[,-1, drop=FALSE])
    
    classes<-c(classes,unique(empty$prediction))
    
    #update empty column if present in the classifier
    if(empty_class > ""){
      predempty[,empty_col] <- predempty$confidence.empty
      predempty<-predempty[,names(predempty)!="confidence.empty"]
      classes <- classes[!(1:length(classes) %in% (which(classes[(nclasses+1):length(classes)]=="empty")+nclasses))]
    }
    
    #update human column if present in the classifier
    if(human_class > ""){
      predempty[,human_col] <- predempty$confidence.human
      predempty<-predempty[,names(predempty)!="confidence.human"]
      classes <- classes[!(1:length(classes) %in% (which(classes[(nclasses+1):length(classes)]=="human")+nclasses))]
    }
    
    #update vehicle column if present in the classifier
    if(vehicle_class > ""){
      predempty[,vehicle_col] <- predempty$confidence.vehicle
      predempty<-predempty[,names(predempty)!="confidence.vehicle"]
      classes <- classes[!(1:length(classes) %in% (which(classes[(nclasses+1):length(classes)]=="vehicle")+nclasses))]
    }
    
    #set columns if the are not in classes
    if(empty_class==""){
      empty_col<-which(names(predempty)=="confidence.empty")
    }
    if(human_class==""){
      human_col<-which(names(predempty)=="confidence.human")
    }
    if(vehicle_class==""){
      vehicle_col<-which(names(predempty)=="confidence.vehicle")
    }
    
    animals$prediction <- classes[apply(predictions_raw, 1, which.max)]
    animals$confidence <- apply(predictions_raw, 1, max) * animals$conf
    empty$conf<-1
    animals<-rbind(animals,empty[,-ncol(empty)]) # dont add ID column
    predictions_raw<-rbind(cbind(predictions_raw,matrix(0,nrow(predictions_raw),ncol(predempty)-ncol(predictions_raw))),as.matrix(predempty))
  }
  
  #sort animals and predictions
  if(is.null(sort_columns)){
    sort_columns<-c(station_col,"datetime")
  }
  sort<-do.call(order,animals[,sort_columns])
  
  animals_sort <- animals[sort,,drop=FALSE]
  predsort <- predictions_raw[sort,,drop=FALSE]
  
  
  i=1
  c=nrow(animals_sort)/100
  
  #loop over all animals rows
  cat("Classifying animal images..\n")
  opb <- pbapply::pboptions(char = "=")
  pb <- pbapply::startpb(1, nrow(animals_sort))
  
  conf_placeholder = numeric(nrow(animals_sort))
  predict_placeholder = character(nrow(animals_sort))
  
  while(i<=nrow(animals_sort)){
    if(i > c){
      pbapply::setpb(pb, i) 
      c=c+nrow(animals_sort)/100
    }
    
    #rows pertaining to a sequence
    rows <- i
    
    #last row in current sequence
    last_index = i+1
    
    # while within same sequence
    while(!is.na(animals_sort$datetime[last_index]) & !is.na(animals_sort$datetime[i]) & 
          last_index<nrow(animals_sort) & animals_sort[last_index,station_col]==animals_sort[i,station_col] & 
          difftime(animals_sort$datetime[last_index], animals_sort$datetime[i],units="secs") <= maxdiff){
      rows<-c(rows,last_index)
      last_index=last_index+1
    }
    
    
    #check if there are multiple boxes in a sequence
    if(length(rows)>1){ #multiple boxes in the sequence
      predclass<-apply(predsort[rows,],1,which.max)
      #check if there are empty predictions
      if(length(empty_col)==0 || !(empty_col %in% predclass) || length(which(predclass %in% empty_col))==length(rows)){
        #no empties
        predsort_confidence <- predsort[rows,]*animals_sort$conf[rows]
        predbest <- apply(predsort_confidence, 2, mean)
        conf_placeholder[rows]<-max(predsort_confidence[,which.max(predbest)])
        predict_placeholder[rows]<-classes[which.max(predbest)]
      }
      
      #process sequences with some empty
      else{ 
        #select images for which all boxes or frames are empty
        sel_all_empty<-tapply(predclass==empty_col,animals_sort[rows,file_col],sum) ==
          tapply(predclass==empty_col,animals_sort[rows,file_col],length)
        #classify files with species
        #records with animals and no empties
        sel_no_empties<-which(animals_sort[rows,file_col] %in% names(sel_all_empty[!sel_all_empty]) & !(predclass %in% empty_col))
        #records in files with animals
        sel_mixed<-which(animals_sort[rows,file_col] %in% names(sel_all_empty[!sel_all_empty]))
        
        
        if(length(sel_no_empties)>0 & length(sel_mixed)>0){
          predsort_confidence<-matrix(predsort[rows[sel_no_empties],]*animals_sort$conf[rows[sel_no_empties]],ncol=ncol(predsort))
          predbest<-apply(predsort_confidence,2,mean)
          conf_placeholder[rows[sel_mixed]]<-max(predsort_confidence[,which.max(predbest)])
          predict_placeholder[rows[sel_mixed]]<-classes[which.max(predbest)]
        }
        #classify empty images
        for(s in names(sel_all_empty[sel_all_empty])){
          row_index<-which(animals_sort[rows,file_col] %in% s)
          predsort_confidence <- matrix(predsort[rows[row_index],]*animals_sort$conf[rows[row_index]],ncol=ncol(predsort))
          predbest<-apply(predsort_confidence,2,mean) 
          conf_placeholder[rows[row_index]]<-max(predbest) 
          predict_placeholder[rows[row_index]]<-classes[which.max(predbest)]
        }
      }
    }
    #only one box in the sequence
    else{ 
      predbest<-predsort[rows,,drop=FALSE]
      conf_placeholder[rows]<-max(predbest*animals_sort$conf[rows])
      predict_placeholder[rows]<-classes[which.max(predbest)]
    }
    # move to next sequence
    i=last_index
  }
  
  animals_sort$confidence <- conf_placeholder
  animals_sort$prediction <- predict_placeholder
  
  pbapply::setpb(pb, nrow(animals_sort))
  pbapply::closepb(pb)
  
  animals_sort[do.call(order,animals_sort[,sort_columns]),]
}
