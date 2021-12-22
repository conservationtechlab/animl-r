#' Leverage sequences to classify images
#'
#' Images must be within maxdiff seconds of each other to be considered in sequence.
#' This function retains "Empty" classification even if other images within the
#' sequence are predicted to contain animals.
#' Classification confidence is weighted by MD confidence.
#'
#' @param imagesallanimal subselection of all images that contain MD animals
#' @param mlpredictions classifier predictions
#' @param classes list of all possible classes
#' @param emptycol integer that represents the empty class
#' @param maxdiff maximum difference between images in seconds to be included in a sequence, defaults to 60
#'
#' @return reclassified imagesallanimal dataframe
#' @export
#'
#' @examples
#'
#' classifySequence(imagesallanimal,results,classes,17)
#'
classifySequence <- function(imagesallanimal,mlpredictions,classes,emptycol,maxdiff=60){

  #sort by SurveyID, StationID, DateTime
  sort<-with(imagesallanimal,order(Station,DateTime))
  imagesallanimal<-imagesallanimal[sort,]
  predsort<-mlpredictions[sort,]


  #loop through all images
  i=1
  system.time(while(i<=nrow(imagesallanimal)){
    if(i %% 10000<3)cat(i,"/",nrow(imagesallanimal)," ",format(Sys.time(), "%H:%M:%S"),"\n")
    sequence <- i
    j=i+1

    #find all images within a sequence, image creation times must be within maxdiff seconds to be considered in sequence
    while(!is.na(imagesallanimal$DateTime[j]) & !is.na(imagesallanimal$DateTime[i]) & j<nrow(imagesallanimal) & imagesallanimal$Camera[j]==imagesallanimal$Camera[i] & difftime(imagesallanimal$DateTime[j],imagesallanimal$DateTime[i],units="secs")<=maxdif){
      sequence <- c(sequence,j)
      j=j+1
    }

    #check if there are multiple crops in a sequence
    if(length(sequence)>1){

      predclass<-apply(predsort[sequence,],1,which.max)

      #no images are empty or there is no empty class
      if(length(emptycol)==0 | !(emptycol %in% predclass) | length(which(predclass %in% emptycol))==length(sequence)){
        predsort2<-predsort[sequence,]*imagesallanimal$md_confidence[sequence]
        predbest<-apply(predsort2,2,mean)
        imagesallanimal$prediction[sequence]<-which.max(predbest)
        imagesallanimal$confidence[sequence]<-max(predsort2[,which.max(predbest)])
        imagesallanimal$Common[sequence]<-classes[imagesallanimal$prediction[sequence]]
      }
      # there are some images within the sequence that are empty
      else{
        #are all boxes empty?
        sel<-tapply(predclass==emptycol,imagesallanimal$FilePathOrig[sequence],sum)==
          tapply(predclass==emptycol,imagesallanimal$FilePathOrig[sequence],length)

        #classify images with species
        sel2<-which(imagesallanimal$FilePathOrig[sequence] %in% names(sel[!sel]) & !(predclass %in% emptycol))
        sel3<-which(imagesallanimal$FilePathOrig[sequence] %in% names(sel[!sel]))

        #choose the best class weighted by the most confident MD prediction
        predsort2<-matrix(predsort[sequence[sel2],]*imagesallanimal$md_confidence[sequence[sel2]],ncol=ncol(predsort))
        predbest<-apply(predsort2,2,mean)
        imagesallanimal$prediction[sequence[sel3]]<-which.max(predbest)
        imagesallanimal$confidence[sequence[sel3]]<-max(predsort2[,which.max(predbest)])
        imagesallanimal$Common[sequence[sel3]]<-classes[imagesallanimal$prediction[sequence[sel3]]]

        #classify empty images
        for(s in names(sel[sel])){
          sel2<-which(imagesallanimal$FilePathOrig[sequence] %in% s)
          predbest<-apply(matrix(predsort[sequence[sel2],]*imagesallanimal$md_confidence[sequence[sel2]],ncol=ncol(predsort)),2,mean)
          imagesallanimal$prediction[sequence[sel2]]<-which.max(predbest)
          imagesallanimal$confidence[sequence[sel2]]<-max(predbest)
          imagesallanimal$Common[sequence[sel2]]<-classes[imagesallanimal$prediction[sequence[sel2]]]
        }
      }
    }
    #only one image in the sequence
    else{
      predbest<-predsort[sequence,]
      imagesallanimal$prediction[sequence]<-which.max(predbest)
      imagesallanimal$confidence[sequence]<-max(predbest*imagesallanimal$md_confidence[sequence])
      imagesallanimal$Common[sequence]<-classes[imagesallanimal$prediction[sequence]]
    }
    i=j
  })

  imagesallanimal
}
