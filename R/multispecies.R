#' Classify images with multiple potential species
#'
#' @param imagesallanimal dataframe of MD animal images
#' @param mlpredictions classifier predictions for each box
#' @param classes list of all possible classes
#' @param emptycol integer value of the empty column
#'
#' @return list of images with multiple species, includes number of animals for each class
#' @export
#'
#' @examples
#' \dontrun{
#' imagesallanimal <- multispecies(imagesallanimal,mlpredictions,classes,17)
#' }
multispecies <- function(imagesallanimal,mlpredictions,classes,emptycol){

  multianimal <- imagesallanimal[0,]

  i=1
  system.time(while(i<=nrow(imagesallanimal)){
    if(i %% 10000<3)cat(i,"/",nrow(imagesallanimal)," ",format(Sys.time(), "%H:%M:%S"),"\n")
    sequence <- i
    j=i+1

    #find all images within a sequence, image creation times must be within maxdiff seconds to be considered in sequence
    while(imagesallanimal$FileName[j]==imagesallanimal$FileName[i]){
      sequence <- c(sequence,j)
      j=j+1
    }

    #check if there are multiple crops in an image
    if(length(sequence)>1){

      #predclass<-apply(mlpredictions[sequence,],1,which.max)

      pred_weightby_MD<-mlpredictions[sequence,]*imagesallanimal$md_confidence[sequence]
      predbest<-apply(pred_weightby_MD,2,mean)
      imagesallanimal$prediction[sequence]<-which.max(predbest)
      imagesallanimal$confidence[sequence]<-max(pred_weightby_MD[,which.max(predbest)])
      imagesallanimal$Common[sequence]<-classes[imagesallanimal$prediction[sequence]]
    }

    #only one crop in the image
    else{
      predbest<-mlpredictions[sequence,]
      imagesallanimal$prediction[sequence]<-which.max(predbest)
      imagesallanimal$confidence[sequence]<-max(predbest*imagesallanimal$md_confidence[sequence])
      imagesallanimal$Common[sequence]<-classes[imagesallanimal$prediction[sequence]]
    }
    i=j
  })

  multianimal
}
