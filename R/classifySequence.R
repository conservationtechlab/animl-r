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
#' \dontrun{
#' predictions <-classifyCropsSpecies(images,modelfile,resize=456)
#' imagesallanimal <- classifySequences(images,predictions,classes,17,maxdiff=60)
#' }
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
    while(!is.na(imagesallanimal$DateTime[j]) & !is.na(imagesallanimal$DateTime[i]) & j<nrow(imagesallanimal) & imagesallanimal$Camera[j]==imagesallanimal$Camera[i] & difftime(imagesallanimal$DateTime[j],imagesallanimal$DateTime[i],units="secs")<=maxdiff){
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



#' Select Best Classification From Multiple Frames
#'
#' @param mdanimals dataframe of all frames including species classification
#' @param how method for selecting best prediction, defaults to most frequent
#'
#' @return dataframe with new prediction in "Common" column
#' @export
#'
#' @examples
#' \dontrun{
#' mdanimals <- classifyVideo(mdanimals)
#' }
classifyVideo <- function(mdanimals,how="count"){

  sort<-with(mdanimals,order(Site,Camera,NewName))
  mdanimals<-mdanimals[sort,]

  videonames <- unique(mdanimals$NewName)

  steps<-length(videonames)
  pb <-pbapply::startpb(1,steps)

  for(i in 1:steps){
    v <- videonames[i]
    sequence = mdanimals[mdanimals$NewName == v,]
    guesses <- sequence %>% dplyr::group_by(sequence$prediction) %>%
      dplyr::summarise(mean = mean(sequence$confidence), n = dplyr::n())

    if(how == "conf"){ best <- guesses[which.max(guesses$mean),]}
    else{
      best <- which.max(guesses$n)
      guess <- guesses[best,]
      if(guess$prediction=="empty" && nrow(guesses) > 1){
        newguesses <- guesses[-best,]
        guess <- newguesses[which.max(newguesses$mean),]
      }
    }
    mdanimals[mdanimals$NewName == v,]$Common = guess$prediction
    pbapply::setpb(pb,i)
  }
  pbapply::setpb(pb,steps)
  pbapply::closepb(pb)
  mdanimals
}

