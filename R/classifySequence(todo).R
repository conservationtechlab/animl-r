# NEEDS TO BE UPDATED FOR CURRENT MD/CLASSIFIER OUTPUTS


#' Leverage sequences to classify images
#'
#' Images must be within maxdiff seconds of each other to be considered in sequence.
#' This function retains "Empty" classification even if other images within the
#' sequence are predicted to contain animals.
#' Classification confidence is weighted by MD confidence.
#'
#' @param animals subselection of all images that contain MD animals
#' @param mlpredictions classifier predictions
#' @param classes list of all possible classes
#' @param outfile file to which results will be saved
#' @param maxdiff maximum difference between images in seconds to be included in a sequence, defaults to 60
#'
#' @return reclassified animals dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' predictions <-classifyCropsSpecies(images,modelfile,resize=456)
#' animals <- classifySequences(images,predictions,classes,17,maxdiff=60)
#' }
classifySequence <- function(animals, mlpredictions, classfile, outfile = NA, maxdiff=60){
  if (!is.na(outfile) && file.exists(outfile)) {
    date <- exifr::read_exif(outfile, tags = "FileModifyDate")[[2]]
    date <- strsplit(date, split = " ")[[1]][1]
    if (tolower(readline(prompt = sprintf("Output file already exists and was last modified %s, would you like to load it? y/n: ", date)) == "y")) {
      return(loadData(outfile))
    }
  }
  if(!file.exists(imagedir)){stop("Error: the given class file does not exist")}
  classes<-read.table(classfile,stringsAsFactors = F)$x
  emptycol<-which(classes %in% c("empty","Empty","EMTPY","blank","Blank","BLANK"))

  #sort by SurveyID, StationID, DateTime
  sort<-with(animals,order(Station,DateTime))
  animals<-animals[sort,]
  predsort<-mlpredictions[sort,]



  #loop through all images
  i=1
  system.time(while(i<=nrow(animals)){
    if(i %% 10000<3)cat(i,"/",nrow(animals)," ",format(Sys.time(), "%H:%M:%S"),"\n")
    sequence <- i
    j=i+1

    #find all images within a sequence, image creation times must be within maxdiff seconds to be considered in sequence
    while(!is.na(animals$DateTime[j]) & !is.na(animals$DateTime[i]) & j<nrow(animals) & animals$Camera[j]==animals$Camera[i] & difftime(animals$DateTime[j],animals$DateTime[i],units="secs")<=maxdiff){
      sequence <- c(sequence,j)
      j=j+1
    }

    #check if there are multiple crops in a sequence
    if(length(sequence)>1){

      predclass<-apply(predsort[sequence,],1,which.max)

      #no images are empty or there is no empty class
      if(length(emptycol)==0 | !(emptycol %in% predclass) | length(which(predclass %in% emptycol))==length(sequence)){
        predsort2<-predsort[sequence,]*animals$md_confidence[sequence]
        predbest<-apply(predsort2,2,mean)
        animals$prediction[sequence]<-which.max(predbest)
        animals$confidence[sequence]<-max(predsort2[,which.max(predbest)])
        animals$Species[sequence]<-classes[animals$prediction[sequence]]
      }
      # there are some images within the sequence that are empty
      else{
        #are all boxes empty?
        sel<-tapply(predclass==emptycol,animals$FilePathOrig[sequence],sum)==
          tapply(predclass==emptycol,animals$FilePathOrig[sequence],length)

        #classify images with species
        sel2<-which(animals$FilePathOrig[sequence] %in% names(sel[!sel]) & !(predclass %in% emptycol))
        sel3<-which(animals$FilePathOrig[sequence] %in% names(sel[!sel]))

        #choose the best class weighted by the most confident MD prediction
        predsort2<-matrix(predsort[sequence[sel2],]*animals$md_confidence[sequence[sel2]],ncol=ncol(predsort))
        predbest<-apply(predsort2,2,mean)
        animals$prediction[sequence[sel3]]<-which.max(predbest)
        animals$confidence[sequence[sel3]]<-max(predsort2[,which.max(predbest)])
        animals$Species[sequence[sel3]]<-classes[animals$prediction[sequence[sel3]]]

        #classify empty images
        for(s in names(sel[sel])){
          sel2<-which(animals$FilePathOrig[sequence] %in% s)
          predbest<-apply(matrix(predsort[sequence[sel2],]*animals$md_confidence[sequence[sel2]],ncol=ncol(predsort)),2,mean)
          animals$prediction[sequence[sel2]]<-which.max(predbest)
          animals$confidence[sequence[sel2]]<-max(predbest)
          animals$Species[sequence[sel2]]<-classes[animals$prediction[sequence[sel2]]]
        }
      }
    }
    #only one image in the sequence
    else{
      predbest<-predsort[sequence,]
      animals$prediction[sequence]<-which.max(predbest)
      animals$confidence[sequence]<-max(predbest*animals$md_confidence[sequence])
      animals$Species[sequence]<-classes[animals$prediction[sequence]]
    }
    i=j
  })
  # save classified images
  if(!is.null(outfile)){
    saveData(animals, outfile)
  }
  animals
}
