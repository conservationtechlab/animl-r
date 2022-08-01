#' Select Best Classification From Multiple Frames
#'
#' @param animals dataframe of all frames including species classification
#' @param how method for selecting best prediction, defaults to most frequent
#' @param outfile file path to which the data frame should be saved
#'
#' @return dataframe with new prediction in "Species" column
#' @export
#'
#' @examples
#' \dontrun{
#' mdanimals <- classifyVideo(mdanimals)
#' }
poolCrops <- function(animals, how = "count", count = FALSE, shrink = FALSE, outfile = NA) {
  if (checkFile(outfile)) { return(loadData(outfile))}
  if (!is(animals, "data.frame")) { stop("'animals' must be DataFrame")}
  
  videonames <- unique(animals$NewName)
  
  steps <- length(videonames)
  pb <- pbapply::startpb(1, steps)
  
  row <- 1
  for (i in 1:steps) {
    v <- videonames[i]
    sequence <- animals[animals$NewName == v, ]
    guesses <- sequence %>%
      dplyr::group_by(sequence$prediction) %>%
      dplyr::summarise(mean = mean(sequence$confidence), n = dplyr::n())
    
    if (how == "conf"){ 
      guess <- guesses[which.max(guesses$mean), ] 
    }  
    else {
        best <- which.max(guesses$n)
        guess <- guesses[best, ]
        if (guess$prediction == "empty" && nrow(guesses) > 1) {
          guesses <- guesses[-best, ]
          guess <- guesses[which.max(guesses$mean), ]
        }
      }
      
    animals[animals$NewName == v, ]$prediction <- guess$prediction
    if(count){
      animals[animals$NewName == v, ]$count <- guess$n
    }} 
    pbapply::setpb(pb, i)
  }
  pbapply::setpb(pb, steps)
  pbapply::closepb(pb)
  
  if(shrink){
    topchoice = animals[order(animals[,'FilePath'],-animals[,'confidence']),]
    animals = topchoice[!duplicated(topchoice$NewName),]
  }
  
  # save data
  if(!is.na(outfile)){ saveData(animals, outfile)}
  animals
}
