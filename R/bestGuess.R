#' Select Best Classification From Multiple Frames
#'
#' @param manifest dataframe of all frames including species classification
#' @param sort method for selecting best prediction, defaults to most frequent
#' @param count if true, return column with number of MD crops for that animal (does not work for images)
#' @param shrink if true, return a reduced dataframe with one row per image
#' @param outfile file path to which the data frame should be saved
#' @param prompt if true, prompts the user to confirm overwrite
#' @param parallel Toggle for parallel processing, defaults to FALSE
#' @param workers number of processors to use if parallel, defaults to 1
#'
#' @return dataframe with new prediction in "Species" column
#' @import dplyr
#' @importFrom methods is
#' @export
#'
#' @examples
#' \dontrun{
#' mdmanifest <- bestGuess(manifest, sort = "conf")
#' }
bestGuess <- function(manifest, sort = "count", count = FALSE, shrink = FALSE, 
                      outfile = NULL, prompt = TRUE, parallel = FALSE, workers = 1) {
  
  if (checkFile(outfile)) { return(loadData(outfile))}
  if (!is(manifest, "data.frame")) { stop("'manifest' must be DataFrame")}
  
  videonames <- unique(manifest$FilePath)
  steps <- length(videonames)
  
  run.parallel <- function(i){
    sequence <- manifest[manifest$FilePath == videonames[i], ]
    
    guesses <- sequence %>% dplyr::group_by(prediction) %>% dplyr::summarise(confidence = max(confidence), n = dplyr::n())
    
    #most confident
    if (sort == "conf") { 
      guess  <- guesses[which.max(guesses$confidence), ] 
      if (guess$prediction == "empty" && nrow(guesses) > 1) {
        guesses <- guesses[guesses$prediction != "empty", ]
        guess <- guesses[which.max(guesses$confidence), ]
      }
    }  
    #most frequent unless empty
    else if (sort == "count") {
      guesses <- guesses[order(guesses$confidence, decreasing=TRUE),]
      best <- which.max(guesses$n)
      guess <- guesses[best, ]
      if (guess$prediction == "empty" && nrow(guesses) > 1) {
        guesses <- guesses[guesses$prediction != "empty", ]
        guess <- guesses[which.max(guesses$n), ]
      }
    }
    else { stop("Must select guess by 'conf' (confidence) or by 'count' (frequency)") }
    #print(guess)
    sequence$prediction <- guess$prediction
    sequence$confidence <- guess$confidence
    
    if (count) { sequence$count <- guess$n }
    # one entry per image/video
    if(shrink){ sequence = sequence[!duplicated(sequence$FilePath),] }
    
    sequence
  }
  
  if (parallel) {
    cl <- parallel::makeCluster(min(parallel::detectCores(), workers), type = "PSOCK")
    parallel::clusterExport(cl, list("sort", "count", '%>%'), envir = environment())
    parallel::clusterSetRNGStream(cl)
    
    results <- pbapply::pblapply(1:steps, function(x) { run.parallel(x) }, cl = cl)
    parallel::stopCluster(cl)
  } 
  else {
    results <- pbapply::pblapply(1:steps, function(x) { run.parallel(x) })
  }
  
  results <- do.call(rbind, results)
  
  if(!is.null(outfile)){saveData(results, outfile, prompt)}
  
  results
}

utils::globalVariables(c("prediction", "confidence"))