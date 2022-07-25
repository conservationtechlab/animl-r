#' 
#'
#' @param resultsfile the csv containing file names and classifier predictions
#' @param linkdir the directory of sorted sym or hard links
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' results <- correctResults("/mnt/projects/Stacy-Dawes_Kenya/WWK_2022-06-09/Working-Directory/Data/Results.csv","/mnt/projects/Stacy-Dawes_Kenya/WWK_2022-06-09/Working-Directory/Link")
#' write.csv(results, "/mnt/projects/Stacy-Dawes_Kenya/WWK_2022-06-09/Working-Directory/Data/CorrectedResults.csv")
#' }
correctResults <- function(resultsfile, linkdir){
  if(!dir.exists(linkdir)) {stop("The given directory does not exist.")}
  
  FilePath <- list.files(linkdir,recursive = TRUE, include.dirs = TRUE)
  files <- data.frame(FilePath)
  
  files$FileName <- sapply(files$FilePath,function(x)strsplit(x,"/")[[1]][2]) 
  files$Species <- sapply(files$FilePath,function(x)strsplit(x,"/")[[1]][1]) 
  
  results <- read.csv(resultsfile)
  if("Species" %in% names(results)){results = results[,!(names(results) %in% c("Species"))]}
  
  corrected <- merge(results,files,by="FileName")
  return(corrected)
}


