#' Udate Results from File Browser
#'
#' @param resultsfile final results file with predictions, expects a "UniqueName" column
#' @param linkdir symlink directory that has been validated
#'
#' @return dataframe with new "Species" column that contains the verified species
#' @export
#'
#' @examples
#' \dontrun{
#' results <- updateResults(resultsfile, linkdir)
#' }
updateResults <- function(resultsfile, linkdir){
  if(!dir.exists(linkdir)) {stop("The given directory does not exist.")}
  
  FilePath <- list.files(linkdir, recursive = TRUE, include.dirs = TRUE)
  files <- data.frame(FilePath)
  
  files$UniqueName <- sapply(files$FilePath,function(x)strsplit(x,"/")[[1]][2])
  files$Species <- sapply(files$FilePath,function(x)strsplit(x,"/")[[1]][1])
  
  dplyr::rename(files, )
  
  results <- utils::read.csv(resultsfile)
  if("Species" %in% names(results)){results = results[,!(names(results) %in% c("Species"))]}
  
  corrected <- merge(results, files, by.x="UniqueName")
  return(corrected)
}