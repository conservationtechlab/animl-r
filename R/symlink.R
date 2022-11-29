#' Create SymLink Directories and Sort Classified Images
#'
#' @param alldata DataFrame of classified images 
#' @param linkdir Destination directory for symlinks
#' @param outfile Results file to save to
#' @param copy Toggle to determine copy or hard link, defaults to link
#' 
#'
#' @return none
#' @export 
#'
#' @examples
#' \dontrun{}
symlinkClasses <- function(manifest, linkdir, threshold = 0.5, outfile = NULL, copy = FALSE) {
  if (checkFile(outfile)) { return(loadData(outfile))}
  
  if (threshold)
  # place low-confidence images into "Unknown" category
  alldata$prediction[alldata$confidence < threshold &
    !(alldata$prediction %in% c("empty", "Empty", "human", "Human", "vehicle", "Vehicle"))] <- "unknown"

  # create species directories
  for (s in unique(alldata$prediction)) {
    if (!dir.exists(paste0(linkdir, s))) dir.create(paste0(linkdir, s), recursive = T)
  }
  
  if ("UniqueName" %in% names(manifest)) {
    alldata$Link <- paste0(linkdir, alldata$prediction, "/", alldata$UniqueName)
  }
  
  else { # create a new name
    name <- strsplit(basename(x), ".", fixed = T)[[1]][1]
    rnd <- sprintf("%05d", round(stats::runif(1, 1, 99999), 0))
    output <- file.path(tempdir, paste0(name, "_", rnd, "_%5d", ".jpg"))
  }
  # create link
  
  if(copy){
    mapply(file.copy, alldata$FilePath, alldata$Link)
  }
  else{
    mapply(file.link, alldata$FilePath, alldata$Link)
  }
  if (!is.null(outfile)) { saveData(alldata, outfile)}  
}




#' Create SymLink Directories and Sort Classified Images Based on MD Results
#'
#' @param alldata DataFrame of classified images 
#' @param linkdir Destination directory for symlinks
#' @param outfile Results file to save to
#' @param copy Toggle to determine copy or hard link, defaults to link
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{}
symlinkMD <- function(alldata, linkdir, outfile = resultsfile, copy=FALSE){
  if (checkFile(outfile)) { return(loadData(outfile))}
  
  MDclasses <- c("empty","animal","human","vehicle")
  for (s in MDclasses) {
    if (!dir.exists(paste0(linkdir, s))) dir.create(paste0(linkdir, s), recursive = T)
  }
  
  alldata$MDprediction <- sapply(alldata$max_detection_category,function(x) MDclasses[x+1])
  
  alldata$MDLink <- paste0(linkdir, alldata$MDprediction, "/", alldata$NewName)

  if(copy){
    mapply(file.copy, alldata$FilePath, alldata$MDLink)
  }
  else{
    mapply(file.link, alldata$FilePath, alldata$MDLink)
  }
  if (!is.null(outfile)) { saveData(files, outfile)}
}
