#' Create SymLink Directories and Sort Classified Images
#'
#' @param alldata DataFrame of classified images 
#' @param linkdir Destination directory for symlinks
#'
#' @return none
#' @export 
#'
#' @examples
#' \dontrun{}
symlinkClasses <- function(alldata, linkdir) {
  

  # place low-confidence images into "Unknown" category
  alldata$prediction[alldata$confidence < 0.5 &
    !(alldata$prediction %in% c("empty", "Empty", "human", "Human", "vehicle", "Vehicle"))] <- "Unknown"

  # create species directories
  for (s in unique(alldata$prediction)) {
    if (!dir.exists(paste0(linkdir, s))) dir.create(paste0(linkdir, s), recursive = T)
  }
  
  # create link
  alldata$Link <- paste0(linkdir, alldata$prediction, "/", alldata$NewName)
  print(alldata)

  # link images to species directory
  mapply(file.link, alldata$FilePath, alldata$Link)
  print(alldata)
  
  alldata
}

symlinkMD <- function(alldata,linkdir){
  MDclasses <- c("empty","animal","human","vehicle")
  for (s in MDclasses) {
    if (!dir.exists(paste0(linkdir, s))) dir.create(paste0(linkdir, s), recursive = T)
  }
  
  alldata$MDprediction <- sapply(alldata$max_detection_category,function(x) MDclasses[x+1])
  
  alldata$MDLink <- paste0(linkdir, alldata$MDprediction, "/", alldata$NewName)
  print(alldata)
  
  # link images to species directory
  mapply(file.link, alldata$FilePath, alldata$MDLink)
  
}
