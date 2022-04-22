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

  # create link
  alldata$Link <- paste0(linkdir, alldata$Species, "/", alldata$NewName)

  # place low-confidence images into "Unknown" category
  alldata$Species[alldata$confidence < 0.5 &
    !(alldata$Species %in% c("empty", "Empty", "human", "Human", "vehicle", "Vehicle"))] <- "Unknown"

  # create species directories
  for (s in unique(alldata$Species)) {
    if (!dir.exists(paste0(linkdir, s))) dir.create(paste0(linkdir, s), recursive = T)
  }

  # link images to species directory
  mapply(file.link, alldata$FilePath, alldata$Link)
}