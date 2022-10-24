#' Extract exif Data and Create File Manifest
#'
#' @param imagedir file path
#' @param exif returns date and time information from exif data, defaults to true
#' @param offset add offset to videos, defaults to 0
#' @param outfile file path to which the data frame should be saved
#'
#' @return files dataframe [FilePath,FileModifyDate]
#' @export
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' files <- extractFiles("C:\\Users\\usr\\Pictures\\")
#' }
buildFileManifest <- function(imagedir, exif = TRUE, offset=0, outfile = NULL) {
  if (checkFile(outfile)) { return(loadData(outfile)) }
  
  if (!dir.exists(imagedir)) { stop("The given directory does not exist.") }
  
  # Reads files in directory and extracts their EXIF data
  if (exif) {
    files <- tryCatch( 
      {
        exifr::read_exif(imagedir, tags = c("filename", "directory", "DateTimeOriginal","FileModifyDate"), recursive = TRUE)
      },
      error = function(cond) { return(NULL) },
      warning = function(cond) {},
      finally = {}
    )
    if (length(files) == 0) { stop("No files found in directory.") }
    
    colnames(files)[1] <- "FilePath"
    files <- as.data.frame(files)
    files$DateTimeOriginal <- as.POSIXct(files$DateTimeOriginal, format="%Y:%m:%d %H:%M:%S")
    files$FileModifyDate <- as.POSIXct(files$FileModifyDate, format="%Y:%m:%d %H:%M:%S")
    files$DateTimeAdjusted <- as.POSIXct(files$FileModifyDate, format="%Y:%m:%d %H:%M:%S") + (offset*3600)
    
    files <- files %>% dplyr::mutate(DateTime = dplyr::coalesce(DateTimeOriginal, FileModifyDate))
  }
  # return simple file list 
  else{
    files <- list.files(imagedir, full.names = TRUE, recursive = TRUE)
    files <- as.data.frame(files)
    colnames(files)[1] <- "FilePath"
    files$FileName <- sapply(files$FilePath, function(x) basename(x))
    files$Directory <- sapply(files$FilePath, function(x) dirname(x))                                               
  }
  
  #save output
  if (!is.null(outfile)) { saveData(files, outfile) }
  
  files
}
