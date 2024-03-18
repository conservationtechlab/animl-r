#' Extract exif Data and Create File Manifest
#'
#' @param imagedir path to a directory with image or video files. 
#' @param exif returns date and time information from exif data, defaults to true
#' @param offset add offset in hours for videos when using the File Modified date, defaults to 0
#' @param recursive Should directories be scanned recursively? Default TRUE
#' @param outfile file path to which the data frame should be saved
#'
#' @return files dataframe with or without file dates
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' files <- extractFiles("C:\\Users\\usr\\Pictures\\")
#' }
buildFileManifest <- function(imagedir, exif = TRUE, offset=0, recursive=TRUE, outfile = NULL) {
  if (checkFile(outfile)) { return(loadData(outfile)) }
  
  if (!dir.exists(imagedir)) { stop("The given directory does not exist.") }
  
  # Reads files in directory and extracts their EXIF data
  if (exif) {
    files <- tryCatch( 
      {
        exifr::read_exif(imagedir, tags = c("filename", "directory", "DateTimeOriginal","FileModifyDate","CreateDate"), recursive = recursive)
      }
      error = function(cond) { return(NULL) },
      warning = function(cond) {},
      finally = {}
    )
    if (length(files) == 0) { stop("No files found in directory.") }
    
    colnames(files)[1] <- "FilePath"
    files <- as.data.frame(files)
    
    files$FileModifyDate <- as.POSIXct(files$FileModifyDate, format="%Y:%m:%d %H:%M:%S")
    files$CreateDate <- as.POSIXct(files$CreateDate, format="%Y:%m:%d %H:%M:%S")
    
    
    if ("DateTimeOriginal" %in% names(files)){
      files$DateTimeOriginal <- as.POSIXct(files$DateTimeOriginal, format="%Y:%m:%d %H:%M:%S")
      files <- files %>% dplyr::mutate("DateTime" = dplyr::coalesce(files$DateTimeOriginal, files$CreateDate, files$FileModifyDate + (offset*3600)))
    }
    else{
      files$DateTime = files$FileModifyDate
    }
  }
  # return simple file list 
  else {
    files <- list.files(imagedir, full.names = TRUE, recursive = recursive)
    files <- as.data.frame(files)
    colnames(files)[1] <- "FilePath"
    files$FileName <- sapply(files$FilePath, function(x) basename(x))
    files$Directory <- sapply(files$FilePath, function(x) dirname(x))                                               
  }
  
  #save output
  if (!is.null(outfile)) { saveData(files, outfile) }
  
  files
}
