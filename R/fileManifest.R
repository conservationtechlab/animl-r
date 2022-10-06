#' Extract exif data and create dataframe, adjust time if necessary.
#'
#' @param imagedir file path
#' @param outfile file path to which the data frame should be saved
#'
#' @return files dataframe [FilePath,FileModifyDate]
#' @export
#'
#' @examples
#' \dontrun{
#' files <- extractFiles("C:\\Users\\usr\\Pictures\\")
#' }
buildFileManifest <- function(imagedir, outfile = NULL) {
  if (checkFile(outfile)) { return(loadData(outfile))}

  if (!dir.exists(imagedir)) {stop("The given directory does not exist.")}

  # Reads files in directory and extracts their EXIF data
  files <- tryCatch(
    {
      exifr::read_exif(imagedir, tags = c("filename", "directory", "FileModifyDate"), recursive = TRUE)
      
    },
    error = function(cond) { return(NULL)  },
    warning = function(cond) {},
    finally = {}
  )
  
  if (length(files) == 0) {stop("No files found in directory.")}

  colnames(files)[1] <- "FilePath"
  files <- as.data.frame(files)
<<<<<<< Updated upstream
  files$DateTime <- sapply(files$FileModifyDate, function(x)toString(strptime(x, format="%Y:%m:%d %H:%M:%S")[1]))
=======
  
  # update to type(date)
  
>>>>>>> Stashed changes
  # Save file manifest
  #
  if (!is.null(outfile)) { saveData(files, outfile)}

  files
}