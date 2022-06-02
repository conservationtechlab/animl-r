#' Load .csv or .Rdata file 
#'
#' @param file the full path of the file to load
#'
#' @return data extracted from the file
#' @export
#'
#' @examples
#' \dontrun{
#'   loadData("path/to/newfile.csv")
#' }

loadData <- function(file) {
  ext <- strsplit(basename(file), split="\\.")[[1]][-1]
  if(ext == "csv"){
    return(read.csv(file))
  }
  if(ext == "RData"){
    load(file)
  }
}

#' Save Data to Given File
#'
#' @param data the dataframe to be saved
#' @param outfile the full path of the saved file
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{
#'  saveData(files,"path/to/newfile.csv")
#' }
saveData <- function(data, outfile) {
  if (file.exists(outfile)) {
    if (tolower(readline(prompt = "Output file exists, would you like to overwrite? y/n: ")) == "y") {
      write.csv(data, file = outfile, row.names = F, quote = F)
    }
  } else {
    write.csv(data, file = outfile, row.names = F, quote = F)
  }
}




#' Check for files existence and prompt user if they want to load
#'
#' @param file the full path of the file to check
#'
#' @return a boolean indicating wether a file was found 
#'             and the user wants to load or not
#' @export
#'
#' @examples
#' \dontrun{
#'   checkFile("path/to/newfile.csv")
#' }
checkFile <- function(file) {
  if (!is.null(file) && file.exists(file)) {
    date <- exifr::read_exif(file, tags = "FileModifyDate")[[2]]
    date <- strsplit(date, split = " ")[[1]][1]
    if (tolower(readline(prompt = sprintf("Output file already exists and was last modified %s, would you like to load it? y/n: ", date)) == "y")) {
      return(TRUE)
    }
  }
  FALSE
}
