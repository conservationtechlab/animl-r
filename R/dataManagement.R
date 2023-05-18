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
  if (ext == "csv") { return(utils::read.csv(file)) }
  else{ stop("Error. Expecting a .csv file.") }
}

#' Save Data to Given File
#'
#' @param data the dataframe to be saved
#' @param outfile the full path of the saved file
#' @param prompt if true, prompts the user to confirm overwrite
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{
#'  saveData(files,"path/to/newfile.csv")
#' }
saveData <- function(data, outfile, prompt = TRUE) {
  if (file.exists(outfile) & prompt == TRUE) {
    if (tolower(readline(prompt = "Output file exists, would you like to overwrite? y/n: ")) == "y") {
      utils::write.csv(data, file = outfile, row.names = F, quote = F)
    }
  } 
  else { utils::write.csv(data, file = outfile, row.names = F, quote = F) }
}




#' Check for files existence and prompt user if they want to load
#'
#' @param file the full path of the file to check
#'
#' @return a boolean indicating wether a file was found 
#'             and the user wants to load or not
#' @importFrom methods is
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
