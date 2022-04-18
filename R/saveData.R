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
