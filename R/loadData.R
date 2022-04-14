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