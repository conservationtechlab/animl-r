#' Extract exif data and create dataframe, adjust time if necessary.
#'
#' @param imagedir file path
#' @param timezone_offset integer to adjust file modify time
#'
#' @return images
#' @export
#'
#' @examples
#' \dontrun{
#' images <- extractFiles("C:\\Users\\usr\\Pictures\\")
#' }
extractFiles <- function(imagedir,timezone_offset=0){
  images<-exifr::read_exif(imagedir,tags=c("filename","directory","DateTimeOriginal","FileModifyDate"), recursive = TRUE)
  colnames(images)[1]<-"FilePath"
  images<-as.data.frame(images)
  if(!"DateTimeOriginal" %in% names(images)){
    images$DateTime<-as.POSIXct(images$FileModifyDate,format="%Y:%m:%d %H:%M:%S")
  }
  else{images$DateTime<-as.POSIXct(images$DateTimeOriginal,format="%Y:%m:%d %H:%M:%S") }

  images$DateTimeModified<-as.POSIXct(images$FileModifyDate,format="%Y:%m:%d %H:%M:%S")
  images$DateTimeAdjusted <- as.POSIXct(images$FileModifyDate,format="%Y:%m:%d %H:%M:%S")+timezone_offset*3600

  return(images)
}

#' #' Extract exif data using parallel processing *NEEDS CORRECTING
#' #'
#' #' @param path
#' #' @param tags
#' #' @param recursive
#' #' @param args
#' #' @param quiet
#' #' @param parallel
#' #' @param nproc
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' extractFiles_par <-function(path, tags = NULL, recursive = FALSE, args = NULL,
#'                          quiet = TRUE,parallel=F,nproc=detectCores()){
#'   require(exifr)
#'   path <- path.expand(path)
#'   # if (recursive) {
#'   #   missing_dirs <- path[!dir.exists(path)]
#'   #   if (length(missing_dirs) > 0) {
#'   #     stop("Did you mean recursive = TRUE? ", "The following directories are missing (or are not directories). ",
#'   #          paste(missing_files, collapse = ", "))
#'   #   }
#'   # }
#'   # else {
#'   #   missing_files <- path[!file.exists(path) | dir.exists(path)]
#'   #   if (length(missing_files) > 0) {
#'   #     stop("Did you mean recursive = TRUE? ", "The following files are missing (or are not files): ",
#'   #          paste(missing_files, collapse = ", "))
#'   #   }
#'   # }
#'   if (length(path) == 0) {
#'     return(tibble::tibble(SourceFile = character(0)))
#'   }
#'   if (recursive) {
#'     args <- c(args, "-r")
#'   }
#'   if (!is.null(tags)) {
#'     tags <- gsub("\\s", "", tags)
#'     args <- c(paste0("-", tags), args)
#'   }
#'   args <- unique(c("-n", "-j", "-q", "-b", args))
#'   if (quiet) {
#'     args <- c(args, "-q")
#'   }
#'   if (!quiet)
#'     message("Generating command line arguments...")
#'   if (.Platform$OS.type == "windows") {
#'     command_length <- 8191
#'   }else{
#'     command_length <- 50 * 1024
#'   }
#'   images_per_command <- length(path)
#'   commands <- exiftool_command(args = args, fnames = path)
#'   ngroups <- 1
#'   groups <- rep(1, length(path))
#'   while (any(nchar(commands) >= (command_length * 0.75)) &&
#'          (images_per_command >= 2)) {
#'     images_per_command <- images_per_command%/%2
#'     ngroups <- (length(path) + images_per_command - 1)%/%images_per_command
#'     groups <- rep(seq_len(ngroups), rep(images_per_command,
#'                                         ngroups))[seq_along(path)]
#'     commands <- vapply(split(path, groups), function(fnames) exiftool_command(args = args,
#'                                                                               fnames = fnames), character(1))
#'   }
#'   if (!quiet)
#'     message("Running ", length(commands), " commands",commands,"\n",args)
#'   if(parallel){
#'     require(parallel)
#'     type="PSOCK"
#'
#'     cl <- makeCluster(min(detectCores(),nproc),type=type)
#'     clusterExport(cl,list("path","groups","args","quiet"),
#'                   envir=environment())
#'
#'     clusterEvalQ(cl,library(exifr))
#'
#'     results <- pblapply(split(path, groups), function(fnames) exifr:::read_exif_base(args,fnames, quiet = quiet),cl=cl)
#'     stopCluster(cl)
#'
#'   }else{
#'     results <- pblapply(split(path, groups), function(fnames) exifr:::read_exif_base(args,fnames, quiet = quiet))
#'   }
#'   tibble::as_tibble(do.call(plyr::rbind.fill, results))
#' }

