#' Extract exif data and create dataframe, adjust time if necessary.
#'
#' @param imagedir file path
#' @param outfile file path to which the data frame should be saved
#'
#' @return files dataframe 
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
    error = function(cond) {
      return(NULL)
    },
    warning = function(cond) {},
    finally = {}
  )
  
  if (length(files) == 0) {stop("No files found in directory.")}

  colnames(files)[1] <- "FilePath"
  files <- as.data.frame(files)
  files$DateTime <- sapply(files$FileModifyDate, function(x)toString(strptime(x, format="%Y:%m:%d %H:%M:%S")[1]))
  # Save file manifest
  if (!is.null(outfile)) { saveData(files, outfile)}

  files
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
#'   files_per_command <- length(path)
#'   commands <- exiftool_command(args = args, fnames = path)
#'   ngroups <- 1
#'   groups <- rep(1, length(path))
#'   while (any(nchar(commands) >= (command_length * 0.75)) &&
#'          (files_per_command >= 2)) {
#'     files_per_command <- files_per_command%/%2
#'     ngroups <- (length(path) + files_per_command - 1)%/%files_per_command
#'     groups <- rep(seq_len(ngroups), rep(files_per_command,
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

