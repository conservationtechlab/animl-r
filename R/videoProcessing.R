#' Extract frames from video for classification
#'
#' This function can take
#'
#' @param files dataframe of videos
#' @param outdir directory to save frames to
#' @param outfile file to which results will be saved
#' @param fps frames per second, otherwise determine mathematically
#' @param frames number of frames to sample
#' @param parallel Toggle for parallel processing, defaults to FALSE
#' @param workers number of processors to use if parallel, defaults to 1
#' @param checkpoint if not parallel, checkpoint ever n files, defaults to 1000
#'
#' @return dataframe of still frames for each video
#' @export
#'
#' @examples
#' \dontrun{
#' frames <- extractFrames(videos, outdir = "C:\\Users\\usr\\Videos\\", frames = 5)
#' }
extractFrames <- function(files, outdir = tempfile(), outfile = NULL,
                          fps = NULL, frames = NULL, parallel = FALSE, 
                          workers = 1, checkpoint = 1000) {

  if (checkFile(outfile)) { return(loadData(outfile)) } 
  
  #check if input is dataframe
  if (is(files, "data.frame")) { 
    images <- files[tolower(tools::file_ext(files$FileName)) %in% c("jpg", "png", "jpeg"), ]
    images$Frame <- images$FilePath
    videos <- files[tolower(tools::file_ext(files$FileName)) %in% c("mp4", "avi", "mov", "wmv", "mpg", "mpeg", "asf", "m4v"), ]
    filelist <- videos$FilePath
  } #otherwise, check if list of videos 
  else if (is(files, "character") & length(files) > 0) {  filelist <- files }
  else { stop("Error: Expect 'files' to be Data Frame or vector of filepaths.") }
  
  if (outdir != "" & !dir.exists(outdir)) {
    if (!dir.create(outdir, recursive = TRUE)) { stop("Output directory invalid.\n") }
  }
  if (!is.null(fps) & !is.null(frames)) { message("If both fps and frames are defined fps will be used.") }
  if (is.null(fps) & is.null(frames)) { stop("Either fps or frames need to be defined.") }
 

  if (parallel) {
    type <- "PSOCK"
    pbapply::pboptions(use_lb=TRUE)
    cl <- parallel::makeCluster(min(parallel::detectCores(), workers), type = type)
    parallel::clusterExport(cl, list("outdir", "format", "fps", "frames"), envir = environment())

    parallel::clusterSetRNGStream(cl)

    parallel::clusterEvalQ(cl, library(av))
    results <- pbapply::pblapply(filelist, function(x) {
      try(extractFramesSingle(x, outdir, fps=fps, frames=frames)) }, cl = cl)
    parallel::stopCluster(cl)
    pbapply::pboptions(use_lb=FALSE)
    results <- do.call(rbind, results)
  } 
  else {
    pb <- pbapply::startpb(1, nrow(videos))
    results <- data.frame(FilePath = character(), Frame = character())
    for(i in 1:nrow(videos)){
      result <- extractFramesSingle(videos[i,]$FilePath, outdir, fps=fps, frames=frames)
      results <- rbind(results,result)
     
      # checkpoint
      if (!is.null(outfile) & (i %% checkpoint) == 0) {
        save(results, file = outfile)
      }
      pbapply::setpb(pb, i) 
    }
    pbapply::setpb(pb, nrow(videos))
    pbapply::closepb(pb)
  } 

  if (is(files, "data.frame")) { 
    videoframes <- merge(videos, results) 
    allframes <- rbind(images, videoframes)
  }
  else { allframes <- results }
  # save frames to files
  if(!is.null(outfile)) { saveData(allframes, outfile) }

  allframes
}


#' Extract Frames for Single Video
#'
#' @param x filepath to image
#' @param outdir directory to save frames to
#' @param fps number of frames per second to save
#' @param frames number of frames evenly distributed to save
#'
#' @return dataframe of filepaths, frame paths
#' @export
#'
#' @examples
#' \dontrun{
#' result <- extractFramesSingle(video$FilePath, outdir, frames=3)
#' }
extractFramesSingle <- function(x, outdir, fps=NULL, frames=NULL) {
  result <- tryCatch(
    { data <- av::av_media_info(x) },
    error = function(e) {
      message(e)
      return(NA)
    }
  )
  if (result$video$frames < 5) { 
    files <- data.frame(FilePath = x, Frame = "File Corrupt")
  } 
  else {
    vfilter <- ifelse(length(frames), paste0("fps=", round(1 / (result$duration / (frames-1)), 3)), "null")
    vfilter <- ifelse(length(fps), paste0("fps=", fps), vfilter)
    framerate <- result$video$framerate
    tempdir <- tempfile()
    dir.create(tempdir)
    name <- strsplit(basename(x), ".", fixed = T)[[1]][1]
    rnd <- sprintf("%05d", round(stats::runif(1, 1, 99999), 0))
    #always extract first frame
    output <- file.path(tempdir, paste0(name, "_", rnd, "_00000", ".jpg"))
    av::av_encode_video(input = x, output = output, framerate = framerate, vfilter = "select=eq(n\\,0)", verbose = F)
    first <- data.frame(FilePath = x, tmpframe = list.files(tempdir, pattern = paste0(name, "_", rnd, "_00000", ".jpg"), full.names = TRUE), stringsAsFactors = F)
    #extract remaining frames
    output <- file.path(tempdir, paste0(name, "_", rnd, "_%5d", ".jpg"))
    av::av_encode_video(input = x, output = output, framerate = framerate, vfilter = vfilter, verbose = F)
    files <- data.frame(FilePath = x, tmpframe = list.files(tempdir, pattern = paste0(name, "_", rnd, "_\\d{5}", ".jpg"), full.names = TRUE), stringsAsFactors = F)

    files$Frame <- paste0(outdir, basename(files$tmpframe))
    file.copy(files$tmpframe, files$Frame)
    file.remove(files$tmpframe)
    files[, c("FilePath", "Frame")]
  }
}

