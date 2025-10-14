#' Extract frames from video for classification
#'
#' This function can take
#'
#' @param files dataframe of videos
#' @param out_dir directory to save frames to
#' @param out_file file to which results will be saved
#' @param fps frames per second, otherwise determine mathematically
#' @param frames number of frames to sample
#' @param file_col string value indexing which column contains file paths
#' @param parallel Toggle for parallel processing, defaults to FALSE
#' @param num_workers number of processors to use if parallel, defaults to 1
#' @param checkpoint if not parallel, checkpoint ever n files, defaults to 1000
#'
#' @return dataframe of still frames for each video
#' @export
#'
#' @examples
#' \dontrun{
#' frames <- extract_frames(manifest, out_dir = "C:\\Users\\usr\\Videos\\", frames = 5)
#' }
extract_frames <- function(files, out_dir = tempfile(), out_file = NULL,
                           fps = NULL, frames = NULL, file_col="filepath", 
                           parallel = FALSE, num_workers = 1, checkpoint = 1000) {
  if (!is.null(fps)){ fps <- as.integer(fps) }
  if (!is.null(frames)){ frames <- as.integer(frames) }
  animl_py <- get("animl_py", envir = parent.env(environment()))
  animl_py$extract_frames(files, out_dir, out_file=out_file, fps=fps, frames=frames, 
                          file_col=file_col, parallel=parallel, num_workers=as.integer(num_workers), 
                          checkpoint=as.integer(checkpoint))
}
