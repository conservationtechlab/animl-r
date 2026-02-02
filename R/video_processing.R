#' Extract frames from video for classification
#'
#' @param files dataframe of videos
#' @param frames number of frames to sample
#' @param fps frames per second, otherwise determine mathematically
#' @param out_file csv file to which results will be saved
#' @param out_dir directory to save frames to if not null
#' @param file_col string value indexing which column contains file paths
#' @param parallel Toggle for parallel processing, defaults to FALSE
#' @param num_workers number of processors to use if parallel, defaults to 1
#'
#' @return dataframe of still frames for each video
#' @export
#'
#' @examples
#' \dontrun{
#' frames <- extract_frames(manifest, out_dir = "C:\\Users\\usr\\Videos\\", frames = 5)
#' }
extract_frames <- function(files, frames=5, fps=NULL, out_file=NULL, out_dir=NULL,
                           file_col="filepath", parallel=FALSE, num_workers=1){
  if (!is.null(fps)){ fps <- as.integer(fps) }
  if (!is.null(frames)){ frames <- as.integer(frames) }
  animl_py <- .animl_internal$animl_py
  animl_py$extract_frames(files, frames=frames, fps=fps, out_file=out_file, out_dir=out_dir,
                          file_col=file_col, parallel=parallel, num_workers=as.integer(num_workers))
}


#' Given a video path, return a specific frame as an RGB image
#'
#' @param video_path path to video file
#' @param frame frame number to extract  (default is 0)

#'
#' @returns rgb_frame: extracted frame as RGB image
#' @export
#'
#' @examples
#' \dontrun{get_frame_as_image('/example/path/to/video.mp4', frame=213)}
get_frame_as_image <- function(video_path, frame=0){
  animl_py <- .animl_internal$animl_py
  animl_py$get_frame_as_image(video_path, frame=frame)
}