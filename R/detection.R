
MD_LABELS = list('0'='empty', '1'='animal', '2'='human', '3'='vehicle')

#' Load an Object Detector
#'
#' @param model_path path to detector model file
#' @param model_type type of model expected:  "mdv5", "mdv6", "mdv1000-cedar", "mdv1000-larch", "mdv1000-sorrel",
#'                   "mdv1000-redwood", "mdv1000-spruce", "yolov5", "yolo", "onnx" 
#'                   for yolo models v6+, use "yolo", for v5, use "yolov5". 
#'                   for mdv1000 models, specify the version: cedar, larch, sorrel, redwood, spruce
#' @param device specify to run on cpu or gpu
#'
#' @return detector object
#' @export
#'
#' @examples
#' \dontrun{md_py <- megadetector("/mnt/machinelearning/megaDetector/md_v5a.0.0.pt", 
#'                                model_type='mdv5', device='cuda:0')}
load_detector <- function(model_path, model_type, device=NULL){
  animl_py <- .animl_internal$animl_py
  animl_py$load_detector(model_path, model_type=model_type, device=device)
}


#' Apply MegaDetector to a Given Batch of Images
#'
#' @param detector preloaded md model
#' @param image_file_names list of image filenames, a single image filename, or folder
#' @param resize_width  width to resize images to
#' @param resize_height height to resize images to
#' @param letterbox if True, resize and pad image to keep aspect ratio, else resize without padding
#' @param category_map mapping of category IDs to human-readable labels
#' @param confidence_threshold only detections above this threshold are returned
#' @param file_col select which column if image_file_names is a manifest
#' @param batch_size size of each batch
#' @param num_workers number of processes to handle the data
#' @param device  specify to run on cpu or gpu
#' @param checkpoint_path path to checkpoint file
#' @param checkpoint_frequency write results to checkpoint file every N images
#'
#' @return list with two elements: $detections - Megadetector json format detections, $failed_files - files that failed on inference
#' @export
#'
#' @examples
#' \dontrun{mdres <- detect(md_py, allframes$Frame, 1280, 960, device='cpu')}
detect <- function(detector,
                   image_file_names,
                   resize_width,
                   resize_height,
                   letterbox=TRUE,
                   category_map=MD_LABELS,
                   confidence_threshold=0.1,
                   file_col='filepath',
                   batch_size=1,
                   num_workers=1,
                   device=NULL,
                   checkpoint_path=NULL,
                   checkpoint_frequency=-1){
  
  animl_py <- .animl_internal$animl_py
  results <- animl_py$detect(detector,
                  image_file_names, 
                  as.integer(resize_width), 
                  as.integer(resize_height),
                  letterbox=letterbox,
                  category_map=category_map,
                  confidence_threshold=confidence_threshold,
                  file_col=file_col,
                  batch_size=as.integer(batch_size),
                  num_workers=as.integer(num_workers),
                  device=device,
                  checkpoint_path=checkpoint_path,
                  checkpoint_frequency=as.integer(checkpoint_frequency))
  names(results) <- c('detections','failed_files')
  return(results)
}


#' Parse MD results into a simple dataframe
#'
#' @param results json output from megadetector
#' @param manifest optional dataframe containing all frames
#' @param out_file optional path to save dataframe
#' @param threshold confidence threshold to include bbox
#' @param file_col column in manifest that refers to file paths 
#' 
#' @return original dataframe including md results
#' @export
#'
#' @examples
#' \dontrun{
#' mdresults <- parseMD(mdres)
#' }
parse_detections <- function(results,
                             manifest=NULL,
                             out_file=NULL,
                             threshold=0,
                             file_col="filepath") {
  
  animl_py <- .animl_internal$animl_py
  animl_py$parse_detections(results,
                            manifest=manifest,
                            out_file=out_file,
                            threshold=threshold,
                            file_col=file_col)
}


#' Return MD empty, vehicle and human images in a dataframe
#'
#' @param manifest all megadetector frames
#'
#' @return list of empty/human/vehicle allframes with md classification
#' @export
#'
#' @examples
#' \dontrun{
#' empty <- get_empty(imagesall)
#' }
get_empty <- function(manifest) {
  if (!is(manifest, "data.frame")) { stop("'manifest' must be Data Frame")}
  
  # fill md empties with 0
  manifest$category[is.na(manifest$category)] <- 0
  empty <- manifest[manifest$category != 1, ]
  
  if (nrow(empty) == 0) {
    empty <- data.frame(matrix(ncol = ncol(manifest), nrow = 0))
    colnames(empty) <- names(manifest)
    return(empty)
  }
  empty$prediction <- NA
  empty$confidence <- NA
  
  categories <- unique(manifest$category)
  if (0 %in% categories) {
    empty[which(empty$category == 0), "prediction"] <- "empty"
    empty[which(empty$category == 0), "confidence"] <- 1
  }
  if (2 %in% categories) {
    empty[which(empty$category == 2), "prediction"] <- "human"
    empty[which(empty$category == 2), "confidence"] <- empty$conf[which(empty$category == 2)]
  }
  if (3 %in% categories) {
    empty[which(empty$category == 3), "prediction"] <- "vehicle"
    empty[which(empty$category == 3), "confidence"] <- empty$conf[which(empty$category == 3)]
  }
  return(empty)
}


#' Return a dataframe of only MD animals
#'
#' @param manifest all megadetector frames
#'
#' @return animal frames classified by MD
#' @export
#'
#' @examples
#' \dontrun{
#' animals <- get_animals(imagesall)
#' }
get_animals <- function(manifest){
  if (!is(manifest, "data.frame")) { stop("'manifest' must be Data Frame")}
  return(manifest[which(manifest$category == 1), , drop = FALSE])
}
