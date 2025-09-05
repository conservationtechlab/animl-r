#' Load an Object Detector
#'
#' @param model_path path to detector model file
#' @param model_type type of model expected ["MDV5", "MDV6", "YOLO"]
#' @param device specify to run on cpu or gpu
#'
#' @return megadetector object
#' @export
#'
#' @examples
#' \dontrun{md_py <- megadetector("/mnt/machinelearning/megaDetector/md_v5a.0.0.pt")}
load_detector <- function(model_path, model_type, device=NULL){
  # first check if animl-py is loaded
  if(reticulate::py_module_available("animl")){
    animl_py <- reticulate::import("animl")
  }
  else{
    stop('animl-py environment must be loaded first via reticulate')
  }
  animl_py$load_detector(model_path, model_type, device=device)
}


#' Apply MegaDetector to a Given Batch of Images
#'
#' @param detector preloaded md model
#' @param image_file_names list of image filenames, a single image filename, or folder
#' @param resize_width  width to resize images to
#' @param resize_height height to resize images to
#' @param letterbox if True, resize and pad image to keep aspect ratio, else resize without padding
#' @param confidence_threshold only detections above this threshold are returned
#' @param file_col select which column if image_file_names is a manifest
#' @param batch_size size of each batch
#' @param num_workers number of processes to handle the data
#' @param device  specify to run on cpu or gpu
#' @param checkpoint_path path to checkpoint file
#' @param checkpoint_frequency write results to checkpoint file every N images

#' @param 

#'
#' @return list of dictionaries of MegaDetector detections
#' @export
#'
#' @examples
#' \dontrun{mdres <- detectMD_batch(md_py, allframes$Frame)}
detect <- function(detector, image_file_names, resize_width, resize_height,
                   letterbox=TRUE, confidence_threshold=0.1, file_col='frame',
                   batch_size=1, num_workers=1, device=NULL,
                   checkpoint_path=NULL, checkpoint_frequency=-1){

  if(reticulate::py_module_available("animl")){
    animl_py <- reticulate::import("animl")
  }
  else{ stop('animl-py environment must be loaded first via reticulate')}

  animl_py$detect(detector, image_file_names, as.integer(resize_width), as.integer(resize_height),
                           letterbox=letterbox, confidence_threshold=confidence_threshold,
                           file_col=file_col, batch_size=as.integer(batch_size), 
                           num_workers=as.integer(num_workers), device=device,
                           checkpoint_path=checkpoint_path, 
                           checkpoint_frequency=as.integer(checkpoint_frequency))
}


#' parse MD results into a simple dataframe
#'
#' @param results json output from megadetector
#' @param manifest dataframe containing all frames
#' @param out_file path to save dataframe
#' @param buffer percentage buffer to move bbox away from image edge
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
parse_detections <- function(results, manifest = NULL, out_file = NULL, buffer=0.02, threshold=0, file_col="frame") {
  if (check_file(out_file)) { return(load_data(out_file))}
  
  if (!is(results, "list")) { stop("MD results input must be list") }
    
  else{
    f <- function(data) {
      if (!("detections" %in% names(data))){
          #print('File error ', data$file)
          return()
        }
      if (length(data$detections) > 0) {
          x <- data.frame()
          for(detection in data$detections){
            if (detection$conf > threshold){
                x <- rbind(x, data.frame(file=data$file,
                                         max_detection_conf= data$max_detection_conf,
                                         category = detection$category, conf = detection$conf, 
                                         bbox_x = detection$bbox_x, bbox_y = detection$bbox_y, 
                                         bbox_w = detection$bbox_w, bbox_h = detection$bbox_h, 
                                         stringsAsFactors = F))
            }
          }
          return(x)
      } 
      else {
        return(data.frame(file = data$file, max_detection_conf=data$max_detection_conf,
                          category = 0, conf = NA, 
                          bbox_x = NA, bbox_y = NA, 
                          bbox_w = NA, bbox_h = NA, 
                          stringsAsFactors = F))
      }
    }
    df <- do.call(rbind.data.frame, sapply(results, f, simplify = F))
    
    df$bbox_x <- sapply(df$bbox_x, function(x) min(max(x, buffer), 1 - buffer))
    df$bbox_y <- sapply(df$bbox_y, function(x) min(max(x, buffer), 1 - buffer))
    df$bbox_w <- sapply(df$bbox_w, function(x) min(max(x, buffer), 1 - buffer))
    df$bbox_h <- sapply(df$bbox_h, function(x) min(max(x, buffer), 1 - buffer))
    
    # merge to manifest if given
    if (!is.null(manifest)) { df <- merge(manifest, df, by.x=file_col, by.y="file") } 

    # Save file
    if (!is.null(out_file)) { save_data(df, out_file)}

    return(df) 
  }
}

