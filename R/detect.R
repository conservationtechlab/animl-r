#' Load MegaDetector
#'
#' @param model_path path to MegaDetector model (v5)
#' @param device load model onto given device description
#'
#' @return megadetector object
#' @export
#'
#' @examples
#' \dontrun{md_py <- megadetector("/mnt/machinelearning/megaDetector/md_v5a.0.0.pt")}
megadetector <- function(model_path, device=NULL){
  # first check if animl-py is loaded
  if(reticulate::py_module_available("animl")){
    animl_py <- reticulate::import("animl")
  }
  else{
    stop('animl-py environment must be loaded first via reticulate')
  }
  animl_py$megadetector$MegaDetector(model_path, device=device)
}


#' Apply MegaDetector to a Given Batch of Images
#'
#' @param detector preloaded md model
#' @param image_file_names list of image filenames, a single image filename, or folder
#' @param checkpoint_path path to checkpoint file
#' @param checkpoint_frequency write results to checkpoint file every N images
#' @param confidence_threshold only detections above this threshold are returned
#' @param quiet print debugging statements when false, defaults to true
#' @param image_size overrides default image size, 1280
#' @param file_col select which column if image_file_names is a manifest
#'
#' @return list of dictionaries of MegaDetector detections
#' @export
#'
#' @examples
#' \dontrun{mdres <- detectMD_batch(md_py, allframes$Frame)}
detect_MD_batch <- function(detector, image_file_names, checkpoint_path=NULL, checkpoint_frequency=-1,
                          confidence_threshold=0.1, quiet=TRUE, image_size=NULL, file_col='Frame'){
  if(reticulate::py_module_available("animl")){
    animl_py <- reticulate::import("animl")
  }
  else{ stop('animl-py environment must be loaded first via reticulate')}

  animl_py$detect_MD_batch(detector, image_file_names, checkpoint_path=checkpoint_path, 
                           checkpoint_frequency=checkpoint_frequency,
                           confidence_threshold=confidence_threshold, quiet=quiet, 
                           image_size=image_size, file_col=toString(file_col))
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
parse_MD <- function(results, manifest = NULL, out_file = NULL, buffer=0.02, threshold=0, file_col="Frame") {
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
                                         bbox1 = detection$bbox1, bbox2 = detection$bbox2, 
                                         bbox3 = detection$bbox3, bbox4 = detection$bbox4, 
                                         stringsAsFactors = F))
            }
          }
          return(x)
      } 
      else {
        return(data.frame(file = data$file, max_detection_conf=data$max_detection_conf,
                          category = 0, conf = NA, 
                          bbox1 = NA, bbox2 = NA, 
                          bbox3 = NA, bbox4 = NA, 
                          stringsAsFactors = F))
      }
    }
    df <- do.call(rbind.data.frame, sapply(results, f, simplify = F))
    
    df$bbox1 <- sapply(df$bbox1, function(x) min(max(x, buffer), 1 - buffer))
    df$bbox2 <- sapply(df$bbox2, function(x) min(max(x, buffer), 1 - buffer))
    df$bbox3 <- sapply(df$bbox3, function(x) min(max(x, buffer), 1 - buffer))
    df$bbox4 <- sapply(df$bbox4, function(x) min(max(x, buffer), 1 - buffer))
    
    # merge to manifest if given
    if (!is.null(manifest)) { df <- merge(manifest, df, by.x=file_col, by.y="file") } 

    # Save file
    if (!is.null(out_file)) { save_data(df, out_file)}

    return(df) 
  }
}

