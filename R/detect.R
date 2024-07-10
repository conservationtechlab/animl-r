#' Load MegaDetector
#'
#' @param model_path path to MegaDetector model (v5)
#'
#' @return
#' @export
#'
#' @examples
megadetector <- function(model_path){
  # first check if animl-py is loaded
  if(py_module_available("animl")){
    animl_py <- import("animl")
  }
  else{
    stop('animl-py environment must be loaded first via reticulate')
  }
  animl_py$megadetector$MegaDetector("/mnt/machinelearning/megaDetector/md_v5a.0.0.pt")
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
#' @return
#' @export
#'
#' @examples
detectMD_batch <- function(detector, image_file_names, checkpoint_path=NULL, checkpoint_frequency=-1,
                          confidence_threshold=0.1, quiet=TRUE, image_size=NULL, file_col='Frame'){
  if(py_module_available("animl")){
    animl_py <- import("animl")
  }
  else{
    stop('animl-py environment must be loaded first via reticulate')
  }
  animl_py$detect_MD_batch(detector, image_file_names, checkpoint_path, checkpoint_frequency,
                           confidence_threshold, quiet, image_size, file_col)
}


#' parse MD results into a simple dataframe
#'
#' @param manifest dataframe containing all frames
#' @param mdresults raw MegaDetector output
#' @param outfile file path to save dataframe to
#' @param buffer percentage buffer to move bbox away from image edge
#' 
#' @return original dataframe including md results
#' @export
#'
#' @examples
#' \dontrun{
#' mdresults <- parseMD(mdres)
#' }
parseMD <- function(mdresults, manifest = NULL, outfile = NULL, buffer=0.02) {
  if (checkFile(outfile)) { return(loadData(outfile))}
  
  if (!is(mdresults, "list")) { stop("MD results input must be list") }
    
  else{
    f <- function(data) {
      if (length(data$detections) > 0) {
          x <- data.frame()
          for(i in data$detections){
            x <- rbind(x, data.frame(file=data$file, category=i$category, conf=i$conf, 
                                     bbox1 = i$bbox1, bbox2=i$bbox2, bbox3=i$bbox3, bbox4=i$bbox4, stringsAsFactors = F))
          }
          return(x)
      } 
      else {
        return(data.frame(file = data$file, category = 0, conf = NA, 
                   bbox1 = NA, bbox2 = NA, bbox3 = NA, bbox4 = NA, stringsAsFactors = F))
        
      }
    }
    
    results <- do.call(rbind.data.frame, sapply(mdresults, f, simplify = F))
    results$bbox1[results$bbox1 > 1-buffer] <- 1-buffer
    results$bbox2[results$bbox2 > 1-buffer] <- 1-buffer
    results$bbox3[results$bbox3 > 1-buffer] <- 1-buffer
    results$bbox4[results$bbox4 > 1-buffer] <- 1-buffer
    
    results$bbox1[results$bbox1 < buffer] <- buffer
    results$bbox2[results$bbox2 < buffer] <- buffer
    results$bbox3[results$bbox3 < buffer] <- buffer
    results$bbox4[results$bbox4 < buffer] <- buffer
    
    # merge to dataframe if given
    if (!is.null(manifest)) { results <- merge(manifest, results, by.x="Frame",by.y="file") } 
    
    # Save file
    if (!is.null(outfile)) { saveData(results, outfile)}
    
    return(results) 
  }
}
