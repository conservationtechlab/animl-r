#' parse MD results into a simple dataframe
#'
#' @param manifest dataframe containing all frames
#' @param mdresults raw MegaDetector output
#' @param outfile file path to save dataframe to
#' 
#' @return original dataframe including md results
#' @export
#'
#' @examples
#' \dontrun{
#' mdresults <- parseMD(mdres)
#' }
parseMD <- function(mdresults, manifest = NULL, outfile = NULL) {
  if (checkFile(outfile)) { return(loadData(outfile))}
  
  if (!is(mdresults, "list")) { stop("MD results input must be list") }
    
  else{
    f <- function(data) {
      if (nrow(data$detections) > 0) {
        data.frame(file = data$file, max_detection_conf = data$max_detection_conf, 
                   max_detection_category = data$max_detection_category, data$detections, stringsAsFactors = F)
      } 
      else {
        data.frame(file = data$file, max_detection_conf = data$max_detection_conf, 
                   max_detection_category = 0, category = 0, conf = NA, 
                   bbox1 = NA, bbox2 = NA, bbox3 = NA, bbox4 = NA, stringsAsFactors = F)
      }
    }
    
    results <- do.call(rbind.data.frame, sapply(mdresults, f, simplify = F))
    
    results$bbox1[results$bbox1 > 0.98] <- 0.98
    results$bbox2[results$bbox2 > 0.98] <- 0.98
    results$bbox3[results$bbox3 > 0.98] <- 0.98
    results$bbox4[results$bbox4 > 0.98] <- 0.98
    
    results$bbox1[results$bbox1 < 0.02] <- 0.02
    results$bbox2[results$bbox2 < 0.02] <- 0.02
    results$bbox3[results$bbox3 < 0.02] <- 0.02
    results$bbox4[results$bbox4 < 0.02] <- 0.02
    
    # merge to dataframe if given
    if (!is.null(manifest)) { results <- merge(manifest, results, by.x="Frame",by.y="file") } 
    
    # Save file
    if (!is.null(outfile)) { saveData(results, outfile)}
    
    results 
  }
}
