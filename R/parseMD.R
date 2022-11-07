#' parse MD JSON results file into a simple dataframe
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
#' mdresults <- parseMDsimple(mdres)
#' }
parseMD <- function(mdresults, manifest = NULL, outfile = NULL) {
  if (checkFile(outfile)) { return(loadData(outfile))}
  
  if (is(mdresults, "list")) { 
    
  f <- function(data) {
    if (nrow(data$detections) > 0) {
      data.frame(Frame = data$file, MaxMDConfidence = data$max_detection_conf, 
                 MaxCategory = data$max_detection_category, data$detections, stringsAsFactors = F)
    } 
    else {
      data.frame(Frame = data$file, MaxMDConfidence = data$max_detection_conf, 
                 MaxCategory = 0, category = 0, conf = NA, 
                 bbox1 = NA, bbox2 = NA, bbox3 = NA, bbox4 = NA, stringsAsFactors = F)
    }
  }
  
  results <- do.call(rbind.data.frame, sapply(mdresults, f, simplify = F))
  
  # merge to dataframe if given
  if (!is.null(manifest)) { results <- merge(manifest, results) } 
  
  # Save file
  if (!is.null(outfile)) { saveData(results, outfile)}
  
  results 
  }
  
  # JSON 
  else if (is(mdresults,"json")) {
    results <- input[[1]]
    delete <- numeric()
    opb <- pbapply::pboptions(char = "=")
    pb <- pbapply::startpb(1, length(results)) 
    for (n in 1:length(results)) {
      if (!is.null(results[[n]][["failure"]])) {
        delete <- c(delete, n)
      } else {
        if (length(results[[n]]$detections) > 0) {
          detections <- data.frame(category = character(), conf = numeric(), bbox1 = numeric(), bbox2 = numeric(), bbox3 = numeric(), bbox4 = numeric(), stringsAsFactors = F)
          for (m in 1:length(results[[n]]$detections)) {
            detections <- rbind(detections, data.frame(
              category = as.character(results[[n]]$detections[[m]]$category),
              conf = results[[n]]$detections[[m]]$conf,
              bbox1 = results[[n]]$detections[[m]]$bbox[[1]],
              bbox2 = results[[n]]$detections[[m]]$bbox[[2]],
              bbox3 = results[[n]]$detections[[m]]$bbox[[3]],
              bbox4 = results[[n]]$detections[[m]]$bbox[[4]], stringsAsFactors = F
            ))
          }
          results[[n]]$MaxCategory <- detections$category[which(detections$conf == max(detections$conf))][1]
          results[[n]]$detections <- detections
        } else {
          results[[n]]$MaxCategory <- "0"
        }
        results[[n]] <- results[[n]][c(1, 2, 4, 3)]
      }
      if ((n %% round(length(results) / 100, 0)) == 0) 
        pbapply::setpb(pb, n) 
    }
    pbapply::setpb(pb, length(results))
    pbapply::closepb(pb)
    if (length(delete) > 0) {
      results[-delete]
    } else {
      results
    }

  }
  else { stop("MD results input must be list or json ")}
}
