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
#' 
#' # ADD THRESHOLD FILTER 0.1
parseMD <- function(mdresults, manifest = NULL, outfile = NULL) {
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
    
    return(results) 
  }
}


#############################################
#' converte the JSON file produced bye the
#' Python version of MegaDetector into the format
#' produced by detectObjectBatch
#'
#' @param json json data in a list format
#' 
#' @return a list of MegaDetector results
#'
#' @examples
#' \dontrun{
#' mdresults <- parseMDjson(json)
#' }
#' 
#' #SCRAP
parseMDjson<-function(json){
  results<-json[[1]]
  delete<-numeric()
  opb<-pbapply::pboptions(char = "=")
  pb <-pbapply::startpb(1,length(results)) #txtProgressBar(min = 0, max = length(results), style = 3)
  for(n in 1:length(results)){
    if(!is.null(results[[n]][['failure']])){
      delete<-c(delete,n)
    }else{
      if(length(results[[n]]$detections)>0){
        detections<-data.frame(category=character(),conf=numeric(),bbox1=numeric(),bbox2=numeric(),bbox3=numeric(),bbox4=numeric(),stringsAsFactors = F)
        for(m in 1:length(results[[n]]$detections)){
          detections<-rbind(detections,data.frame(category=as.character(results[[n]]$detections[[m]]$category),
                                                  conf=results[[n]]$detections[[m]]$conf,
                                                  bbox1=results[[n]]$detections[[m]]$bbox[[1]],
                                                  bbox2=results[[n]]$detections[[m]]$bbox[[2]],
                                                  bbox3=results[[n]]$detections[[m]]$bbox[[3]],
                                                  bbox4=results[[n]]$detections[[m]]$bbox[[4]],stringsAsFactors=F))
        }
        results[[n]]$max_detection_category<-detections$category[which(detections$conf==max(detections$conf))][1]
        results[[n]]$detections<-detections
      }else{
        results[[n]]$max_detection_category<-"0"
      }
      results[[n]]<-results[[n]][c(1,2,4,3)]
    }
    if((n %% round(length(results)/100,0)) ==0) pbapply::setpb(pb,n)#setTxtProgressBar(pb, n)
  }
  pbapply::setpb(pb,length(results))
  pbapply::closepb(pb)
  if(length(delete)>0)
    results[-delete] 
  else
    results
}
