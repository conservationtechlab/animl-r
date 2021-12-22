#' parse MD JSON results file
#'
#' @param json md output file
#'
#' @return list of MD detections
#' @export
#'
#' @examples
#'
#' mdres <- parseMDjson(mdresults.json)
parseMDjson<-function(json){
  results<-json[[1]]
  delete<-numeric()
  opb<-pboptions(char = "=")
  pb <-startpb(1,length(results)) #txtProgressBar(min = 0, max = length(results), style = 3)
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
    if((n %% round(length(results)/100,0)) ==0) setpb(pb,n)#setTxtProgressBar(pb, n)
  }
  setpb(pb,length(results))
  closepb(pb)
  if(length(delete)>0)
    results[-delete]
  else
    results
}
