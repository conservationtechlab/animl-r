#' parse MD JSON results file
#'
#' @param json md output file
#'
#' @return list of MD detections
#' @export
#'
#' @examples
#' \dontrun{
#' mdres <- parseMDjson("MDresults.json")
#' }
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

#' parse MD JSON results file into a simple dataframe
#'
#' @param mdresults raw MegaDetector output
#'
#' @return flattened dataframe of results
#' @export
#'
#' @examples
#' \dontrun{
#' mdresults <- parseMDsimple(mdres)
#' }
parseMDsimple<-function(mdresults){
  f<-function(data){
    if(nrow(data$detections)>0){
      data.frame(Frame=data$FilePath,max_detection_conf=data$max_detection_conf,max_detection_category=data$max_detection_category,data$detections,stringsAsFactors = F)
    }else{

      data.frame(Frame=data$FilePath,max_detection_conf=data$max_detection_conf,max_detection_category=data$max_detection_category,category=0,conf=NA,bbox1=NA,bbox2=NA,bbox3=NA,bbox4=NA,stringsAsFactors = F)}
  }
  results<-do.call(rbind.data.frame,sapply(mdresults,f,simplify = F))
  colnames(results)[4:5]<-c("md_class","md_confidence")
  results
}
