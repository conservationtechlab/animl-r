# matchBox <- function(first,second,buffer = 0.001){
#   #box is close enough
#   if((abs(first$bbox1 - second$bbox1) <= buffer) && (abs(first$bbox2 - second$bbox2) <= buffer) &&
#      (abs(first$bbox3 - second$bbox3) <= buffer) && (abs(first$bbox4 - second$bbox4) <= buffer)){
#
#     return(TRUE)
#   }
#   # not close enough
#   else{return(FALSE)}
# }
#
#
#
# checkMDResults <- function(mdresults){
#   cleaned <- list()
#   steps<-nrow(mdresults)
#   pb <-pbapply::startpb(1,steps)
#
#   for(i in 1:(steps-1)){
#     first <- mdresults[i,]
#
#     if(is.na(first$bbox1)){next}
#
#     count = 1
#     for(j in i:steps){
#       if(j %in% cleaned){next}
#       second <- mdresults[j,]
#       if(is.na(second$bbox1)){next}
#       match <- matchBox(first,second)
#       if(match){
#         #same box same image
#         if(first$Frame == second$Frame){
#           cleaned
#         }
#         #same box different image
#         else{
#           count <- count + 1
#           if(count > 3){
#             cleaned <- c(cleaned,j)
#           }
#         }
#       }
#       else{next}
#     }
#     pbapply::setpb(pb,i)
#   }
#   pbapply::setpb(pb,steps)
#   pbapply::closepb(pb)
#   cleaned
# }
# boxes <- mdresflat[mdresflat$md_class != 0,]
# boxes <- boxes[1]
#
# test <- checkMDResults(boxes[1:1000,])
