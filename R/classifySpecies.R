#' Classifies Crops Using Specified Models
#'
#' @param mdresults flattened mdresults dataframe
#' @param model models with which to classify species
#' @param resize resize images before classification, defaults to 299x299px
#' @param standardize standardize images, defaults to FALSE
#' @param batch_size number of images processed in each batch (keep small)
#' @param workers number of cores
#'
#' @return a matrix of likelihoods for each class for each image
#' @export
#'
#' @examples
#' \dontrun{
#' pred<-classifySpecies(imagesallanimal,paste0(modelfile,".h5"),
#'                       resize=456,standardize=FALSE,batch_size = 64,workers=8)
#' #' }
#' classifySpecies<-function(mdresults,model,resize=299,standardize=TRUE,batch_size=32,workers=1){
#'   filecol<-which(colnames(mdresults) %in% c("file","Frame"))[1]
#'   model<-keras::load_model_hdf5(model)
#'   predict_steps=ceiling(nrow(mdresults)/batch_size)
#'   cropGenerator<-generateCropsFromFile(x=mdresults[,filecol],boxes=as.matrix(mdresults[,c("bbox1","bbox2","bbox3","bbox4")]),
#'                                        resize=resize,standardize=standardize,batch_size=batch_size)
#'
#'   pred <- model %>% keras::predict_generator(generator=cropGenerator,steps=predict_steps,workers=workers,verbose=1)
#'
#' }

# classifySpecies<-function(mdresults,models,resize=299,standardize=TRUE,batch_size=32,workers=1){
#   source_python("ImageCropGenerator.py")
#   ### Muti-model
#   filecol<-which(colnames(mdresults) %in% c("file","FilePath"))[1]
#   n.models<-length(models)
#   predlist<-list()
#   predict_steps=ceiling(nrow(mdresults)/batch_size)
#   cropGenerator<-GenerateCropsFromFile(x=mdresults[,filecol],boxes=as.matrix(mdresults[,c("bbox1","bbox2","bbox3","bbox4")]),resize=resize,standardize=standardize,batch_size=batch_size)
#
#   for(m in 1:n.models){
#     cat("Loading model",m,"/",n.models,"\n")
#     model<-load_model_hdf5(models[m])
#     predlist[[m]]<-model %>% predict_generator(generator=cropGenerator,steps=predict_steps,workers=workers,verbose=1)
#   }
#
#   predarray<-array(unlist(predlist),c(dim(predlist[[1]]),n.models))
#   predarray<-aperm(predarray,c(3,1,2))
#   apply(predarray,c(2,3),mean)
# }
