#' Run MD on a single image
#'
#' Returns the MD bounding boxes, classes, confidence above the min_conf
#' threshold for a single image. #' Requires a an mdsession is already
#' loaded (see loadMDModel() ) and the file path of the image in question.
#'
#'
#' @param mdsession should be the output from loadMDmodel(model)
#' @param imagefile The path for the image in question
#' @param min_conf Confidence threshold for returning bounding boxes, defaults to 0.1
#'
#' @return a list of MD bounding boxes, classes, and confidence for the image
#' @export
#'
#' @examples
#' \dontrun{
#'  images <- read_exif(imagedir, 
#'                      tags = c("filename", "directory", "FileModifyDate"), 
#'                      recursive = TRUE)
#'  colnames(images)[1] <- "FilePath"
#'  mdsession <- loadMDModel(mdmodel)
#'  mdres <- classifyImageMD(mdsession, images$FilePath[1])
#' }
detectObject <- function(mdsession, imagefile,mdversion=5 , min_conf = 0.1) {
  if (!("mdsession" %in% class(mdsession))) stop("Expecting a mdsession object.")
  if(mdversion<=4){
    img <- loadImage(imagefile, FALSE)
    # get tensors
    image_tensor <- mdsession$graph$get_tensor_by_name("image_tensor:0")
    box_tensor <- mdsession$graph$get_tensor_by_name("detection_boxes:0")
    score_tensor <- mdsession$graph$get_tensor_by_name("detection_scores:0")
    class_tensor <- mdsession$graph$get_tensor_by_name("detection_classes:0")
    
    np <- reticulate::import("numpy")
    res <- mdsession$run(list(box_tensor, score_tensor, class_tensor), feed_dict = list("image_tensor:0" = np$expand_dims(img, axis = F)))
    resfilter <- which(res[[2]] >= min_conf)
    list(
      FilePath = imagefile, max_detection_conf = max(res[[2]]), max_detection_category = res[[3]][which(res[[2]] == max(res[[2]]))][1],
      detections = data.frame(
        category = res[[3]][resfilter], conf = res[[2]][resfilter],
        bbox1 = res[[1]][1, resfilter, 2], bbox2 = res[[1]][1, resfilter, 1],
        bbox3 = res[[1]][1, resfilter, 4] - res[[1]][1, resfilter, 2],
        bbox4 = res[[1]][1, resfilter, 3] - res[[1]][1, resfilter, 1]
      )
    )
  }else{
    img <- loadImage_Resize_Size(imagefile,height=1280,width=1280,pad=TRUE,standardize=TRUE)
    # get tensors
    image_tensor=mdsession$graph$get_tensor_by_name('x:0')
    output_tensor = mdsession$graph$get_tensor_by_name('Identity:0')
    
    np <- reticulate::import("numpy")
    res<-mdsession$run(list(output_tensor),feed_dict=list("x:0"=np$expand_dims(img[[1]], axis = F)))
    res<-res[[1]][1,,]
    
    resfilter<-tf$image$non_max_suppression(res[,1:4],res[,5],as.integer(100),score_threshold=min_conf)
    resfilter<-as.matrix(resfilter)[,1]
    
    img_width<-img[[2]]$numpy()
    img_height<-img[[3]]$numpy()
    
    a1<-as.vector((img_height-img_width)/img_height)
    a2<-(img_width-img_height)/img_width
    
    if(img_width>img_height){
      list(FilePath = imagefile, max_detection_conf = max(res[,5]),
           max_detection_category = which(apply(res[,6:8,drop=FALSE],2,max)==max(res[,6:8])),
           detections = data.frame(
             category = apply(res[resfilter,6:8,drop=FALSE],1,which.max), conf = apply(res[resfilter,6:8,drop=FALSE],1,max),
             bbox1 = as.vector(res[resfilter,1])-as.vector(res[resfilter,3])/2,
             bbox2 = ((as.vector(res[resfilter,2])-as.vector(res[resfilter,4])/2)-a2/2)/(1-a2),
             bbox3 = as.vector(res[resfilter,3]),
             bbox4 = as.vector(res[resfilter,4])/(1-a2)
           )
      )
    }else{
      list(FilePath = imagefile, max_detection_conf = max(res[,5]),
           max_detection_category = which(apply(res[,6:8,drop=FALSE],2,max)==max(res[,6:8])),
           detections = data.frame(
             category = apply(res[resfilter,6:8,drop=FALSE],1,which.max), conf = apply(res[resfilter,6:8,drop=FALSE],1,max),
             bbox1 = ((as.vector(res[resfilter,1])-as.vector(res[resfilter,3])/2)-a1/2)/(1-a1),
             bbox2 = (as.vector(res[resfilter,2])-as.vector(res[resfilter,4])/2),
             bbox3 = as.vector(res[resfilter,3])/(1-a1),
             bbox4 = as.vector(res[resfilter,4])
           )
      )
    } 
    
  }
}
  


#' Run MegaDetector on a batch of images
#'
#' Runs MD on a list of image filepaths.
#' Can resume for a results file and will checkpoint the results after a set
#' number of images
#'
#' @param mdsession should be the output from loadMDmodel(model)
#' @param images list of image filepaths
#' @param min_conf Confidence threshold for returning bounding boxes, defaults to 0.1
#' @param batch_size Process images in batches, defaults to 1
#' @param resultsfile File containing previously checkpointed results
#' @param checkpoint Bank results after processing a number of images, defaults to 5000
#'
#' @return a list of lists of bounding boxes for each image
#' @export
#'
#' @examples
#' \dontrun{
#' images <- read_exif(imagedir,
#'   tags = c("filename", "directory", "DateTimeOriginal", "FileModifyDate"),
#'   recursive = TRUE
#' )
#' colnames(images)[1] <- "FilePath"
#' mdsession <- loadMDModel(mdmodel)
#' mdres <- classifyImagesBatchMD(mdsession, images$FilePath,
#'   resultsfile = mdresultsfile, checkpoint = 2500
#' )
#' }
detectObjectBatch <- function(mdsession, images,mdversion=5, min_conf = 0.1, batch_size = 1, resultsfile = NULL, checkpoint = 5000) {
  if (!("mdsession" %in% class(mdsession))) stop("Expecting a mdsession object.")
  if (!is.null(resultsfile)) {
    if (!dir.exists(dirname(resultsfile))) stop("Results file directory does not exist.\n")
    if (tolower(substr(resultsfile, nchar(resultsfile) - 5, nchar(resultsfile))) != ".rdata") {
      resultsfile <- paste0(resultsfile, ".RData")
    }
    
    # if results file exists prompt user to load it and resume
    if (!is.null(resultsfile) & file.exists(resultsfile)) {
      if (tolower(readline(prompt = "Results file exists, would you like to resume? y/n: ")) == "y") {
        load(resultsfile)
        images <- images[!(images %in% sapply(results, function(x) x$file))]
        cat(length(results), "records loaded.\n")
      } else {
        results <- list()
      }
    } else {
      results <- list()
    }
  } else {
    results <- list()
  }
  
  if(mdversion<=4){
    # create data generator
    dataset <- ImageGenerator(images, standardize = FALSE, batch_size = batch_size)
    
    # get tensors
    image_tensor <- mdsession$graph$get_tensor_by_name("image_tensor:0")
    box_tensor <- mdsession$graph$get_tensor_by_name("detection_boxes:0")
    score_tensor <- mdsession$graph$get_tensor_by_name("detection_scores:0")
    class_tensor <- mdsession$graph$get_tensor_by_name("detection_classes:0")
    
    steps <- ceiling(length(images) / batch_size)
    opb <- pbapply::pboptions(char = "=")
    pb <- pbapply::startpb(1, steps) # txtProgressBar(min = 0, max = length(results), style = 3)
    # process all images
    for (i in 1:steps) {
      # catch errors due to empty or corrupted images
      if (!inherits(try(img <- dataset$get_next(), silent = T), "try-error")) {
        res <- mdsession$run(list(box_tensor, score_tensor, class_tensor), feed_dict = list("image_tensor:0" = img$numpy()))
        for (l in 1:dim(res[[1]])[1]) {
          resfilter <- which(res[[2]] >= min_conf)
          results[[length(results) + 1]] <- list(
            FilePath = images[(i * batch_size - batch_size) + l], max_detection_conf = max(res[[2]][l, ]),
            max_detection_category = res[[3]][which(res[[2]][l, ] == max(res[[2]][l, ]))][1],
            detections = data.frame(
              category = res[[3]][l, resfilter], conf = res[[2]][l, resfilter],
              bbox1 = res[[1]][l, resfilter, 2],
              bbox2 = res[[1]][l, resfilter, 1],
              bbox3 = res[[1]][l, resfilter, 4] - res[[1]][l, resfilter, 2],
              bbox4 = res[[1]][l, resfilter, 3] - res[[1]][l, resfilter, 1]
            )
          )
        }
      }
      # save intermediate results at given checkpoint interval
      if (!is.null(resultsfile) & (i %% checkpoint / batch_size) == 0) {
        save(results, file = resultsfile)
      }
      pbapply::setpb(pb, i)
    }
    pbapply::setpb(pb, steps)
    pbapply::closepb(pb)
  }else{
    # create data generator
    if(batch_size>1)print("Megadetector based on Yolo5 currently only supports a batch size of 1")
    batch_size=1 
    dataset <- ImageGeneratorSize(images,resize_height=1280,resize_width=1280, pad=TRUE, standardize = TRUE, batch_size = batch_size)
    
    # get tensors
    image_tensor=mdsession$graph$get_tensor_by_name('x:0')
    output_tensor = mdsession$graph$get_tensor_by_name('Identity:0')
    
    steps <- ceiling(length(images) / batch_size)
    opb <- pbapply::pboptions(char = "=")
    pb <- pbapply::startpb(1, steps) # txtProgressBar(min = 0, max = length(results), style = 3)
    # process all images
    for (i in 1:steps) {
      # catch errors due to empty or corrupted images
      if (!inherits(try(img <- dataset$get_next(), silent = T), "try-error")) {
        res<-mdsession$run(list(output_tensor),feed_dict=list("x:0"=img[[1]]$numpy()))
        res<-res[[1]][1,,]
        
        resfilter<-tf$image$non_max_suppression(res[,1:4],res[,5],as.integer(100),score_threshold=min_conf)
        resfilter<-as.matrix(resfilter)[,1]
        
        img_width<-img[[2]]$numpy()
        img_height<-img[[3]]$numpy()
        
        a1<-as.vector((img_height-img_width)/img_height)
        a2<-(img_width-img_height)/img_width
        
        if(img_width>img_height){
          results[[length(results) + 1]] <- list(
            FilePath = images[(i * batch_size - batch_size)+1], max_detection_conf = max(res[,5]),
            max_detection_category = which(apply(res[,6:8,drop=FALSE],2,max)==max(res[,6:8])),
            detections = data.frame(
              category = apply(res[resfilter,6:8,drop=FALSE],1,which.max), conf = apply(res[resfilter,6:8,drop=FALSE],1,max),
              bbox1 = as.vector(res[resfilter,1])-as.vector(res[resfilter,3])/2,
              bbox2 = ((as.vector(res[resfilter,2])-as.vector(res[resfilter,4])/2)-a2/2)/(1-a2),
              bbox3 = as.vector(res[resfilter,3]),
              bbox4 = as.vector(res[resfilter,4])/(1-a2)
            )
          )
        }else{
          results[[length(results) + 1]] <- list(
            FilePath = images[(i * batch_size - batch_size)+1], max_detection_conf = max(res[,5]),
            max_detection_category = which(apply(res[,6:8,drop=FALSE],2,max)==max(res[,6:8])),
            detections = data.frame(
              category = apply(res[resfilter,6:8,drop=FALSE],1,which.max), conf = apply(res[resfilter,6:8,drop=FALSE],1,max),
              bbox1 = ((as.vector(res[resfilter,1])-as.vector(res[resfilter,3])/2)-a1/2)/(1-a1),
              bbox2 = (as.vector(res[resfilter,2])-as.vector(res[resfilter,4])/2),
              bbox3 = as.vector(res[resfilter,3])/(1-a1),
              bbox4 = as.vector(res[resfilter,4])
            )
          )
        }
      }
      # save intermediate results at given checkpoint interval
      if (!is.null(resultsfile) & (i %% checkpoint / batch_size) == 0) {
        save(results, file = resultsfile)
      }
      pbapply::setpb(pb, i)
    }
    pbapply::setpb(pb, steps)
    pbapply::closepb(pb)
  }
  if(!is.null(resultsfile)){
    save(results, file = resultsfile)}
  results
}