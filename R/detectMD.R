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
  if (!("mdsession" %in% class(mdsession)) && !("mdmodel" %in% class(mdsession))) stop("Expecting a mdsession or mdmodel object.")
  if(("mdsession" %in% class(mdsession))){
    type<-"mdsession"
  }else{
    type<-"mdmodel"
  }
  if(!file.exists(imagefile))stop("Image files does not exist.")
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
    if(type=="mdsession"){
      # get tensors
      image_tensor=mdsession$graph$get_tensor_by_name('x:0')
      output_tensor = mdsession$graph$get_tensor_by_name('Identity:0')
    }else{
      infer<-mdsession$signatures["serving_default"]
    }
    
    np <- reticulate::import("numpy")
    
    if(type=="mdsession"){
      res<-mdsession$run(list(output_tensor),feed_dict=list("x:0"=tf$reshape(img[[1]],as.integer(c(1,1280,1280,3)))$numpy()))
      res<-tf$cast(res[[1]],tf$float32)
    }else{
      res<-infer(tf$reshape(img[[1]],as.integer(c(1,1280,1280,3))))
      res<-res[[1]]
    }
    
    
    scores<-(as.array(res[,,6:8])*as.array(res)[,,c(5,5,5),drop=F])
    resfilter<-tensorflow::tf$image$combined_non_max_suppression(tf$reshape(res[,,1:4],as.integer(c(dim(res)[1],dim(res)[2],1,4))),scores,max_output_size_per_class=as.integer(100),
                                                                 max_total_size=as.integer(100),score_threshold=min_conf,clip_boxes=TRUE)
    #images[(i * batch_size - batch_size)+1]                                                                                                                                
    lapply(1:length(resfilter$valid_detections),processYOLO5,resfilter$nmsed_boxes,
           resfilter$nmsed_classes,resfilter$nmsed_scores,resfilter$valid_detections,img)[[1]]
    
  }
}
  
# MAKE FLEXIBLE

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
  if (!("mdsession" %in% class(mdsession)) && !("mdmodel" %in% class(mdsession))) stop("Expecting a mdsession or mdmodel object.")
  if(("mdsession" %in% class(mdsession))){
    type<-"mdsession"
  }else{
    type<-"mdmodel"
  }
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
    starttime<-Sys.time()
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
    #if(batch_size>1)print("Megadetector based on Yolo5 currently only supports a batch size of 1")
    #batch_size=1 
    dataset <- ImageGeneratorSize(images,resize_height=1280,resize_width=1280, pad=TRUE, standardize = TRUE, batch_size = batch_size)
    
    if(type=="mdsession"){
      # get tensors
      image_tensor=mdsession$graph$get_tensor_by_name('x:0')
      output_tensor = mdsession$graph$get_tensor_by_name('Identity:0')
    }else{
      infer<-mdsession$signatures["serving_default"]
    }
    
    steps <- ceiling(length(images) / batch_size)
    opb <- pbapply::pboptions(char = "=")
    pb <- pbapply::startpb(1, steps) # txtProgressBar(min = 0, max = length(results), style = 3)
    starttime<-Sys.time()
    # process all images
    for (i in 1:steps) {
      # catch errors due to empty or corrupted images
      if (!inherits(try(img <- dataset$get_next(), silent = T), "try-error")) {
        if(type=="mdsession"){
          resbatch<-mdsession$run(list(output_tensor),feed_dict=list("x:0"=img[[1]]$numpy()))
          resbatch<-tf$cast(resbatch[[1]],tf$float32)
        }else{
          resbatch<-infer(img[[1]])
          resbatch<-resbatch[[1]]
        }
        
        scores<-(as.array(resbatch[,,6:8])*as.array(resbatch)[,,c(5,5,5),drop=F])
        resfilter<-tensorflow::tf$image$combined_non_max_suppression(tf$reshape(resbatch[,,1:4],as.integer(c(dim(resbatch)[1],dim(resbatch)[2],1,4))),scores,max_output_size_per_class=as.integer(100),
                                                                     max_total_size=as.integer(100),score_threshold=min_conf,clip_boxes=TRUE)
        #images[(i * batch_size - batch_size)+1]                                                                                                                                
        results<-c(results,lapply(1:length(resfilter$valid_detections),processYOLO5,resfilter$nmsed_boxes,
                                  resfilter$nmsed_classes,resfilter$nmsed_scores,resfilter$valid_detections,img))
        
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
  cat(paste(length(images),"images processed.",round(length(images)/(as.numeric(Sys.time())-as.numeric(starttime)),1),"images/s\n"))
  results
}


# HIDE?

#' Process YOLO5 output and convert to MD format
#'
#' Returns a list with the standard MD output format. Used for batch processing
#'
#'
#' @param n index for the record in the batch
#' @param boxes array of boxes returned by combined_non_max_suppression
#' @param classes vector of classes returned by combined_non_max_suppression
#' @param scores vector of probabilities returned by combined_non_max_suppression
#' @param selection vector of number of detected boxes returned by combined_non_max_suppression
#' @param batch batch used to detect objects
#'
#' @return a list of MD bounding boxes, classes, and confidence for the image
#' @export
#'
processYOLO5<-function(n,boxes,classes,scores,selection,batch){
  boxes<-as.array(boxes)
  classes<-as.array(classes)
  scores<-as.array(scores)

  img_width<-batch[[2]]$numpy()[n]
  img_height<-batch[[3]]$numpy()[n]

  a1<-as.vector((img_height-img_width)/img_height)
  a2<-as.vector((img_width-img_height)/img_width)

  if(as.vector(selection)[n]>0){
    filter<-c(1:(as.vector(selection)[n]))

    if(img_width>img_height){
      list(FilePath =as.character(batch[[4]][n]), max_detection_conf = max(scores[n,filter],0),
           max_detection_category = classes[n,1]+1,
           detections = data.frame(
             category = classes[n,filter]+1, conf = scores[n,filter],
             bbox1 = pmin(pmax(boxes[n,filter,1]-boxes[n,filter,3]/2,0),1),
             bbox2 = pmin(pmax(((boxes[n,filter,2]-boxes[n,filter,4]/2)-a2/2)/(1-a2),0),1),
             bbox3 = pmin(pmax(boxes[n,filter,3],0.01),1),
             bbox4 = pmin(pmax(boxes[n,filter,4]/(1-a2),0.01),1)
           )
      )
    }else{
      list(FilePath = as.character(batch[[4]][n]), max_detection_conf = max(scores[n,filter],0),
           max_detection_category = classes[n,1]+1,
           detections = data.frame(
             category = classes[n,filter]+1, conf = scores[n,filter],
             bbox1 = pmin(pmax(((boxes[n,filter,1]-boxes[n,filter,3]/2)-a1/2)/(1-a1),0),1),
             bbox2 = pmin(pmax((boxes[n,filter,2]-boxes[n,filter,4]/2),0),1),
             bbox3 = pmin(pmax(boxes[n,filter,3]/(1-a1),0.01),1),
             bbox4 = pmin(pmax(boxes[n,filter,4],0.01),1)
           )
      )
    }
  }else{
    list(FilePath = as.character(batch[[4]][n]), max_detection_conf = max(scores[n,],0),
         max_detection_category = 0,
         detections = data.frame(
           category = integer(), conf = numeric(),
           bbox1 = numeric(),
           bbox2 = numeric(),
           bbox3 = numeric(),
           bbox4 = numeric())
    )
  }
}