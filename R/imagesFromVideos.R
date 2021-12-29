#' Extract frames from video for classification
#'
#' This function can take
#'
#' @param videos dataframe of videos
#' @param outdir directory to save frames to
#' @param format output format for frames, defaults to jpg
#' @param fps frames per second, otherwise determine mathematically
#' @param frames number of frames to sample
#' @param parallel T/F toggle for parallel processing, defaults to FALSE
#' @param nproc number of processors to use if parallel, defaults to 1
#'
#' @return dataframe of still frames for each video
#' @export
imagesFromVideos<-function (videos, outdir = tempfile(), format = "jpg", fps = NULL,frames=NULL,parallel=F,nproc=1) {
  if(outdir!="" & !dir.exists(outdir)){
    if(!dir.create(outdir,recursive = T))
      stop("Output directory invalid.\n")}

  run.parallel<-function(x){
    vfilter <- ifelse(length(fps), paste0("fps=", fps),
                      "null")
    vfilter <- ifelse(length(frames), paste0("fps=", round(1/(av::av_media_info(x)$duration/frames),3)),
                      vfilter)
    framerate <- av::av_media_info(x)$video$framerate
    tempdir<-tempfile()
    dir.create(tempdir)
    name<-strsplit(basename(x),".",fixed=T)[[1]][1]
    codec <- switch(format, jpeg = "mjpeg", jpg = "mjpeg", format)
    #add a random number to avoid duplicate file names
    rnd<-sprintf("%05d", round(stats::runif(1,1,99999),0))
    output <- file.path(tempdir, paste0(name,"_",rnd,"_%5d.", format))
    #output <- file.path(tempdir, paste0(name,"_%5d.", format))
    av::av_encode_video(input = x, output = output, framerate = framerate,
                    codec = codec, vfilter = vfilter)
    files<-data.frame(videofile=x,tmpframe=list.files(tempdir, pattern = paste0(name,"_",rnd,"_\\d{5}.", format),
                                                      full.names = TRUE),stringsAsFactors = F)
    files$videoframe<-paste0(outdir,basename(files$tmpframe))
    file.rename(files$tmpframe,files$videoframe)
    files[,c("videofile","videoframe")]
  }
  opb<-pbapply::pboptions(char = "=")
  if(parallel){
    type="PSOCK"

    cl <- parallel::makeCluster(min(parallel::detectCores(),nproc),type=type)

    parallel::clusterExport(cl,list("outdir","format","fps","frames"),
                  envir=environment())
    #set random number generator for cluster
    parallel::clusterSetRNGStream(cl)

    results<-pbapply::pblapply(videos,function(x){run.parallel(x)},cl=cl)
    parallel::stopCluster(cl)

  }else{
    results<-pbapply::pblapply(videos,function(x){run.parallel(x)})
  }
  results<-do.call(rbind,results)
  results
}
