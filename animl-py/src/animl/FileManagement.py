import pandas as pd
import os
import glob
from PIL import Image, ExifTags
from datetime import datetime

def load_image(input_file):
    """
    from CameraTraps/visualization/visualization_utils.py
    """
    image = Image.open(input_file)
    if image.mode not in ('RGBA', 'RGB', 'L'):
        raise AttributeError('Input image {} uses unsupported mode {}'.format(input_file, image.mode))
    if image.mode == 'RGBA' or image.mode == 'L':
        # PIL.Image.convert() returns a converted copy of this image
        image = image.convert(mode='RGB')
    image.load()
    return image


def is_image(s):
    """
    from CameraTraps/MegaDetector
    Check a file's extension against a hard-coded set of image file extensions    '
    """
    image_extensions = ['.jpg', '.jpeg', '.gif', '.png', '.mp4']
    ext = os.path.splitext(s)[1].lower()
    return ext in image_extensions


#def extractFrames(video,frames,fps):
#    check metadata 
    
#    if(result.frames < 5):
#        files = data.frame(FilePath = x, Frame = "File Corrupt")
#    else:
        
#      vfilter = "fps=" + round(1 / (result$duration / frames), 3) if len(frames) else "null"
#      vfilter = "fps=" + str(fps) if len(fps) else vfilter
        
#      framerate <- result$video$framerate
#      tempdir <- tempfile()
#      dir.create(tempdir)
    
#      name <- strsplit(basename(x), ".", fixed = T)[[1]][1]
        
#      rnd <- sprintf("%05d", round(stats::runif(1, 1, 99999), 0))
    
#      output <- file.path(tempdir, paste0(name, "_", rnd, "_%5d", ".jpg"))
        
#      av::av_encode_video(input = x, output = output, framerate = framerate, vfilter = vfilter, verbose = F)
#      files <- data.frame(FilePath = x, tmpframe = list.files(tempdir, pattern = paste0(name, "_", rnd, "_\\d{5}", ".jpg"), full.names = TRUE), stringsAsFactors = F)
    
#      files$Frame <- paste0(outdir, basename(files$tmpframe))
#      file.copy(files$tmpframe, files$Frame)
#      file.remove(files$tmpframe)
#      files[, c("FilePath", "Frame")]
    
#    return(files)
    




def imagesFromVideos(files,outdir = tempfile(),outfile = NA,
                     format = "jpg",fps = None,frames = None, 
                     parallel = False, nproc = 1):
    #if (checkFile(outfile)) { return(loadData(outfile))}

    assert type(files) == "DataFrame", "'files' must be Data Frame."
    if not os.path.isdir(outdir): os.makedirs(outdir)
    
    if((fps != None) and (frames != None)):
        print("If both fps and frames are defined fps will be used.")
    
    assert (fps != None) and (frames != None), "Either fps or frames need to be defined."

      images <- files[tools::file_ext(files$FileName) %in% c("jpg", "JPG", "png", "PNG"), ]
    images$Frame <- images$FilePath
    videos <- files[tools::file_ext(files$FileName) %in% c("mp4", "MP4", "avi", "AVI"), ]

    
    if(parallel):
      #  type <- "PSOCK"

    #    cl <- parallel::makeCluster(min(parallel::detectCores(), nproc), type = type)
   #     parallel::clusterExport(cl, list("outdir", "format", "fps", "frames"), envir = environment())

     #   parallel::clusterSetRNGStream(cl)

     #   parallel::clusterEvalQ(cl, library(av))
     #   results <- pbapply::pblapply(videos$FilePath, function(x) {
     #     try(run.parallel(x))
     #   }, cl = cl)
      #  parallel::stopCluster(cl)
        
    else: 
        for i in videos:
            videoframes = 
            
    videoframes <- merge(videos, results)
    allframes <- rbind(images, videoframes)
    
    # save frames to files
    if(!is.na(outfile)) { saveData(allframes, outfile)}
    
    return(allframes)



def buildFileManifest(imagedir, outfile = None):
    if outfile: pass
        #load file manifest 
    if not os.path.isdir(imagedir):
        return("The given directory does not exist.")

    files = glob.glob(os.path.join(imagedir, '**', '*.*'), recursive=True)
    images = [s for s in files if is_image(s)]

    images = pd.DataFrame(images, columns = ["FilePath"])
    images["FileName"] = images["FilePath"].apply(lambda x: os.path.split(x)[1])
    images["FileModifyDate"] = images["FilePath"].apply(lambda x: datetime.fromtimestamp(os.path.getmtime(x)).strftime('%Y-%m-%d %H:%M:%S'))

    return(images)
