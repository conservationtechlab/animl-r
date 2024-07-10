# This functions are defunct/extraneous and need maintenance to bring in line
# with the rest of the package.


#' Extract bounding boxes for a single image and save as new images
#'
#' Requires the unflattened raw MD output
#'
#' @param image single image, raw MD output format (list)
#' @param min_conf Confidence threshold (defaults to 0, not in use)
#' @param buffer Adds a buffer to the MD bounding box, defaults to 2px
#' @param return.crops Toggle to return list of cropped images, defaults to FALSE
#' @param save Toggle to save output cropped, defaults to FALSE
#' @param resize Size in pixels to resize cropped images, NA if images are not resized, defaults to NA
#' @param outdir Directory in which output cropped images will be saved
#' @param quality Compression level of output cropped image, defaults to 0.8
#'
#' @return a flattened data.frame containing crop information
#' @export
#' @details A variable crop_rel_path in the image list can be used to change the path where the crops will be stored. 
#' @details The final output path will be the outdir plus the crop_rel_path.
#'
#' @examples
#' \dontrun{
#' images <- read_exif(imagedir, tags = c("filename","directory"), recursive = TRUE)
#' crops <- extractBoxesFromMD(images[1, ], return.crops = TRUE, save = TRUE)
#' }
extractBoxesFromMD <- function(image, min_conf = 0, buffer = 0, return.crops = FALSE, save = FALSE, resize = NA, outdir = "", quality = 0.8) {
  if (save & !dir.exists(outdir)) {
    stop("Output directory invalid.\n")
  }
  images_flat <- data.frame(
    FilePath = character(), md_class = numeric(), md_confidence = numeric(), pixelx = numeric(), pixely = numeric(),
    x1 = numeric(), x2 = numeric(), y1 = numeric(), y2 = numeric(),
    xmin = numeric(), xmax = numeric(), ymin = numeric(), ymax = numeric(), crop_path = character(), stringsAsFactors = FALSE
  )

  # load image
  jpg <- jpeg::readJPEG(image$FilePath)
  jpgy <- dim(jpg)[1]
  jpgx <- dim(jpg)[2]

  # get bounding boxes
  # get bounding boxes
  if (is.data.frame(image$detections) && nrow(image$detections)>0) {
    s <- image$detections

    # extract bounding box
    if (return.crops) {
      crops <- list()
    }

    c <- 1
    for (j in 1:nrow(s)) {
      xmin <- max(0, round(s[j, ]$bbox1 * jpgx, 0))
      xmax <- min(jpgx, round(s[j, ]$bbox1 * jpgx + max(1, s[j, ]$bbox3 * jpgx), 0))
      ymin <- max(0, round(s[j, ]$bbox2 * jpgy, 0))
      ymax <- min(jpgy, round(s[j, ]$bbox2 * jpgy + max(1, s[j, ]$bbox4 * jpgy), 0))
      buffer2 <- max(xmax - xmin, ymax - ymin) * buffer

      xminb <- max(0, xmin - buffer2)
      xmaxb <- min(jpgx, xmax + buffer2)
      yminb <- max(0, ymin - buffer2)
      ymaxb <- min(jpgy, ymax + buffer2)

      if (length(dim(jpg)) == 2) {
        dim(jpg) <- c(dim(jpg)[1], dim(jpg)[2], 1)
      }

      crop <- jpg[yminb:ymaxb, xminb:xmaxb, ]

      # resize and pad if requested
      if (!is.na(resize)) {
        crop <- resizePad(crop, resize)
      }

      # if we return crops save crop in list
      if (return.crops) {
        crops[[c]] <- crop
      }

      # save image if requested
      imgname <- ""
      if (save) {
        if(!is.null(image$crop_rel_path)){
          imgname <- paste0(outdir, image$crop_rel_path,basename(image$FilePath))
        }else{
          imgname <- paste0(outdir, basename(image$FilePath))
        }
        imgbase <- strsplit(basename(imgname), "[.]")[[1]][1]
        imgext <- strsplit(basename(imgname), "[.]")[[1]][2]
        if (nrow(s) > 1) {
          imgname <- paste0(dirname(imgname), "/", imgbase, "_c", j, ".", imgext)
        }
        i=j
        while(file.exists(imgname)){
          imgname <- paste0(dirname(imgname), "/", imgbase, "_c", i, ".", imgext)  
          i=i+1
        }
        if (!dir.exists(dirname(imgname))) {
          dir.create(dirname(imgname), recursive = TRUE)
        }

        jpeg::writeJPEG(crop, imgname, quality = quality)
      }
      line <- data.frame(
        FilePath = image$FilePath, md_class = as.numeric(s[j, ]$category), md_confidence = s[j, ]$conf, pixelx = jpgx, pixely = jpgy,
        x1 = s[j, ]$bbox1, x2 = s[j, ]$bbox2, y1 = s[j, ]$bbox3, y2 = s[j, ]$bbox4,
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, crop_path = imgname, stringsAsFactors = FALSE
      )
      images_flat <- rbind(images_flat, line)
    }
  } else {
    line <- data.frame(
      FilePath = image$FilePath, md_class = 0, md_confidence = image$max_detection_conf, pixelx = jpgx, pixely = jpgy,
      x1 = NA, x2 = NA, y1 = NA, y2 = NA,
      xmin = NA, xmax = NA, ymin = NA, ymax = NA, crop_path = "", stringsAsFactors = FALSE
    )
    images_flat <- rbind(images_flat, line)
  }
  if (return.crops) {
    list(crops = crops, data = images_flat)
  } else {
    images_flat
  }
}


#' Extract crops from a single image represented by a processed dataframe
#'
#' @param image dataframe containing MD output (assumes single row)
#' @param min_conf Confidence threshold (defaults to 0, not in use)
#' @param buffer Adds a buffer to the MD bounding box, defaults to 2px
#' @param save Toggle to save output cropped, defaults to FALSE
#' @param resize Size in pixels to resize cropped images, NA if images are not resized, defaults to NA
#' @param outdir Directory in which output cropped images will be saved
#' @param quality Compression level of output cropped image, defaults to 0.8
#'
#' @return A dataframe containing image and crop paths
#' @details A variable crop_rel_path in the image list can be used to change the path where the crops will be stored. 
#' @details The final output path will be the outdir plus the crop_rel_path.
#' @export
#'
#' @examples
#' \dontrun{
#' crops <- extractBoxesFromFlat(mdresflat[1, ], save = TRUE, out)
#' }
extractBoxesFromFlat <- function(image, min_conf = 0, buffer = 0, save = TRUE, resize = NA, outdir = "", quality = 0.8) {
  if (save & !dir.exists(outdir)) {
    stop("Output directory invalid.\n")
  }
  
  if(sum(is.na(image[,c("bbox1","bbox2","bbox3","bbox4")]))==0 & image$max_conf>=min_conf){

    # load image
    jpg <- jpeg::readJPEG(image$Frame)
    jpgy <- dim(jpg)[1]
    jpgx <- dim(jpg)[2]
  
    # get bounding boxes
    # get bounding boxes
    xmin <- max(0, round(image$bbox1 * jpgx, 0))
    xmax <- min(jpgx, round(image$bbox1 * jpgx + max(1, image$bbox3 * jpgx), 0))
    ymin <- max(0, round(image$bbox2 * jpgy, 0))
    ymax <- min(jpgy, round(image$bbox2 * jpgy + max(1, image$bbox4 * jpgy), 0))
    buffer2 <- max(xmax - xmin, ymax - ymin) * buffer
  
    xminb <- max(0, xmin - buffer2)
    xmaxb <- min(jpgx, xmax + buffer2)
    yminb <- max(0, ymin - buffer2)
    ymaxb <- min(jpgy, ymax + buffer2)
    if (length(dim(jpg)) == 2) {
      dim(jpg) <- c(dim(jpg)[1], dim(jpg)[2], 1)
    }
  
    crop <- jpg[yminb:ymaxb, xminb:xmaxb, ]
  
    # resize and pad if requested
    if (!is.na(resize)) {
      crop <- resizePad(crop, resize)
    }
  
    # save image if requested
    if (save) {
      if(!is.null(image$crop_rel_path)){
        imgname <- paste0(outdir, image$crop_rel_path,basename(image$Frame))
      }else{
        imgname <- paste0(outdir, basename(image$Frame))
      }
      imgbase <- strsplit(basename(imgname), "[.]")[[1]][1]
      imgext <- strsplit(basename(imgname), "[.]")[[1]][2]
      j=1
      while(file.exists(imgname)){
        imgname <- paste0(dirname(imgname), "/", imgbase, "_c", j, ".", imgext)  
        j=j+1
      }
      if (!dir.exists(dirname(imgname))) {
        dir.create(dirname(imgname), recursive = T)
      }

      jpeg::writeJPEG(crop, imgname, quality = quality)
    }
    data.frame(image,crop_path=imgname)
  }
}


#'  Extract bounding boxes and save as new image from a batch of images
#'
#' @param images list of MD output or flat data.frame
#' @param min_conf Confidence threshold (defaults to 0, not in use)
#' @param buffer Adds a buffer to the MD bounding box, defaults to 2px
#' @param save Toggle to save output cropped, defaults to FALSE
#' @param resize Size in pixels to resize cropped images, NA if images are not resized, defaults to NA
#' @param outdir Directory in which output cropped images will be saved
#' @param quality Compression level of output cropped image, defaults to 0.8
#' @param parallel Toggle to enable parallel processing, defaults to FALSE
#' @param nproc Number of workers if parallel = TRUE, defaults to output of detectCores()
#'
#' @return a flattened dataframe containing crop information
#' @details A variable crop_rel_path in the image list or data.frame can be used to change the path where the crops will be stored. 
#' @details The final output path will be the outdir plus the crop_rel_path.
#' @export
#'
#' @examples
#' \dontrun{
#' images <- read_exif(imagedir, tags = c("filename", "directory"), recursive = TRUE)
#' crops <- extractAllBoxes(images,save=TRUE,out)
#' }
extractBoxes <- function(images, min_conf = 0, buffer = 0, save = FALSE, resize = NA, outdir = "", quality = 0.8, parallel = FALSE, nproc = parallel::detectCores()) {
  if (outdir != "" & !dir.exists(outdir)) {
    if (!dir.create(outdir, recursive = TRUE)) {
      stop("Output directory invalid.\n")
    }
  }
  if(!(inherits(images,"list") | inherits(images,"data.frame"))){
    stop("images needs to be a list of MD results or a data.frame.\n")
  }
  if(inherits(images,"list")){
    # define processing function
    run.parallel <- function(i) {
      if (file.exists(images[[i]]$FilePath)) {
        extractBoxesFromMD(images[[i]], min_conf = min_conf, buffer = buffer, resize = resize, save = save, outdir = outdir, quality = quality)
      } 
      else { NA }
    }
    opb <- pbapply::pboptions(char = "=")
    if (parallel) {
      type <- "PSOCK"
      
      cl <- parallel::makeCluster(min(parallel::detectCores(), nproc), type = type)
      parallel::clusterExport(cl, list("buffer", "resize", "quality", "outdir", "images", "extractBoxesFromMD", "resizePad"), envir = environment())
      # set random number generator for cluster
      parallel::clusterSetRNGStream(cl)
      
      results <- pbapply::pblapply(1:length(images), function(x) {
        run.parallel(x)
      }, cl = cl)
      parallel::stopCluster(cl)
    } else {
      results <- pbapply::pblapply(1:length(images), function(x) {
        run.parallel(x)
      })
    }
    results <- do.call(rbind, results)
    results
  }else if(inherits(images,"data.frame")){
    # define processing function
    run.parallel <- function(i) {
      if (file.exists(images[i, ]$Frame)) {
        extractBoxesFromFlat(images[i, ], min_conf = min_conf, buffer = buffer, resize = resize, save = save, outdir = outdir, quality = quality)
      } else {
        NA
      }
    }
    
    opb <- pbapply::pboptions(char = "=")
    if (parallel) {
      type <- "PSOCK"
      cl <- parallel::makeCluster(min(parallel::detectCores(), nproc), type = type)
      parallel::clusterExport(cl, list("buffer", "resize", "quality", "outdir", "images", "extractBoxesFromFlat", "resizePad"), envir = environment())
      # set random number generator for cluster
      parallel::clusterSetRNGStream(cl)
      
      results <- pbapply::pblapply(1:nrow(images), function(x) { run.parallel(x) }, cl = cl)
      parallel::stopCluster(cl)
    } else {
      results <- pbapply::pblapply(1:nrow(images), function(x) { run.parallel(x) })
    }
    results <- do.call(rbind, results)
    results    
  }
}

#' Resize an image with padding
#'
#' @param img  the image, read by jpeg library
#' @param size new size
#'
#' @return returns resized jpeg image
#' @export
#'
#' @examples
#' \dontrun{
#' crop <- resizePad(cropped_image_path,256)
#' }
resizePad <- function(img, size = 256) {
  if (dim(img)[0] == 0 || dim(img)[1] == 0) {
    return(img)
  }
  imgpad <- array(0, c(max(dim(img)), max(dim(img)), 3))
  xstart <- max(1, floor((dim(imgpad)[2] - dim(img)[2]) / 2))
  ystart <- max(1, floor((dim(imgpad)[1] - dim(img)[1]) / 2))
  imgpad[ystart:(ystart + dim(img)[1] - 1), xstart:(xstart + dim(img)[2] - 1), ] <- img
  imgres <- imager::resize(imager::as.cimg(imgpad), size_x = size, size_y = size, interpolation_type = 3)
  imgres[, , 1, ]
}

