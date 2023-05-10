#' Title
#' 
#' @param imagedir description
#' @param mdmodel description
#' @param speciesmodel description
#' @param classes description
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{
#' imagedir <- "examples/test_data/Southwest"
#'  mdmodel <- "/mnt/machinelearning/megaDetector/md_v5b.0.0_saved_model"
#'  modelfile <- "/mnt/machinelearning/Models/Southwest/2022/Southwest_v2.h5"
#'  classes <- "/mnt/machinelearning/Models/Southwest/2022/classes.txt"
#'  animl(imagedir,mdmodel,modelfile,classes) }
animl <-function(imagedir, mdmodel, speciesmodel, classes){
  #establish animl global variables
  pkg.env <- new.env(parent = emptyenv())
  setupDirectory(imagedir,pkg.env)
  message("Building file manifest...")
  files <- buildFileManifest(imagedir, outfile = pkg.env$filemanifest, exif = TRUE)
  
  #build new name
  basedepth=length(strsplit(imagedir,split="/")[[1]])
  
  files$Region<-sapply(files$Directory,function(x)strsplit(x,"/")[[1]][basedepth])
  files$Site<-sapply(files$Directory,function(x)strsplit(x,"/")[[1]][basedepth+1])
  files$Camera<-sapply(files$Directory,function(x)strsplit(x,"/")[[1]][basedepth+2])
  
  #files must have a new name for symlink
  files$UniqueID = round(stats::runif(nrow(files), 1, 99999),0)
  files$UniqueName = paste(files$Region,files$Site,files$Camera,format(files$DateTime,format="%Y-%m-%d_%H%M"),files$UniqueID,sep="_")
  files$UniqueName = paste0(files$UniqueName, ".", tolower(tools::file_ext(files$FileName)))

  message("Extracting frames from videos...")
  allframes <- imagesFromVideos(files, outdir = pkg.env$vidfdir, 
                                outfile=pkg.env$imageframes, frames=5, 
                                parallel=T, workers=parallel::detectCores())
  
  # MegaDetector
  message("Begin object detection...")
  
  if (checkFile(pkg.env$mdresults)){ 
    load(pkg.env$mdresults) }
  else{
    mdsession <- loadMDModel(mdmodel)
    
    results <- detectObjectBatch(mdsession, allframes$Frame, 
                               outfile = pkg.env$mdresults, checkpoint = 2500)
  }
  
  y <- parseMD(results, manifest = allframes)
  animals <- getAnimals(y)
  empty <- getEmpty(y)
  
  # Species Classifier
  message("Begin species classification...")
  if (checkFile(pkg.env$predresults)){ animals <- loadData(pkg.env$predresults) }
  else{
    pred <- predictSpecies(animals, speciesmodel, batch = 64, workers = 8)
    
    animals <- applyPredictions(animals, pred, classes, 
                                outfile = pkg.env$predresults, counts = TRUE)
  }
  
  #rejoin animal and empty data splits
  manifest <- rbind(animals,empty)
  # Classify sequences / select best prediction
  best <- bestGuess(manifest, sort = "count", parallel = T, workers = parallel::detectCores(), shrink = TRUE)
  
  # Symlinks
  message("Creating symbolic links...")
  alldata <- symlinkSpecies(best, pkg.env$linkdir, outfile = pkg.env$resultsfile)
}
