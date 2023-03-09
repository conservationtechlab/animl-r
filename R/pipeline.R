#' Run the Full Animl Workflow
#'
#' @param imagedir the directory path containing images to analyze
#' @param workingdir the directory path to save outputs to
#' @param mdmodel location of the MegaDetector model
#'
#' @return a file manifest with predictions
#' @export
#'
#' @examples
#' \dontrun{
#' animlPipeline("~/examples/test_data/", "home/kyra/test/", NULL, NULL, NULL)
#' }
animlPipeline <- function(imagedir, workingdir, mdmodel, classmodel, classlabels){
  
  date = paste0(Sys.Date(),"_")
  setupDirectory(workingdir,date)
  print("Processing videos...")
  files <- buildFileManifest(imagedir, outfile = filemanifest, prompt = FALSE, exif=TRUE)
  manifest <- imagesFromVideos(files, outdir = vidfdir, frames = 5, parallel=T, workers= parallel::detectCores(), 
                               outfile = imageframes, prompt = FALSE)
  print("Detecting animals...")
  mdsession <- loadMDModel(mdmodel)
  mdres <- detectObjectBatch(mdsession, manifest$Frame, checkpoint = 100, prompt=FALSE)
  manifest <- parseMD(mdres, manifest, outfile = mdresults, prompt = FALSE)
   
  animals <- getAnimals(manifest)
  empty <- getEmpty(manifest)
  print("Classifying animals...")
  pred <- predictSpecies(animals, classmodel)
  animals <- applyPredictions(animals, pred, classlabels, 
                               outfile = resultsfile, prompt = FALSE, counts = TRUE)
   print(animals)
   manifest <- rbind(animals,empty)
   best <- bestGuess(manifest, sort = "conf", parallel = T, workers = 12, prompt = FALSE, shrink = TRUE)
   best
  # alldata <- symlinkSpecies(best, linkdir)
}
