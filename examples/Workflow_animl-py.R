# animl Classification Workflow
#
# c 2021 Mathias Tobler
# Maintained by Kyra Swanson
#
#
#-------------------------------------------------------------------------------
# Setup
#-------------------------------------------------------------------------------
library(animl)
library(reticulate)
use_condaenv("animl-gpu")

device <- 'cuda:0'

imagedir <- "/home/kyra/animl-py/examples/Southwest"

#create global variable file and directory names
workingDirectory(imagedir,globalenv())

# Build file manifest for all images and videos within base directory
files <- buildFileManifest(imagedir, outfile = filemanifest, exif = TRUE)

#===============================================================================
# Add Project-Specific Info
#===============================================================================

#build new name
basedepth=length(strsplit(imagedir,split="/")[[1]])
files$Region<-sapply(files$Directory,function(x)strsplit(x,"/")[[1]][basedepth])

# Process videos, extract frames for ID
allframes <- extractFrames(files, outdir = vidfdir, outfile=imageframes,
                           frames=2, parallel=T, workers=parallel::detectCores())

#===============================================================================
# MegaDetector
#===============================================================================
# Most functions assume MegaDetector version 5. If using an earlier version of 
# MD, specify detectObjectBatch with argument 'mdversion'.

# PyTorch Via Animl-Py
md_py <- megadetector("/mnt/machinelearning/megaDetector/md_v5a.0.0.pt")

mdres <- detectMD_batch(md_py, allframes)
detections <- parseMD(mdres, manifest = allframes, outfile = detections)

#select animal crops for classification
animals <- getAnimals(detections)
empty <- getEmpty(detections)

#===============================================================================
# Species Classifier
#===============================================================================

#modelfile <- "/mnt/machinelearning/Models/Southwest/v2/EfficientNetB5_456_Unfrozen_05_0.26_0.92.h5"
#modelfile <- "/mnt/machinelearning/Models/Kenya/2022/EfficientNetB5_456_Unfrozen_04_0.60_0.89.h5"


gabon <- loadModel('/Models/African_Forest/mbaza-gabon.onnx',
                       '/Models/African_Forest/gabon_classes.csv', device='gpu')
southwest <- loadModel('/mnt/machinelearning/Models/Southwest/v3/southwest_v3.pt',
                       '/mnt/machinelearning/Models/Southwest/v3/southwest_v3_classes.csv', device=device)

# Single Image Classification

animals <- animl_py$predict_species(animals, gabon[[1]], gabon[[2]], raw=FALSE, resize=c(768,576), channel_last = TRUE)

animals <- predictSpecies(animals, southwest[[1]], southwest[[2]], device=device, raw=FALSE)

manifest <- rbind(animals,empty)

# Sequence Classification



#===============================================================================
# Symlinks
#===============================================================================

#symlink species predictions
alldata <- symlinkSpecies(animals, linkdir, outfile = resultsfile)

#symlink MD detections only
symlinkMD(best,linkdir)


#===============================================================================
# Export to Camera Base
#===============================================================================






