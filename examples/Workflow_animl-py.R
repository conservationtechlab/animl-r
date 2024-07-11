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
files$Site<-sapply(files$Directory,function(x)strsplit(x,"/")[[1]][basedepth+1])
files$Camera<-sapply(files$Directory,function(x)strsplit(x,"/")[[1]][basedepth+2])
#files must have a new name for symlink
#files$UniqueName=paste(files$Region, files$Site, files$Camera, files$FileName, sep="_")

# Process videos, extract frames for ID
  allframes <- extractFrames(files, outdir = vidfdir, outfile=imageframes,
                           frames=2, parallel=T, workers=parallel::detectCores())

#===============================================================================
# MegaDetector
#===============================================================================
# Most functions assume MegaDetector version 5. If using an earlier version of 
# MD, specify detectObjectBatch with argument 'mdversion'.
animl_py <- import('animl-py')

# PyTorch Via Animl-Py
md_py <- megadetector("/mnt/machinelearning/megaDetector/md_v5a.0.0.pt")

mdres <- detectMD_batch(md_py, allframes$Frame)
detections <- parseMD(mdres, manifest = allframes, outfile = mdresults)

#select animal crops for classification
animals <- getAnimals(detections)
empty <- getEmpty(detections)

#===============================================================================
# Species Classifier
#===============================================================================

#modelfile <- "/mnt/machinelearning/Models/Southwest/v2/EfficientNetB5_456_Unfrozen_05_0.26_0.92.h5"
#modelfile <- "/mnt/machinelearning/Models/Kenya/2022/EfficientNetB5_456_Unfrozen_04_0.60_0.89.h5"

andes <- loadModel()
southwest <- loadModel('/mnt/machinelearning/Models/African_Forest/mbaza-gabon.onnx',
                       '/mnt/machinelearning/Models/African_Forest/gabon_classes.csv', device='gpu')
southwest <- loadModel('/mnt/machinelearning/Models/Southwest/v3/southwest_v3.pt',
                       '/mnt/machinelearning/Models/Southwest/v3/southwest_v3_classes.csv', device='cuda:0')

classes <- southwest[[2]]
classes['Code'] <- classes['species']


# Single Image Classification
animals <- predictSpecies(animals, southwest[[1]], classes, device='cuda:0', raw=FALSE)


animals <- animl_py$predict_species(animals, southwest[[1]], southwest[[2]], raw=FALSE, resize=c(768,576), channel_last = TRUE)
manifest <- rbind(animals,empty)


# Sequence Classification
pred <- predictSpecies(animals, classifier[[1]], classifier[[2]], raw=TRUE)

#manifest <- rbind(animals,empty)

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






