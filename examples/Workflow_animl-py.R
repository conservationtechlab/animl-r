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

imagedir <- "/home/kyra/animl-py/examples/Southwest/"

#create global variable file and directory namesfrom animl import file_management
WorkingDirectory(imagedir, globalenv())

# Build file manifest for all images and videos within base directory
files <- build_file_manifest(imagedir, out_file=filemanifest, exif=TRUE)

#===============================================================================
# Add Project-Specific Info
#====================================+==========================================

# Get Station
basedepth=length(strsplit(imagedir,split="/")[[1]])
files$Station <- sapply(files$FilePath, function(x) strsplit(x,"/")[[1]][basedepth])

# Process videos, extract frames for ID
allframes <- extract_frames(files, out_dir = vidfdir, out_file=imageframes,
                           frames=1, parallel=F, workers=parallel::detectCores())

#===============================================================================
# MegaDetector
#===============================================================================
# Most functions assume MegaDetector version 5. If using an earlier version of 
# MD, specify detectObjectBatch with argument 'mdversion'.

# PyTorch Via Animl-Py
md_py <- megadetector("/home/kyra/animl-py/models/md_v5a.0.0.pt")

mdraw <- detect_MD_batch(md_py, allframes)
mdresults <- parse_MD(mdraw, manifest = allframes, out_file = detections)

#mdresults <- read.csv(detections)
#mdresults$Station <- sapply(mdresults$FilePath, function(x) strsplit(x,"/")[[1]][5])
#select animal crops for classification
animals <- get_animals(mdresults)
empty <- get_empty(mdresults)

#===============================================================================
# Species Classifier
#===============================================================================

southwest <- load_model('/home/kyra/animl-py/models/sdzwa_southwest_v3.pt',
                       '/home/kyra/animl-py/models/sdzwa_southwest_v3_classes.csv')
class_list=southwest[[2]]$Code

# get likelihoods
pred_raw <- predict_species(animals, southwest[[1]])

# Single Classification
animals <- single_classification(animals, pred_raw, class_list)
manifest <- rbind(animals, empty)

# Sequence Classification
manifest <- sequence_classification(animals, empty=empty, pred_raw, classes=class_list, "Station", emptyclass="empty")


#===============================================================================
# Symlinks
#===============================================================================

#symlink species predictions
alldata <- sort_species(manifest, linkdir)

#symlink MD detections only
sort_MD(manifest, linkdir)


