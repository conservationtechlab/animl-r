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


imagedir <- "C:\\Users\\Kyra\\animl-py\\examples\\Southwest"

#create global variable file and directory namesfrom animl import file_management
WorkingDirectory(imagedir, globalenv())

# Build file manifest for all images and videos within base directory
files <- build_file_manifest(imagedir, out_file=filemanifest, exif=TRUE)

#===============================================================================
# Add Project-Specific Info
#====================================+==========================================

# Get Station
#basedepth=length(strsplit(imagedir,split="/")[[1]])
#files$Station <- sapply(files$FilePath, function(x) strsplit(x,"/")[[1]][basedepth])

# Process videos, extract frames for ID
allframes <- extract_frames(files, out_dir = vidfdir, out_file=imageframes,
                            frames=3, parallel=T, num_workers=parallel::detectCores())


#===============================================================================
# MegaDetector
#===============================================================================
# Most functions assume MegaDetector version 5. If using an earlier version of 
# MD, specify detectObjectBatch with argument 'mdversion'.

# PyTorch Via Animl-Py
md_py <- load_detector("C:\\Users\\Kyra\\animl-py\\models\\md_v5a.0.0.pt", model_type = 'mdv5')

mdraw <- detect(md_py, allframes, 1280, 1280, batch_size=4)
mdresults <- parse_detections(mdraw, manifest = allframes, out_file = detections)

#mdresults <- read.csv(detections)
#mdresults$Station <- sapply(mdresults$FilePath, function(x) strsplit(x,"/")[[1]][5])
#select animal crops for classification
animals <- get_animals(mdresults)
empty <- get_empty(mdresults)

#===============================================================================
# Species Classifier
#===============================================================================

classes <- load_class_list('C:\\Users\\Kyra\\animl-py\\models\\sdzwa_southwest_v3_classes.csv')
class_list <- classes$class
southwest <- load_classifier('C:\\Users\\Kyra\\animl-py\\models\\sdzwa_southwest_v3.pt', length(class_list))

# get likelihoods
pred_raw <- classify(southwest, animals, resize_width=299, resize_height=299, out_file=predictions, batch_size=4)

# Single Classification
manifest <- single_classification(animals, empty, pred_raw, class_list)
manifest$station <- 'test'

# Sequence Classification
manifest <- sequence_classification(animals, empty=empty, pred_raw, classes=class_list, station_col="station", empty_class="empty")


#===============================================================================
# Symlinks
#===============================================================================

#symlink species predictions
alldata <- export_folders(manifest, linkdir)
write.csv(alldata, results)

#symlink MD detections only
sort_MD(manifest, linkdir)

#===============================================================================
# REID
#===============================================================================
miew = load_miewid("~/models/miewid_v3.bin")
embeddings = extract_embeddings(manifest, miew)

