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

imagedir <- "C:\\Users\\Kyra\\animl\\examples\\Southwest"
imagedir <- "examples/Southwest"

#create global variable file and directory namesfrom animl import file_management
WorkingDirectory(imagedir, globalenv())

# Build file manifest for all images and videos within base directory
files <- build_file_manifest(imagedir, out_file=filemanifest_file, exif=TRUE, 
                             station_depth = 0, camera_depth = 0, data_timezone = 'America/Los_Angeles')

files <- sequence_calculation(files, 'station')

# Process videos, extract frames for ID
allframes <- extract_frames(files, frames=3, out_file=imageframes_file,
                            parallel=T, num_workers=parallel::detectCores())


#===============================================================================
# MegaDetector
#===============================================================================
# Most functions assume MegaDetector version 5. If using an earlier version of 
# MD, specify detectObjectBatch with argument 'mdversion'.

# PyTorch Via Animl-Py
md_py <- load_detector("/home/kyra/models/md_v5b.0.1.pt", model_type = 'mdv5')

mdraw <- detect(md_py, allframes, 1280, 1280, batch_size=4)
mdresults <- parse_detections(mdraw, manifest = allframes, out_file = detections_file)

#select animal crops for classification
animals <- get_animals(mdresults)
empty <- get_empty(mdresults)


#===============================================================================
# Species Classifier
#===============================================================================

southwest <- load_classifier('/home/kyra/models/sdzwa_southwest_v3.pt', '/home/kyra/models/sdzwa_southwest_v3_classes.csv')

# get likelihoods
pred_raw <- classify(southwest$model, animals, resize_width=299, resize_height=299, out_file=predictions_file, batch_size=4)

# Single Classification
manifest <- single_classification(animals, empty, pred_raw, southwest$classes, best = TRUE)

# Sequence Classification
manifest2 <- sequence_classification(animals, empty=empty, pred_raw, classes=southwest$classes$class, station_col="station", empty_class="empty")


#===============================================================================
# Symlinks
#===============================================================================

#symlink species predictions
alldata <- export_folders(manifest, linkdir, out_file = results_file)


#===============================================================================
# Visualization
#===============================================================================

plot_all_bounding_boxes(manifest, visdir, classifier_label_col='prediction', show_confidence = TRUE)

