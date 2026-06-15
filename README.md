# animl v3.3.0

Animl comprises a variety of machine learning tools for analyzing ecological data. The package includes a set of functions to classify subjects within camera trap field data and can handle both images and videos. 

## Table of Contents
1. [Tips for Use](#tips-for-use)
2. [Models](#models)
3. [Installation](#installation)
4. [Release Notes](#release-notes)

# Tips for Use

Below are the steps required for automatic identification of animals within camera trap images or videos. 

#### 1. File Manifest

First, build the file manifest of a given directory.

```R
library(animl)

imagedir <- "examples/TestData"

#create save-file placeholders and working directories
WorkingDirectory(imagedir, globalenv())

# Read exif data for all images within base directory
files <- build_file_manifest(imagedir, out_file=filemanifest_file, exif=TRUE, data_timezone="America/Los_Angeles")

# Process videos, extract frames for ID
allframes <- extract_frames(files, frames=3, out_file=imageframes_file,
                            parallel=T, num_workers=4)

```
#### 2. Object Detection

This produces a dataframe of images, including frames taken from any videos to be fed into the classifier. The authors recommend a two-step approach using the 'MegaDector' object detector to first identify potential animals and then using a second classification model trained on the species of interest. 

More info on <br>
[MegaDetector v5/v1000](https://github.com/agentmorris/MegaDetector/tree/main) <br>
[MegaDetector v6](https://microsoft.github.io/CameraTraps/megadetector/) 

```R
#Load the Megadetector model
detector <- load_detector("/Models/md_v5b.0.0.pt", model_type = 'mdv5', device='cuda:0')

# Obtain crop information for each image
mdraw <- detect(detector, allframes, resize_width=1280, resize_height=960, batch_size=4, device='cuda:0')

# Add crop information to dataframe
mdresults <- parse_detections(mdraw, manifest = allframes, out_file = detections_file)
```

#### 3. Classification
Then feed the crops into the classifier. We recommend only classifying crops identified by MD as animals.


```R
# Pull out animal crops
animals <- get_animals(mdresults)

# Set of crops with MD human, vehicle and empty MD predictions. 
empty <- get_empty(mdresults)

# load class list
classes <- load_class_list("/Models/Southwest/v3/southwest_v3_classes.csv")
class_list <- classes$class

# load the model
model_file <- "/Models/Southwest/v3/southwest_v3.pt"
southwest <- load_classifier(model_file, nrow(class_list))

# obtain species predictions likelihoods
pred_raw <- classify(southwest, animals, resize_width=480, resize_height=480, 
                     out_file=predictions_file, batch_size=16, num_workers=8)

# apply class_list labels and combine with empty set
manifest <- single_classification(animals, empty, pred_raw, class_list$class)
```

If your data includes videos or sequences, we recommend using the sequence_classification algorithm.
This requires the raw output of the prediction algorithm.

```R
# Sequence Classification
manifest <- sequence_classification(animals, empty=empty, pred_raw, classes=class_list,
                                    station_col="station", empty_class="empty")
```

#### 4. Export

You can export the data into folders sorted by prediction: 
```
manifest <- export_folders(manifest, out_dir=linkdir, out_file=results_file)
```
or into folders sorted by prediction and by station for export to camtrapR:
```
manifest <- export_camtrapR(manifest, out_dir=linkdir, out_file=results_file,
                            label_col='prediction', file_col="filepath", station_col='station')
```
 
You can also export a .json file formatted for COCO
```
manifest <- export_coco(manifest, class_list=class_list, out_file='results.json')
```
Or a .csv file for Timelapse
```
manifest <- export_folders(manifest, out_dir=linkdir)
```






# Models

The Conservation Technology Lab has several [models](https://sandiegozoo.app.box.com/s/9f3xuqldvg9ysaix9c9ug8tdcrmc2eqx) available for use. <br><br>
Detectors:
[MegaDetector v5/v1000](https://github.com/agentmorris/MegaDetector/tree/main) <br>
[MegaDetector v6](https://microsoft.github.io/CameraTraps/megadetector/) 


# Installation

### Requirements
* R >= 4.0
* Reticulate
* Python >= 3.12
* [Animl-Py >= 3.3.0](https://github.com/conservationtechlab/animl-py)

We recommend running animl on a computer with a dedicated GPU.

### Python
animl depends on python and will install python package dependencies if they are not available if installed via miniconda. <br> 

The R version of animl also depends on the python version to handle the machine learning:
[animl-py](https://github.com/conservationtechlab/animl-py)

Animl-r can be installed through CRAN:
```R
install.packages('animl')
```

You must then set up the environment and install animl-py:
```R
library(animl)
animl::animl_install()
```

You will be prompted to restart your R session.


Animl-r can also be installed by downloading this repo, opening the animl.Rproj file in RStudio and selecting Build -> Install Package.


# Release Notes 
## New for 3.3.0
 - compatible with animl-py v3.3.0
 - add data_timezone arg to build_file_manifest for automatic timestamp handling
 - add support for video metadata timestamp via Exiftool 
 - minor bug fixes


### Contributors

Kyra Swanson <br>
Mathias Tobler <br> 
Edgar Navarro <br>
Josh Kessler <br>
Jon Kohler <br>
