# animl v2.0.0

Animl comprises a variety of machine learning tools for analyzing ecological data. The package includes a set of functions to classify subjects within camera trap field data and can handle both images and videos. 

## Table of Contents
1. [Camera Trap Classificaton](#camera-trap-classification)
2. [Models](#models)
3. [Installation](#installation)

## Camera Trap Classification

Below are the steps required for automatic identification of animals within camera trap images or videos. 

#### 1. File Manifest

First, build the file manifest of a given directory.

```R
library(animl)

imagedir <- "examples/TestData"

#create save-file placeholders and working directories
WorkingDirectory(imagedir,globalenv())

# Read exif data for all images within base directory
files <- build_file_manifest(imagedir, out_file=filemanifest, exif=TRUE)

# Process videos, extract frames for ID
allframes <- extract_frames(files, out_dir = vidfdir, out_file=imageframes,
                           frames=2, parallel=T, workers=parallel::detectCores())
```
#### 2. Object Detection

This produces a dataframe of images, including frames taken from any videos to be fed into the classifier. The authors recommend a two-step approach using Microsoft's 'MegaDector' object detector to first identify potential animals and then using a second classification model trained on the species of interest. 

More info on [MegaDetector](https://github.com/agentmorris/MegaDetector/tree/main).
```R
#Load the Megadetector model
md_py <- megadetector("/mnt/machinelearning/megaDetector/md_v5a.0.0.pt")

# Obtain crop information for each image
mdraw <- detect_MD_batch(md_py, allframes)

# Add crop information to dataframe
mdresults <- parse_MD(mdraw, manifest = allframes, out_file = detections)

```
#### 3. Classification
Then feed the crops into the classifier. We recommend only classifying crops identified by MD as animals.

```R
# Pull out animal crops
animals <- get_animals(mdresults)

# Set of crops with MD human, vehicle and empty MD predictions. 
empty <- get_empty(mdresults)

model_file <- "/Models/Southwest/v3/southwest_v3.pt"
class_list <- "/Models/Southwest/v3/southwest_v3_classes.csv"

# load the model
southwest <- load_model(model_file, class_list)

# obtain species predictions
animals <- predict_species(animals, southwest[[1]], southwest[[2]], raw=FALSE)

# recombine animal detections with remaining detections
manifest <- rbind(animals,empty)

```

If your data includes videos or sequences, we recommend using the sequenceClassification algorithm.
This requires the raw output of the prediction algorithm.

```
classes = southwest[[2]]$Code

# Sequence Classification
pred <- predict_species(animals, southwest[[1]], southwest[[2]], raw=TRUE)
manifest <- sequenceClassification(animals, empty=empty, pred, classes, "Station", emptyclass="empty")
```

# Models

The Conservation Technology Lab has several [models](https://sandiegozoo.app.box.com/s/9f3xuqldvg9ysaix9c9ug8tdcrmc2eqx) available for use. 

## Installation

#### Requirements
* R >= 4.0
* Reticulate
* Python >= 3.9
* [Animl-Py = 1.4.3](https://github.com/conservationtechlab/animl-py)

We recommend running animl on a computer with a dedicated GPU.
Animl also depends on [exiftool](https://exiftool.org/index.html) for accessing file metadata.

#### Python
animl depends on python and will install python package dependencies if they are not available if installed via CRAN. <br> 
However, we recommend setting up a conda environment using the provided config file. 

[Instructions to install conda](https://docs.conda.io/projects/conda/en/latest/user-guide/install/index.html)

The R version of animl depends on the python version to handle the machine learning:
[animl-py](https://github.com/conservationtechlab/animl-py)

Next, install animl-py in your preferred python environment (such as conda) using pip:
```
pip install animl
```

Animl-r can be installed through CRAN:
```R
install.packages('animl')
```
Animl-r can also be installed by downloading this repo, opening the animl.Rproj file in RStudio and selecting Build -> Install Package.


### Contributors

Kyra Swanson <br>
Mathias Tobler <br> 
Edgar Navarro <br>
Josh Kessler <br>
Jon Kohler <br>
