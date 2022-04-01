# animl

Animl comprises a variety of machine learning tools for analyzing ecological data. The package includes a set of functions to classify subjects within camera trap field data and can handle both images and videos. 

## Table of Contents
1. [Camera Trap Classificaton](#camera-trap-classification)
2. [Models](#models)
3. 

## Camera Trap Classification

Automatic identification of animals within camera trap images or videos. 
First, build the file manifest:

```R
imagedir <- "examples/TestData"

#create save-file placeholders and working directories
setupDirectory(imagedir)

# Read exif data for all images within base directory
files <- buildFileManifest(imagedir)

# Set Region/Site/Camera names 
files <- setLocation(files,imagedir)

# Process videos, extract frames for ID
imagesall<-imagesFromVideos(files,outdir=vidfdir,frames=5,parallel=T,nproc=12)
```

The authors recommend a two-step approach using Microsoft's 'MegaDector' object detector to first identify potential animals and then using a second classification model trained on the species of interest. 

MegaDetector can obtained from
https://github.com/microsoft/CameraTraps/blob/main/megadetector.md



### Models
All of our pre-trained classification models can be obtained at [https://]

Geographical regions represented:
* South America
* African Savanna
* Southwest United States

## What do I need to run animl?

