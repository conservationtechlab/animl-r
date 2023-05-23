# animl v1.0.0

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
setupDirectory(imagedir)

# Read exif data for all images within base directory
files <- buildFileManifest(imagedir)

# Set Region/Site/Camera names based on folder hierarchy
files <- setLocation(files,imagedir)

# Process videos, extract frames for ID
imagesall<-imagesFromVideos(files,outdir=vidfdir,frames=5)
```
#### 2. Object Detection

This produces a dataframe of images, including frames taken from any videos to be fed into the classifier. The authors recommend a two-step approach using Microsoft's 'MegaDector' object detector to first identify potential animals and then using a second classification model trained on the species of interest. 

MegaDetector can obtained from
https://github.com/microsoft/CameraTraps/blob/main/megadetector.md

```R
#Load the Megadetector model
mdsession<-loadMDModel("/path/to/megaDetector/megadetector_v4.1.pb")

#+++++++++++++++++++++
# Classify a single image to make sure everything works before continuing
testMD(imagesall,mdsession)
#+++++++++++++++++++++

# Obtain crop information for each image, checkpoint MegaDetector after every 2500 images
mdres <- classifyImagesBatchMD(mdsession,imagesall$Frame,resultsfile=paste0(datadir,mdresults),checkpoint = 2500)

# Add crop information to dataframe
imagesall <- parseMDsimple(imagesall, mdres)

```
#### 3. Classification
Then feed the crops into the classifier. We recommend only classifying crops identified by MD as animals.

```R
# Pull out animal crops
animals <- imagesall[imagesall$max_detection_category==1,]

# Set of crops with MD human, vehicle and empty MD predictions. 
empty <- setEmpty(imagesall)


modelfile <- "/Models/Southwest/EfficientNetB5_456_Unfrozen_01_0.58_0.82.h5"

# Obtain predictions for each animal crop
pred<-classifySpecies(animals,modelfile,resize=456,standardize=FALSE,batch_size = 64,workers=8)

# Apply human-readable class name to dataframe
# Classes are stored as text file
# Returns a table with number of crops identified for each species
alldata <- applyPredictions(animals,empty,"/Models/Southwest/classes.txt",pred, counts = TRUE)

# Lastly pool crops to get one prediction per file
alldata <- poolCrops(alldata)

```

## Models
All of our pre-trained classification models can be obtained at [https://]

Geographical regions represented:
* South America
* African Savanna
* Southwest United States

## Installation

#### Requirements
* R >= 4.0 
* Python >= 3.7
* Tensorflow >= 2.5

We recommend running animl on a computer with a dedicated GPU.

#### Python
animl depends on python and will install python package dependencies if they are not available if installed via CRAN. <br> 
However, we recommend setting up a conda environment using the provided config file. 

[Instructions to install conda](https://conda.io/projects/conda/en/latest/user-guide/install/index.html)

The file **animl-env.yml** describes the python version and various dependencies with specific version numbers. 
To create the enviroment, from within the animl directory run the following line in a terminal:
```
conda env create -f animl-env.yml
```
The first line creates the enviroment from the specifications file which only needs to be done once. 
This environment is also necessary for the [python version of animl.](https://pypi.org/project/animl/) 



### Contributors

Kyra Swanson <br>
Mathias Tobler <br> 
Edgar Navarro <br>
Josh Kessler <br>
Jon Kohler <br>
