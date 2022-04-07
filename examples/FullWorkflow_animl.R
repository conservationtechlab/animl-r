# animl Classification Workflow
#
# c 2021 Mathias Tobler
# Maintained by Kyra Swanson
#
#
#-------------------------------------------------------------------------------
# Setup
#-------------------------------------------------------------------------------

library(reticulate)
use_condaenv("mlgpu")
library(tensorflow)
library(keras)
library(animl)
library(jpeg)
library(dplyr)

imagedir <- "/mnt/projects/Local_Lion/delaRosa_Cougar/BF/Animal/"

#create global variable file and directory names
setupDirectory(imagedir)

#===============================================================================
# Extract EXIF data
#===============================================================================

# Read exif data for all images within base directory
files <- buildFileManifest(imagedir,outfile = filemanifest)

# Set Region/Site/Camera names
files <- setLocation(files, imagedir, adjust = -2)

# Process videos, extract frames for ID
allframes<-imagesFromVideos(files,outdir=vidfdir,outfile=imageframes,frames=5,parallel=T,nproc=12)


#===============================================================================
# MegaDetector
#===============================================================================

# Load the MegaDetector model
mdsession<-loadMDModel("/mnt/machinelearning/megaDetector/megadetector_v4.1.pb")

#+++++++++++++++++++++
# Classify a single image to make sure everything works before continuing
testMD(imagesall,mdsession)
#+++++++++++++++++++++

mdres <- detectObjectBatch(mdsession,allframes$Frame, outfile = mdresults, checkpoint = 2500)

allframes <- parseMD(allframes, mdres)

#null out low-confidence crops
#check the "empty" folder, if you find animals, lower the confidence or do not run
allframes$max_detection_category[allframes$max_detection_conf<0.1] <- 0

#select animal crops for classification
animals <- allframes[allframes$max_detection_category==1,]
empty <- setEmpty(allframes)


#===============================================================================
# Species Classifier
#===============================================================================

modelfile <- "/mnt/machinelearning/Models/Southwest/EfficientNetB5_456_Unfrozen_01_0.58_0.82.h5"

pred <- classifySpecies(animals,modelfile,resize=456,standardize=FALSE,batch_size = 64,workers=8)

alldata <- applyPredictions(animals,empty,"/mnt/machinelearning/Models/Southwest/classes.txt",pred,
                            outfile = predresults, counts = TRUE)


#===============================================================================
# Post-processing
#===============================================================================

# Classify sequences
#pooledData <- classifySequence(mdanimals,pred,classes,18,maxdiff=60)
alldata <- poolCrops(alldata)

# Run before pooling if you want to allow multiple classifications for images
symlinkClassification <- function(alldata,linkdir)

# Delete symlinks
#sapply(alldata$Link,file.remove)

#===============================================================================
# Export to Camera Base
#===============================================================================

species_list<-read.csv("R:/PeruTrainingData/SpeciesID/species_list_full.csv",stringsAsFactors = F)

alldata$Species<-species_list[match(alldata$Common,species_list$Common),"Species"]

export<-data.frame(ID=1:nrow(alldata),
                   Station_Code=alldata$Camera,
                   Camera_Code1=alldata$Camera,
                   Camera_Code2="",
                   Date=as.Date(alldata$DateTime),
                   Time=strftime(alldata$DateTime, format="%H:%M:%S"),
                   Common=alldata$Common,
                   Species=alldata$Species,
                   perc_species=alldata$confidence1,
                   Count=1,
                   Sex="unknown",
                   Marked=0,
                   ImageNew1=alldata$NewName,
                   ImageNew2="",
                   SourceFile1=alldata$FilePath,
                   SourceFile2="",stringsAsFactors = F)

export<-export[!duplicated(export$SourceFile1),]
export[export$Common=="Empty",]$Species="Blank"
dim(export[export$Date>"2018-06-18",])

write.table(export,file="R:/BrazilNutImportCB.txt",sep="\t",row.names = F,quote = F)



