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
library(animl)
library(magrittr)


imagedir <- "/home/kyra/animl/examples/test_data/Southwest"

#create global variable file and directory names
setupDirectory(imagedir)


#load data if needed
#mdres <- loadData(mdresults)
#alldata <- loadData(predresults)
#alldata <- loadData(classifiedimages)

#===============================================================================
# Extract EXIF data
#===============================================================================

# Read exif data for all images within base directory
files <- buildFileManifest(imagedir)

#build new name
basedepth=length(strsplit(basedir,split="/")[[1]])-1


files$Region<-sapply(files$Directory,function(x)strsplit(x,"/")[[1]][basedepth])
files$Site<-sapply(files$Directory,function(x)strsplit(x,"/")[[1]][basedepth+1])
files$Camera<-sapply(files$Directory,function(x)strsplit(x,"/")[[1]][basedepth+2])


#files must have a new name for symlink
files$NewName=paste(files$Region,files$Site,format(files$DateTime,format="%Y%m%d_%H%M%S"),files$FileName,sep="_")
files$NewName=files$FileName

# Process videos, extract frames for ID
allframes<-imagesFromVideos(files,outfile=imageframes,frames=5,parallel=T,nproc=12)


#===============================================================================
# MegaDetector
#===============================================================================

# Load the MegaDetector model
mdsession <- loadMDModel("/mnt/machinelearning/megaDetector/megadetector_v4.1.pb")

#+++++++++++++++++++++
# Classify a single image to make sure everything works before continuing
testMD(allframes,mdsession)
#+++++++++++++++++++++

mdres <- detectObjectBatch(mdsession,allframes$Frame, resultsfile = mdresults, checkpoint = 2500)

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


# Classify sequences / select best prediction
#mdanimals <- classifySequence(mdanimals,pred,classes,18,maxdiff=60)
alldata <- poolCrops(alldata, outfile = classifiedimages)


#===============================================================================
# Symlinks
#===============================================================================

#symlink species predictions
alldata <- symlinkClasses(alldata, linkdir)

mapply(file.link, alldata$FilePath, alldata$Link)

#symlink MD detections only
symlinkMD(alldata,linkdir)

#delete simlinks
sapply(alldata$Link,file.remove)

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

#===============================================================================
# Export to Zooniverse
#===============================================================================

source_python("ZooniverseFunctions.py")

data = "/mnt/mathias/Camera Trap Data Raw/BIG GRID/September 2021/Data/ImportZooniverse.csv"
alldata = read.csv(data)

imagesallanimal<-alldata[!(alldata$Common %in% c("Empty","empty","human","Human","vehicle","Vehicle")),]

# Confirm project name and subject set name
# where the images will be added
projectname<-"2789"
subjectset<-"Loisaba Round 12"

#take only top classification for each image (adjust later for multiple classifications)
topchoice2 = imagesallanimal[order(imagesallanimal[,'FileName'],-imagesallanimal[,'confidence']),]
topchoice2 = topchoice2[!duplicated(topchoice$NewName),]

#change species name to the one used on Zooniverse
zooconvert <- read.csv("Models/Kenya/Zooniverse_SpeciesList.csv")
topchoice2$ZooniverseCode<-zooconvert[match(topchoice2$Common,zooconvert$Common),"ZooniverseCode"]


toupload<-topchoice2[topchoice2$ZooniverseCode!="Human/Vehicle",]
toupload<-toupload[toupload$ZooniverseCode!="Nothing Here",]


upload_to_Zooniverse(projectname,101729,toupload[58800,],tempdir)


create_SubjectSet(projectname,subjectset)

upload_to_Zooniverse(projectname,99162,alldata[10:30,],tempdir,maxSeq=3,maxTime=15)

