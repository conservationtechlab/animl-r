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

imagedir <- "examples/test_data/Southwest"

#create global variable file and directory names
setupDirectory(imagedir)

# Build file manifest for all images and videos within base directory
files <- buildFileManifest(imagedir, outfile = filemanifest, exif = TRUE)
#===============================================================================
# Add Project-Specific Info
#===============================================================================

#build new name
basedepth=length(strsplit(imagedir,split="/")[[1]])-1

files$Region<-sapply(files$Directory,function(x)strsplit(x,"/")[[1]][basedepth+1])
files$Site<-sapply(files$Directory,function(x)strsplit(x,"/")[[1]][basedepth+2])
files$Camera<-sapply(files$Directory,function(x)strsplit(x,"/")[[1]][basedepth+3])

#files must have a new name for symlink
files$UniqueID = round(runif(nrow(files), 1, 99999),0)
files$UniqueName = paste(files$Site,files$Camera,format(files$DateTime,format="%Y-%m-%d_%H%M"),files$UniqueID,sep="_")
files$UniqueName = paste0(files$UniqueName, ".", tolower(tools::file_ext(files$FileName)))


files$UniqueName=paste(files$Region, files$Site, files$Camera, files$FileName, sep="_")

#files$UniqueName=files$FileName


# Process videos, extract frames for ID
allframes <- imagesFromVideos(files, outdir = vidfdir, outfile=NULL, frames=5, parallel=T, workers=parallel::detectCores())

#===============================================================================
# MegaDetector
#===============================================================================
# Most functions assume MegaDetector version 5. If using an earlier version of 
# MD, specify with argument 'mdversion'.

# Load the MegaDetector model
mdsession <- loadMDModel("//10.24.17.53/machinelearning/megaDetector/md_v5b.0.0_saved_model")
mdsession <- loadMDModel("/mnt/machinelearning/megaDetector/md_v4.1.pb")

#+++++++++++++++++++++
# Classify a single image to make sure everything works before continuing
testMD(allframes[16,]$Frame, mdsession, mdversion = 5, minconf = 0.7)
#+++++++++++++++++++++
mdres <- detectObjectBatch(mdsession, mdversion = 4, allframes$Frame, outfile = NULL, checkpoint = 2500)
mdres <- detectObjectBatch(mdsession, allframes$Frame, outfile = NULL, checkpoint = 2500)

y <- parseMD(mdres, manifest = allframes)
z <- y[(y$conf > 0.5 | is.na(y$conf)),]

#select animal crops for classification

animals <- getAnimals(z)
empty <- getEmpty(z)

#===============================================================================
# Species Classifier
#===============================================================================

modelfile <- "/mnt/machinelearning/Models/Southwest/2022/EfficientNetB5_456_Unfrozen_05_0.26_0.92.h5"
modelfile <- "/mnt/machinelearning/Models/Kenya/2022/EfficientNetB5_456_Unfrozen_04_0.60_0.89.h5"
modelfile <- "/mnt/machinelearning/Models/Southwest/Extended/EfficientNetB5_456_Unfrozen_10_0.30_0.94_final.h5"


pred <- predictSpecies(animals, modelfile, batch = 64, workers = 8)

animals <- applyPredictions(animals, pred, "/mnt/machinelearning/Models/Southwest/2022/classes.txt", 
                            outfile = predresults, counts = TRUE)

#rejoin animal and empty data splits
manifest <- rbind(animals,empty)

# Classify sequences / select best prediction
best <- bestGuess(manifest, sort = "conf", parallel = T, workers = 12, shrink = TRUE)

#mdanimals <- classifySequence(mdanimals,pred,classes,18,maxdiff=60)

#===============================================================================
# Symlinks
#===============================================================================

#symlink species predictions
alldata <- symlinkSpecies(best, linkdir, outfile = resultsfile)

#symlink MD detections only
symlinkMD(best,linkdir)

#===============================================================================
# Export to Camera Base
#===============================================================================

species_list<-read.csv("",stringsAsFactors = F)

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

write.table(export,file="",sep="\t",row.names = F,quote = F)

#===============================================================================
# Export to Zooniverse
#===============================================================================

source_python("ZooniverseFunctions.py")

imagesallanimal <- alldata[!(alldata$Common %in% c("Empty","empty","human","Human","vehicle","Vehicle")),]

# Confirm project name and subject set name
# where the images will be added
projectname <- ""
subjectset <- ""


create_SubjectSet(projectname,subjectset)

#take only top classification for each image (adjust later for multiple classifications)
topchoice = imagesallanimal[order(imagesallanimal[,'FileName'],-imagesallanimal[,'confidence']),]
topchoice = topchoice[!duplicated(topchoice$NewName),]

#change species name to the one used on Zooniverse
zooconvert <- read.csv("")
topchoice2$ZooniverseCode<-zooconvert[match(topchoice2$Common,zooconvert$Common),"ZooniverseCode"]


toupload <- topchoice2[topchoice2$ZooniverseCode!="Human/Vehicle",]
toupload <- toupload[toupload$ZooniverseCode!="Nothing Here",]


upload_to_Zooniverse(projectname,subjectset,toupload,tempdir)


upload_to_Zooniverse(projectname,subjectset,toupload,tempdir,maxSeq=3,maxTime=15)