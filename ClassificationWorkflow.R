# Orbweaver Workflow
#
# c 2021 Mathias Tobler
# Maintained by Kyra Swanson
#
#
# Changes: identify species for crops first, then merge results with Images

#-------------------------------------------------------------------------------
# Setup
#-------------------------------------------------------------------------------

setwd("/mnt/machinelearning/orbweaver")
library(reticulate)
use_condaenv("mlgpu")
library(animl)
source_python("ImageCropGenerator.py")


# Directories
#peru
imagedir <- "/mnt/mathias/Camera Trap Data Raw/Stations Tahuamanu 2019/"
basedir <- "/mnt/mathias/Camera Trap Data Raw/Stations Tahuamanu 2019/Import/"

#kenya
imagedir <- "/mnt/projects/Stacy-Dawes_Kenya/Images for ML_Oct_20/Loisaba August 2018 - March 2019/ROUND 10/"
workingdir <- "/mnt/projects/Stacy-Dawes_Kenya/Images for ML_Oct_20/Loisaba August 2018 - March 2019/ROUND 10/Working Directory/"

workingdir = "C:\\Users\\tswanson\\projects\\"

imagedir <- "\\\\10.24.17.135\\Projects\\Stacy-Dawes_Kenya\\Images for ML_Oct_20\\Loisaba August 2018 - March 2019\\ROUND 10"

build_Directories(workingdir)


#-------------------------------------------------------------------------------
# Extract EXIF data
#-------------------------------------------------------------------------------

# read exif data for images
images<- extractFiles(imagedir)


# get camera name from folder name
# adapt this to folder naming
basedepth=length(strsplit(imagedir,split="/")[[1]])+1
images$Region<-"Loisaba"
images$Station<-sapply(images$Directory,function(x)strsplit(x,"/")[[1]][basedepth])
images$Camera<-sapply(images$Directory,function(x)strsplit(x,"/")[[1]][basedepth+1])


#if from sorted directory
images$location<-sapply(images$Directory,function(x)strsplit(x,"/")[[1]][basedepth])
images$species<-sapply(images$Directory,function(x)strsplit(x,"/")[[1]][basedepth+1])


# Create unique image name from camera name and date time
images$NewName=paste(images$Station,images$Camera,format(images$DateTime,format="%Y%m%d_%H%M%S"),images$FileName,sep="_")
# alternatively, use the same name
#images$NewName=images$FileName


#Process videos, extract frames for ID
videos<-images[tools::file_ext(images$FileName)!="JPG",]
videoframes<-imagesFromVideos(videos$FilePath,outdir=tempdir,frames=5,parallel=T,nproc=12)


#+++++++++++++++++++++
# Check camera codes for typos, etc
sort(unique(images$Camera))
t<-unique(images[,c("Station","Camera")])
t

#check dates
datecheck<-data.frame(Camera=sort(unique(images$Camera)),Start=as.POSIXct(tapply(images$DateTime,images$Camera,min), origin = "1970-01-01"),End=as.POSIXct(tapply(images$DateTime,images$Camera,max), origin = "1970-01-01"))
datecheck$Days<-as.numeric(round(difftime(datecheck$End,datecheck$Start,units="days"),0)+1)

datecheck[datecheck$Start<as.POSIXct("2019-01-01"),]
datecheck
#+++++++++++++++++++++



#=====================
# save point
write.csv(images,file=paste0(datadir,imagefile),row.names = F,quote = F)
write.csv(videoframes,file=paste0(datadir,videoframefile),row.names = F,quote = F)

# load point
images<-read.csv(file=paste0(datadir,imagefile),stringsAsFactors = F)
images$DateTime<-as.POSIXct(images$DateTime)
videoframes<-read.csv(file=paste0(datadir,videoframefile),stringsAsFactors = F)
#=====================


# Combine image and video data
imagesall<-rbind(data.frame(FilePathOrig=images$FilePath,FilePath=images$FilePath),
                 data.frame(FilePathOrig=videoframes$videofile,FilePath=videoframes$videoframe))
imagesall<-merge(imagesall,images,by.x="FilePathOrig",by.y="FilePath")
if(is.null(imagesall$Station))imagesall$Station<-1


#-------------------------------------------------------------------------------
# MegaDetector
#-------------------------------------------------------------------------------

#load the MegaDetector model
mdsession<-loadMDModel(mdmodel)

#+++++++++++++++++++++
#classify a single image to make sure everything works before continuing
mdres<-classifyImageMD(mdsession,images$FilePath[1])
plotBoxes(mdres,minconf = 0.5)
#+++++++++++++++++++++

#classify all images
mdres<-classifyImagesBatchMD(mdsession,images$FilePath,resultsfile=paste0(datadir,mdresults),checkpoint = 2500)
mdresflat<-flattenBoxesMDSimple(mdres)

#=====================
# save point
write.csv(mdresflat,paste0(datadir,cropfile),row.names=F,quote = F)
save(mdres,file=paste0(datadir,mdresults))

# load point
mdresflat<-read.csv(paste0(datadir,cropfile),stringsAsFactors = F)
load(file = paste0(datadir,mdresults))
#=====================

#combine image and box data
imagesall<-merge(images,mdresflat,by="FilePath")
imagesall<-imagesall[order(imagesall$Station,imagesall$Camera,imagesall$DateTime),]

#filter out any low-confidence crops, confidence threshold can be changed
imagesall$max_detection_category[imagesall$max_detection_conf<0.1]<-0

#-------------------------------------------------------------------------------
# Species Classifier
#-------------------------------------------------------------------------------

#modelfile <- "Models/Peru/EfficientNetB5_10000_456_Unfrozen_09_0.68_0.86_Peru"
#modelfile <- "/mnt/machinelearning/Camera Trap Image Analysis/Models/EfficientNetB5_100000_456_Unfrozen_04_1.09_0.81_Kenya"

#list of species the model is trained to recognize
classes<-read.table(paste0(modelfile,".txt"),stringsAsFactors = F)$x

#add new columns for classification
imagesall$prediction <-NA
imagesall$confidence <-NA
imagesall$Common<-NA

#only classify images where MD predicted animals
imagesallanimal<-imagesall[imagesall$max_detection_category==1,]

# use on-the-fly image crop generator, no need to save crop files
pred<-classifyCropsSpecies(imagesallanimal,paste0(modelfile,".h5"),resize=456,standardize=FALSE,batch_size = 64,workers=8)
imagesallanimal$Common<-classes[apply(pred,1,which.max)]

# display classification results
table(classes[apply(pred,1,which.max)])

#=====================
# save point
save(pred,file=paste0(datadir,predresults))
#=====================

#-------------------------------------------------------------------------------
# Process Species Results
#-------------------------------------------------------------------------------

# classify sequences
imagesallanimal <- classifySequences(imagesallanimal,predictions,classes,emptycol,maxdiff=60)


#add empty and people back in
emptydata<-imagesall[imagesall$max_detection_category!=1 & !(imagesall$FilePathOrig %in% imagesallanimal$FilePathOrig),]
emptydata$prediction<-emptydata$md_class
emptydata$confidence<-emptydata$md_confidence
emptydata$Common<-"Empty"
#MD classified as empty
emptydata$md_confidence[emptydata$md_class==0]<- 1-emptydata$max_detection_conf[emptydata$md_class==0]
emptydata$confidence[emptydata$md_class==0]<-1-emptydata$max_detection_conf[emptydata$md_class==0]
#MD classified as human or vehicle
emptydata[emptydata$max_detection_category==2,]$Common="Human"
emptydata[emptydata$max_detection_category==3,]$Common="Vehicle"


#merge animal data and empty data
alldata<-rbind(emptydata,imagesallanimal)
alldata<-alldata[order(alldata$Station,alldata$DateTime),]


#=====================
# save results
write.csv(alldata,paste0(datadir,resultsfile),row.names = F,quote = F)

# load results
alldata<-read.csv(paste0(datadir,resultsfile),stringsAsFactors = F)
#=====================



#+++++++++++++++++++++
#debug issues
im<-"EK000004.JPG"
alldata[basename(alldata$FilePathOrig)==im,]
plotBoxesFlat(alldata[basename(alldata$FilePathOrig)==im,])

emptydata[basename(emptydata$FilePathOrig)==im,]
mdresflat[basename(mdresflat$file)==im,]
round(predsort[which(basename(imagesallanimal$FilePathOrig)==im),],2)

i<-min(which(basename(imagesallanimal$FilePathOrig)==im))

#images where MD detected animal but species classifier returned empty
nrow(alldata[alldata$max_detection_category==1 & alldata$Common=="Empty",])
#+++++++++++++++++++++

#-------------------------------------------------------------------------------
# Symlinks
#-------------------------------------------------------------------------------

# pull first class for each image
image_singleclass<-unique(alldata[,c("FilePathOrig","NewName","Common","confidence")])

# place low-confidence images into "Unknown" category
image_singleclass$Common[image_singleclass$confidence<0.5 & !(image_singleclass$Common %in% c("Empty","Human","Vehicle"))]<-"Unknown"

image_singleclass$link<-paste0(symlinkdir,"/",image_singleclass$Common,"/",image_singleclass$NewName)

# create species directories
for(s in unique(image_singleclass$Common)){
  if(!dir.exists(paste0(symlinkdir,"/",s)))dir.create(paste0(symlinkdir,"/",s),recursive=T)}

#link images to species directory
mapply(file.link,image_singleclass$FilePathOrig,image_singleclass$link)

#delete simlinks
#sapply(image_singleclass$link,file.remove)

#-------------------------------------------------------------------------------
# Export to Camera Base
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
# Export to Zooniverse
#-------------------------------------------------------------------------------

data = "/mnt/mathias/Camera Trap Data Raw/BIG GRID/September 2021/Data/ImportZooniverse.csv"
alldata = read.csv(data)

# Confirm project name and subject set name
# where the images will be added
projectname<-"16850"
subjectset<-"Tambopata Nov 2021"

#take only top classification for each image (adjust later for multiple classifications)
topchoice = alldata[order(alldata[,'NewName'],-alldata[,'confidence1']),]
topchoice = topchoice[!duplicated(topchoice$NewName),]

#change species name to the one used on Zooniverse
zooconvert <- read.csv("/mnt/machinelearning/Amazonia/Zooniverse_SpeciesList.csv")
topchoice$zoospecies<-zooconvert[match(topchoice$Common,zooconvert$Common),"ZooniverseCode"]


tempdir = "/mnt/mathias/Camera Trap Data Raw/BIG GRID/September 2021/Zooniverse/"

images<-topchoice[tools::file_ext(topchoice$FileName)=="JPG",]

connect_to_Panoptes("tkswanson","ShikokuInu9388!")


upload_to_Zooniverse(projectname,99162,alldata[1:12,],tempdir)

source_python("ZooniverseFunctions.py")
create_SubjectSet(projectname,subjectset)

upload_to_Zooniverse(projectname,99162,alldata[10:30,],tempdir,maxSeq=3,maxTime=15)

