#===============================================================================
# Export to Zooniverse
#===============================================================================
library(reticulate)
animlpy <- import("animl")


alldata = read.csv("/mnt/mathias/Camera Trap Data Raw/BIG GRID/September 2021/Data/ImportZooniverse.csv")

# Filter out empty images
imagesallanimal <- alldata[!(alldata$Common %in% c("Empty","empty","human","Human","vehicle","Vehicle")),]

# Make sure there is only one entry per image 
topchoice2 = topchoice2[!duplicated(topchoice$NewName),]

# Confirm project name from Zooniverse
# Create new subject set name
projectname <- "2789"
subjectset <- "Summer 2022"


# Change species name to the one used on Zooniverse
zooconvert <- read.csv("Models/Kenya/Zooniverse_SpeciesList.csv")
topchoice2$ZooniverseCode<-zooconvert[match(topchoice2$Common,zooconvert$Common),"ZooniverseCode"]

connectToZooniverse(username,password)

create_SubjectSet(projectname,subjectset)

upload_to_Zooniverse(projectname,99162,alldata[10:30,],tempdir,maxSeq=3,maxTime=15)