# animl Classification Workflow
#
#
#-------------------------------------------------------------------------------

library(reticulate)
use_condaenv("mlgpu")
library(animl)

imagedir <- "examples/test_data/Southwest"
mdmodel <- "/mnt/machinelearning/megaDetector/md_v5b.0.0_saved_model"
modelfile <- "/mnt/machinelearning/Models/Southwest/2022/EfficientNetB5_456_Unfrozen_05_0.26_0.92.h5"
classes <- "/mnt/machinelearning/Models/Southwest/2022/classes.txt"

animl(imagedir,mdmodel,modelfile,classes)
