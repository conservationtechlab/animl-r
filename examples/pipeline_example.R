# animl Classification Workflow
#
#
#-------------------------------------------------------------------------------

library(animl)

setupEnv() #setup python environment using conda

imagedir <- "/home/kyra/animl/examples/test_data/Test_052323"
mdmodel <- "/mnt/machinelearning/megaDetector/md_v5b.0.0_saved_model"
modelfile <- "/mnt/machinelearning/Models/Peru/EfficientNetB5_10000_UF_0.86_Peru-Amazon.h5"
classes <- "/mnt/machinelearning/Models/Peru/EfficientNetB5_10000_UF_0.86_Peru-Amazon.txt"

animl(imagedir,mdmodel,modelfile,classes)
