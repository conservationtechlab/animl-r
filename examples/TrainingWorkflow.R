############################################
#Training workflow to train image classifier
#Workflow for EfficientNet
#based on cropped images from MegaDetector
#Requires a Python environment with Tensorflow
#



library(reticulate)
reticulate::use_condaenv("mlgpu")
library(animl)