# Configuration Files

A number of animl's functions can be accessed via config file without the user having to
write custom code.<br>
The following config files are provided and can perform the functions below:

animl.yml - To be run on an image directory, runs the full animl pipeline start to finish
classification.yml  - Only runs species classification inference
train.yml - Sets up a custom model training pipeline with pre-split data.


## animl.yml



```
# directories and model files
image_dir:  "/path/to/image_dir"
working_dir: "/path/to/working_dir"
detector_file: "/path/to/detector.ext"
classifier_file: "/path/to/classifier.ext"
class_list: "/path/to/classlist.csv"
# link_dir: "/path/to/image_dir" without this key, will save to working_dir/Sorted

# file metadata
exif: True
station_dir: -1 

# video processing
parallel: True
frames: 1
fps: None

# inference
device: cuda:0
checkpoint_frequency: 1000
batch_size: 4
num_workers: 8 
file_col_detection: frame
file_col_classification: frame
class_label_col: class

# sort
sort: False
copy: False
```

## classification.yml

## train.yml

