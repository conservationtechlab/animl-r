---
layout: default
title: Home
description: Developing open-source technology and machine learning tools for wildlife conservation and ecological research
---

# Getting Started

<br>

{: #about}
## About AniML

**Version 3.4.0**

[GitHub Repository](https://github.com/conservationtechlab/animl-r)

The AniML package is available in Python and R for AI-assisted camera trap image processing.

The AniML package provides functions for ingesting raw image and video files and outputs predictions for species using region-specific species classifier models. We provide several species models including for the African Savanna, the Peruvian Amazon, the Andes mountains, and the Western US. AniML provides the results in a number of export formats, including TimeLapse and CamTrapDP. The package also includes AI-based re-identification tools and custom species model training.

AniML-R uses [AniML-py](https://conservationtechlab.github.io/animl-py) to run the machine learning models. Most functions pass their
arguments to the animl-py function of the same name, and pandas DataFrames, numpy arrays and Python dictionaries are converted to
R data.frames, arrays and named lists by [reticulate](https://rstudio.github.io/reticulate/).

<br>

{: #installation}
### Installation

1. Install the package from CRAN.

    ```r
    install.packages("animl")
    ```

2. Set up the Python environment. This installs Python 3.12 if needed, creates a virtual environment called `animl_env`
   and installs the matching version of animl-py into it.

    ```r
    library(animl)
    animl::animl_install()
    ```

3. Restart your R session and load the package. animl loads the `animl_env` environment automatically.

    ```r
    library(animl)
    check_animl_py()
    ```

Animl-r can also be installed by downloading the repository, opening the animl.Rproj file in RStudio and selecting Build -> Install Package.

<br>

{: #requirements}
### Requirements

AniML-R uses AniML-py to interface with the machine learning frameworks PyTorch, Ultralytics and ONNX.
Upon setup, AniML-R will install AniML-py to a virtual environment.

<ins>Required dependencies</ins>:
* Python 3.12
* reticulate
* animl-py 3.4.x

__Recommended__:
* [ExifTool](https://exiftool.org/)
* [CUDA/cuDNN](https://developer.nvidia.com/cuda/toolkit) (for GPU)

We recommend using AniML with a GPU. To use with an Nvidia GPU, be sure to install the CUDA-compatible
version of [PyTorch](https://pytorch.org/get-started/locally/) in the `animl_env` environment.

<br><br>

---
{: #examples}
# Examples and Usage

---
### Species Classification Inference
{: #inference}

The functionality of animl can be parcelated into its individual functions to suit your data and scripting needs.

1. It is recommended that you use the AniML Working Directory for storing intermediate steps.
   `WorkingDirectory()` creates variables holding the output file paths in the given environment.

    ```r
    library(animl)
    WorkingDirectory('/path/to/save/data', globalenv())
    ```

2. Build the file manifest of your given directory. This will find both images and videos.

    ```r
    files <- build_file_manifest('/path/to/images',
                                 out_file=filemanifest_file,
                                 exif=TRUE,
                                 data_timezone='America/Los_Angeles')
    ```

    The argument `data_timezone` indicates the timezone in which the data was collected, so timestamps are correctly
    interpreted relative to the local timezone.

3. If there are videos, extract individual frames for processing.
   Select either the number of frames or fps using the arguments.
   The other option can be set to NULL or removed.

    ```r
    allframes <- extract_frames(files, frames=3, out_file=imageframes_file, parallel=TRUE)
    ```

4. Pass all images into MegaDetector. We recommend [MDv5a](https://github.com/agentmorris/MegaDetector/releases/download/v5.0/md_v5a.0.0.pt).
   The function `parse_detections()` will convert the detector output to a data.frame and merge detections with the original file manifest, if provided.

    ```r
    detector <- load_detector('/path/to/mdmodel.pt', model_type='mdv5', device='cuda:0')

    mdresults <- detect(detector,
                        allframes,
                        resize_width=1280,
                        resize_height=1280,
                        letterbox=TRUE,
                        file_col='filepath',
                        device='cuda:0',
                        checkpoint_path=mdraw_file,
                        checkpoint_frequency=1000)

    detections <- parse_detections(mdresults, manifest=allframes, out_file=detections_file)
    ```

5. For speed and efficiency, extract the empty/human/vehicle detections before classification.

    ```r
    animals <- get_animals(detections)
    empty <- get_empty(detections)
    ```

6. Classify using the appropriate species model.

    ```r
    classifier <- load_classifier('/path/to/model.pt', '/path/to/classlist.csv', device='cuda:0')

    pred_raw <- classify(classifier,
                         animals,
                         resize_width=480,
                         resize_height=480,
                         file_col='filepath',
                         batch_size=4,
                         out_file=predictions_file)
    ```

7. Apply labels from class list with or without utilizing timestamp-based sequences.

    ```r
    manifest <- single_classification(animals, empty, pred_raw, classifier$classes$class)
    ```

    or, after defining a station column named "station",

    ```r
    manifest <- sequence_classification(animals,
                                        empty,
                                        pred_raw,
                                        classifier$classes$class,
                                        station_col='station',
                                        empty_class='empty',
                                        maxdiff=60)
    ```

8. (OPTIONAL) Save the required columns to csv and then use it to create json for TimeLapse compatibility.

    ```r
    csv_loc <- export_timelapse(manifest, '/path/to/output', only_animal=TRUE)
    export_megadetector(manifest, out_file='final_result.json', detector='MegaDetector v5a')
    ```

9. (OPTIONAL) Create links within a given directory, sorted by species, for file browser access.

    ```r
    manifest <- export_folders(manifest, out_dir=linkdir, out_file=results_file)
    ```

---
### Classifier Model Training
{: #training-example}

1. Assuming a file manifest of training data with species labels, first split the data into training, validation and test splits.
   This function splits each label proportionally by the given fractions, by default 0.8 training, 0.1 validation, 0.1 test.

    ```r
    splits <- export_train_val_test(manifest,
                                    label_col='species',
                                    out_dir='path/to/save/data/',
                                    val_size=0.2,
                                    test_size=0.1,
                                    seed=42)
    train <- splits[[1]]
    val <- splits[[2]]
    test <- splits[[3]]
    ```

2. Set up the training configuration file. Specify the paths to the data splits from the previous step. See the [config README](https://github.com/conservationtechlab/animl-py/blob/main/src/animl/config/README.md).

3. Using the config file, begin training.

    ```r
    train_classifier('/path/to/config.yml')
    ```

    Every 10 epochs (or define custom 'checkpoint_frequency'), the model will be checkpointed to the 'experiment_folder' parameter in the config file, and will contain performance metrics for selection.

4. Testing of a model checkpoint can be done with `test_classifier()`. Add an 'active_model' parameter to the config
   file that contains the path of the checkpoint to test. This will produce a confusion matrix of the test dataset as well
   as a csv containing predicted and ground truth labels for each image.

    ```r
    test_classifier('/path/to/config.yml')
    ```

---
### Re-Identification
{: #re-id-example}

Re-identification compares MiewID embeddings of animal crops to find images of the same individual.

```r
miew <- load_miew('/path/to/miewid.bin', device='cuda:0')
embeddings <- extract_miew_embeddings(miew, animals, batch_size=4)

dist <- compute_distance_matrix(embeddings, embeddings, metric='cosine')
```

<br><br>

---
### Exports
{: #export-example}

Results can be exported for review or for use in other software. See the [Export](#export) reference for all formats.

```r
manifest <- export_camtrapR(manifest, out_dir=linkdir, out_file=results_file)
export_coco(manifest, class_dict=list('0'='empty', '1'='deer'), out_file='results.json')
```

<br><br>

---
# API Reference

<br>

## Environment Setup
{: #environment}
<br>

### animl_install(envname="animl_env", python_version="3.12")
{: #animl_install}

Installs Python if necessary, creates the Python environment and installs the animl-py version matching this package.
If the environment already exists, it is reused and animl-py is installed or updated as needed.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `envname` | character | "animl_env" | Name of the Python environment |
| `python_version` | character | "3.12" | Version of Python to install |

**Returns:** invisible `NULL`

<br><br>

### animl_install_instructions()
{: #animl_install_instructions}

Prints instructions for setting up the Python environment, including a manual virtualenv/pip alternative to `animl_install()`.

**Returns:** invisible `NULL`

<br><br>

### load_animl(envname="animl_env", python_version="3.12", interactive=TRUE)
{: #load_animl}

Loads the Python environment and animl-py. This runs automatically when the package is loaded; call it directly to load a
different environment.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `envname` | character | "animl_env" | Name of the Python environment |
| `python_version` | character | "3.12" | Version of Python expected |
| `interactive` | logical | TRUE | Only print progress messages if TRUE |

**Returns:** none

<br><br>

### check_animl_py()
{: #check_animl_py}

Checks whether animl-py can connect to ExifTool and CUDA.

**Returns:** `list` with three logical values: `exiftool`, `torch_cuda` and `onnx_cuda`

<br><br>

### check_python(python_version="3.12", initialize=TRUE)
{: #check_python}

Checks that the available Python version is compatible with the current version of animl-py, installing it if not.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `python_version` | character | "3.12" | Version of Python to check for |
| `initialize` | logical | TRUE | Initialize Python via reticulate when checking |

**Returns:** none

<br><br>

### create_pyenv(envname="animl_env", python_version="3.12")
{: #create_pyenv}

Installs Python if necessary, creates the virtual environment and installs animl-py into it.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `envname` | character | "animl_env" | Name of the virtual environment to create |
| `python_version` | character | "3.12" | Python version to add to the environment |

**Returns:** invisible `TRUE` on success

<br><br>

### update_animl_py(envname="animl_env")
{: #update_animl_py}

Installs the animl-py version matching this package into the given environment.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `envname` | character | "animl_env" | Name of the Python environment |

**Returns:** none

<br><br>

### delete_pyenv(envname="animl_env")
{: #delete_pyenv}

Deletes the Python environment.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `envname` | character | "animl_env" | Python environment to remove |

**Returns:** none

<br><br>

---
## Data Ingestion and Processing
{: #data-ingestion}
<br>

### WorkingDirectory(workingdir, pkg.env)
{: #workingdirectory}

Creates a folder called "Animl-Directory" within `workingdir`, with "Sorted" and "Plots" subfolders,
and assigns variables containing output file paths to `pkg.env`, typically `globalenv()`.

* `filemanifest_file` = "FileManifest.csv", typically used with `build_file_manifest()`
* `imageframes_file` = "ImageFrames.csv", typically used with `extract_frames()`
* `mdraw_file` = "MD_Raw.json", typically used with `detect()`
* `detections_file` = "Detections.csv", typically used with `parse_detections()`
* `predictions_file` = "Predictions.csv", typically used with `classify()`
* `results_file` = "Results.csv", typically used with the export functions
* `linkdir` = "Sorted/", typically used as the `out_dir` of `export_folders()`
* `visdir` = "Plots/", typically used as the `out_dir` of `plot_all_bounding_boxes()`

| Parameter | Type | Default | Description |
|---|---|---|---|
| `workingdir` | character | required | Existing directory in which to create "Animl-Directory" |
| `pkg.env` | environment | required | Environment to create the path variables in, e.g. `globalenv()` |

**Returns:** none

<br><br>

### build_file_manifest(image_dir, exif=TRUE, out_file=NULL, ...)
{: #build_file_manifest}

Traverse a directory and find image/video files and gather metadata.

To correctly adjust timestamps from exif data, the argument `data_timezone` should be set to the timezone in which the data was collected.
If you are unsure of the timezone, you can list all with `OlsonNames()` to find the best match, or leave as NULL to default to the local timezone.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `image_dir`       | character | required | Path to image/video directory |
| `exif`            | logical   | TRUE  | Returns date and time info from exif data |
| `out_file`        | character | NULL  | File path to which the data.frame should be saved |
| `data_timezone`   | character | NULL  | Timezone of the data, e.g. 'UTC', 'America/New_York'; defaults to local timezone if NULL |
| `station_depth`   | integer   | NULL  | Depth of station directory from the image_dir root in file path, if applicable.* |
| `camera_depth`    | integer   | NULL  | Depth of camera directory from the image_dir root in file path, if applicable.* |
| `recursive`       | logical   | TRUE  | Recursively search through all child directories |

**Returns:** `data.frame` — file manifest with the following columns:
* filepath
* filename
* extension
* width
* height
* createdate (if exif = TRUE, POSIXct)
* filemodifydate (if exif = TRUE, POSIXct)
* datetime (if exif = TRUE, contains createdate or filemodifydate as a fallback, POSIXct)
* station (if station_depth is not NULL)
* camera (if camera_depth is not NULL)

\* For `station_depth`, if file paths are in the format "image_dir/station/date/file.jpg",
`station_depth` would be 1 (0 indexed). If NULL, station column will not be created.
Likewise for `camera_depth`, if file paths are in the format "image_dir/station/camera/date/file.jpg",
`camera_depth` would be 2 (0 indexed). If NULL, camera column will not be created.

<br><br>

### active_times(manifest_dir, file_col="filepath", camera_depth=0, timestamp_col="datetime")
{: #active_times}

Get start and stop dates for each camera folder.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `manifest_dir`    | data.frame | required      | File manifest with file paths and timestamps |
| `file_col`        | character  | "filepath"    | Column in manifest to use for file paths |
| `camera_depth`    | integer    | 0             | Directory depth from which to split cameras, with 0 being the root of the manifest directory |
| `timestamp_col`   | character  | "datetime"    | Column name representing the timestamp in format "%Y-%m-%d %H:%M:%S" |

**Returns:** `data.frame` with a row for each camera and the earliest and latest timestamp of data taken at that camera

<br><br>

### sequence_calculation(manifest, station_col, sort_columns=NULL, file_col="filepath", timestamp_col="datetime", maxdiff=60)
{: #sequence_calculation}

Simple sequence calculation based on time differences between consecutive images from the same station. <br>
Unlike `sequence_classification()`, does not apply any classification or labeling to the sequences.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `manifest`        | data.frame        | required      | File manifest with station and timestamp columns |
| `station_col`     | character         | required      | Column name representing the station or camera |
| `sort_columns`    | character vector  | NULL          | Columns to sort by before calculating sequences. Defaults to `station_col` and `timestamp_col` |
| `file_col`        | character         | "filepath"    | Column name representing the file path |
| `timestamp_col`   | character         | "datetime"    | Column name representing the timestamp in format "%Y-%m-%d %H:%M:%S" |
| `maxdiff`         | numeric           | 60            | Maximum time difference in seconds between consecutive images to be considered part of the same sequence |

**Returns:** `data.frame` — the sorted manifest with an additional `sequence` column, with a unique number for each sequence

<br><br>

### extract_frames(files, frames=5, fps=NULL, out_file=NULL, out_dir=NULL, file_col="filepath", parallel=TRUE, num_workers=4)
{: #extract_frames}

Extract frames from video files in a given data.frame.
Can sample frames based on a specified number of frames or frames per second (fps).

| Parameter | Type | Default | Description |
|---|---|---|---|
| `files`       | data.frame | required      | File manifest containing paths to videos and images |
| `frames`      | integer    | 5             | Number of frames to sample from each video |
| `fps`         | integer    | NULL          | Frames per second to sample from each video. If specified, overrides frames |
| `out_file`    | character  | NULL          | Path to save the extracted frames manifest as a .csv file |
| `out_dir`     | character  | NULL          | Directory to save extracted frame images. If NULL, frames are not saved as images |
| `file_col`    | character  | "filepath"    | Column in manifest to use for file paths |
| `parallel`    | logical    | TRUE          | Toggle to use multiprocessing for frame extraction |
| `num_workers` | integer    | 4             | Number of worker processes to use for parallel processing |

**Returns:** `data.frame` — the input data.frame with an additional "frame" column. The value of frame is 0 for images,
while videos will be represented with multiple rows as indicated by frames or fps, with each row containing the sampled frame number.

<br><br>

---
## Detection
{: #detection}
<br>

### load_detector(model_path, model_type, device=NULL)
{: #load_detector}

Loads a detector model from a file path.

Model types accepted: <br>
"mdv5", "mdv6", "mdv1000-cedar", "mdv1000-larch", "mdv1000-sorrel",
"mdv1000-redwood", "mdv1000-spruce", "yolov5", "yolo", "onnx" <br>
For yolo models v6+, use "yolo", for v5, use "yolov5". For MDv1000 models, specify the version (cedar, larch, sorrel, redwood, spruce).

| Parameter | Type | Default | Description |
|---|---|---|---|
| `model_path`  | character | required | Path to model file |
| `model_type`  | character | required | Type of model |
| `device`      | character | NULL     | Device to run model on, i.e. `"cpu"` or `"cuda"` |

**Returns:** loaded detector object

<br><br>

### detect(detector, image_file_names, resize_width, resize_height, ...)
{: #detect}

Runs a detector model on batches of image files.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `detector`                | object                       | required | Preloaded detector model |
| `image_file_names`        | character / data.frame       | required | Single image path, vector of paths, or manifest data.frame |
| `resize_width`            | integer                      | required | Width to resize images to |
| `resize_height`           | integer                      | required | Height to resize images to |
| `letterbox`               | logical                      | TRUE | Resize and pad to preserve aspect ratio |
| `category_map`            | named list                   | MD_LABELS | Mapping of category IDs to human-readable labels, `list('0'='empty', '1'='animal', '2'='human', '3'='vehicle')` |
| `confidence_threshold`    | numeric                      | 0.1 | Minimum confidence score to retain a detection |
| `file_col`                | character                    | "filepath" | Column name in manifest containing file paths |
| `batch_size`              | integer | 1 | Number of images per batch |
| `num_workers`             | integer | 1 | Number of dataloader workers |
| `device`                  | character | NULL | Device to run inference on: `"cpu"` or `"cuda"` |
| `checkpoint_path`         | character | NULL | Path to save intermediate checkpoint JSON. Checkpoint will be saved after every N batches as specified by checkpoint_frequency. |
| `checkpoint_frequency`    | integer | -1 | Save checkpoint every N batches; -1 disables checkpointing |

**Returns:** `list` with two elements:
- `$detections`: list of detection results in MegaDetector format, one per image
- `$failed_files`: list of files that failed to load during processing (if any)

<br><br>

### parse_detections(results, manifest=NULL, out_file=NULL, threshold=0, file_col="filepath")
{: #parse_detections}

Converts detector output into a detections data.frame.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `results`     | list       | required      | Output of `detect()`, or a checkpoint file loaded with `load_json()`. Failed files are excluded from the results |
| `manifest`    | data.frame | NULL          | Original file manifest; if not NULL, merge detections onto it automatically |
| `out_file`    | character  | NULL          | Path to save detections .csv |
| `threshold`   | numeric    | 0             | Minimum confidence score; detections below are not returned |
| `file_col`    | character  | "filepath"    | Column name containing file paths, will merge results to manifest on this column |

**Returns:** `data.frame` — one row per detection with columns:
             `filepath`, `category`, `category_label`, `conf`, `bbox_x`, `bbox_y`, `bbox_w`, `bbox_h`, `max_detection_conf`

<br><br>

### get_animals(manifest)
{: #get_animals}

Pulls out MD animal detections for classification.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `manifest`    | data.frame | required | Data frame containing one row for every MD detection |

**Returns:** `data.frame` — subset of manifest containing only animal detections

<br><br>

### get_empty(manifest)
{: #get_empty}

Pulls out MD non-animal detections and adds `prediction` and `confidence` columns.
Rows with no detection are labeled "empty" with confidence 1; human and vehicle detections keep the detector confidence.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `manifest`    | data.frame | required | Data frame containing one row for every MD detection |

**Returns:** `data.frame` — subset of manifest containing empty, vehicle and human detections
        with added prediction and confidence columns

<br><br>

---
## Classification
{: #classification}
<br>

### load_classifier(model_path, classes, device=NULL, architecture="efficientnet_v2_m", quiet=TRUE)
{: #load_classifier}

Creates and loads a classifier model of the given architecture from disk, with the associated class list.

| Parameter      | Type  | Default                | Description                                                      |
|----------------|--------|------------------------|------------------------------------------------------------------|
| `model_path`   | character | required            | File or directory path to the model weights                      |
| `classes`      | integer, character or data.frame | required | Number of classes, class list file, or class list data.frame |
| `device`       | character | NULL                | Device to load model on ("cpu" or "cuda")                        |
| `architecture` | character | "efficientnet_v2_m" | Expected architecture name for .pt weights ("efficientnet_v2_m", "convnext_base" or "bioclip_2"). Ignored for .pth and .onnx models |
| `quiet`        | logical   | TRUE                | Toggles suppression of device info messages                      |

**Returns:** `list` with two elements:
- `$model`: loaded classifier model
- `$classes`: class list data.frame, or `NULL` if `classes` is a number. For .onnx models with a `class_dict` in their metadata, the class list is read from the model.

<br><br>

### load_class_list(classlist_file)
{: #load_class_list}

Loads a class list .csv file.

| Parameter         | Type      | Default  | Description                 |
|-------------------|-----------|----------|-----------------------------|
| `classlist_file`  | character | required | File path to class list .csv |

**Returns:** `data.frame` — the class list

<br><br>

### classify(model, detections, resize_width=480, resize_height=480, file_col="filepath", ...)
{: #classify}

Runs prediction for input detections using a preloaded classifier model.

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|--------------------------------------------------------------|
| `model`        | object or list  | required     | Preloaded classifier model, or the list returned by `load_classifier()` |
| `detections`   | data.frame or character | required | Animal detections: a data.frame, vector of file paths, or a file path |
| `resize_width` | integer         | 480          | Image width input size (pixels)                              |
| `resize_height`| integer         | 480          | Image height input size (pixels)                             |
| `file_col`     | character       | "filepath"   | Column name for file paths                                   |
| `crop`         | logical         | TRUE         | Whether to crop images based on bounding boxes               |
| `normalize`    | logical         | TRUE         | Normalize tensors before inference                           |
| `batch_size`   | integer         | 1            | Data generator batch size                                    |
| `num_workers`  | integer         | 1            | Number of worker processes                                   |
| `device`       | character       | NULL         | Device for inference ("cpu" or "cuda")                       |
| `out_file`     | character       | NULL         | Output file path to save prediction results                  |

**Returns:** `list` with two elements:
- `$predictions`: matrix of softmaxed logits for each class, one row per detection that loaded
- `$failed_rows`: row numbers (in `detections`) of detections that failed to load (if any)

If `out_file` already exists, the user is prompted to load the saved predictions instead of re-running the model.

<br><br>

### single_classification(animals, empty, predictions, class_list, best=FALSE, count=FALSE, file_col="filepath")
{: #single_classification}

Assigns the maximum likelihood class label and confidence to each row in a detection data.frame, handling rows that failed to load and "empty" detections.

| Parameter          | Type                  | Default     | Description                                                         |
|--------------------|-----------------------|-------------|---------------------------------------------------------------------|
| `animals`          | data.frame            | required    | Detections with animals (from manifest)                             |
| `empty`            | data.frame or NULL    | required    | Detections with no animals (from manifest); pass `NULL` if there are none |
| `predictions`      | list or matrix        | required    | Output of `classify()`, or just its `$predictions` matrix           |
| `class_list`       | character vector      | required    | Class labels, e.g. `classifier$classes$class`                       |
| `best`             | logical               | FALSE       | If TRUE, returns best prediction for each file only                 |
| `count`            | logical               | FALSE       | If TRUE, adds a count column with the number of detections of each species per file |
| `file_col`         | character             | "filepath"  | Column for file paths                                               |

**Returns:** `data.frame` — animals and empty detections with `prediction` and `confidence` columns

<br><br>

### sequence_classification(animals, empty, predictions, classes, station_col="station", ...)
{: #sequence_classification}

Applies class labels to images based on sequential information.

This function applies image classifications at a sequence level by leveraging information from
multiple images. A sequence is defined as all images at the same camera and station where the
time between consecutive images is <=maxdiff. This can improve classification accuracy, but
assumes that only one species is present in each sequence. If you regularly expect multiple
species to occur in an image or sequence don't use this function.

This function retains "empty" classification even if other images within the sequence are predicted to contain animals.
Classification confidence is weighted by MD confidence.

| Parameter        | Type                    | Default    | Description                                                                |
|------------------|-------------------------|------------|----------------------------------------------------------------------------|
| `animals`        | data.frame              | required   | Detections with animals                                                    |
| `empty`          | data.frame or NULL      | required   | Detections with no animals (empty, human and vehicle), merged back with animals |
| `predictions`    | list or matrix          | required   | Output of `classify()`, or just its `$predictions` matrix                  |
| `classes`        | character vector        | required   | Class labels associated with classifier model, e.g. `classifier$classes$class` |
| `station_col`    | character               | "station"  | Column indicating station/camera                                            |
| `empty_class`    | character               | ""         | Class in `classes` that should be considered "empty", if any                |
| `human_class`    | character               | ""         | Class in `classes` that should be considered "human", if any                |
| `vehicle_class`  | character               | ""         | Class in `classes` that should be considered "vehicle", if any              |
| `sort_columns`   | character vector        | NULL       | Columns to sort by; if not specified defaults to `station_col` and `timestamp_col` |
| `file_col`       | character               | "filepath" | Column indicating image file paths                                         |
| `timestamp_col`  | character               | "datetime" | Column with file timestamps                                                |
| `maxdiff`        | numeric                 | 60         | Maximum time (sec) separating images in the same sequence                  |

**Returns:** `data.frame` — animals and empty detections with `prediction` and `confidence` columns

<br><br>

---
## Re-Identification
{: #re-id}
<br>

### load_miew(file_path, device=NULL)
{: #load_miew}

Loads a MiewID model from a file path.

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|--------------------------------------------------------------|
| `file_path`    | character       | required     | File path to model weights                                   |
| `device`       | character       | NULL         | Device for inference ("cpu" or "cuda")                       |

**Returns:** MiewID model object

<br><br>

### extract_miew_embeddings(miew_model, manifest, file_col="filepath", batch_size=1, num_workers=1, device=NULL)
{: #extract_miew_embeddings}

Extracts MiewID embeddings for a given set of images.

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|--------------------------------------------------------------|
| `miew_model`   | object          | required     | MiewID model object                                          |
| `manifest`     | data.frame      | required     | Data frame of images or crops                                |
| `file_col`     | character       | "filepath"   | Column indicating image file paths                           |
| `batch_size`   | integer         | 1            | Data generator batch size                                    |
| `num_workers`  | integer         | 1            | Number of worker processes                                   |
| `device`       | character       | NULL         | Device for inference ("cpu" or "cuda")                       |

**Returns:** `matrix` — one embedding per manifest row; rows whose image failed to load are filled with NaN

<br><br>

### remove_diagonal(A)
{: #remove_diagonal}

Removes the diagonal elements from a square matrix.

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|--------------------------------------------------------------|
| `A`            | matrix          | required     | Square n x n matrix                                          |

**Returns:** `matrix` — (n-1) x n matrix, each column of `A` with its diagonal element removed

<br><br>

### euclidean_squared_distance(input1, input2)
{: #euclidean_squared_distance}

Computes the squared Euclidean distance between the rows of two feature matrices.

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|--------------------------------------------------------------|
| `input1`       | matrix          | required     | 2-D feature matrix                                           |
| `input2`       | matrix          | required     | 2-D feature matrix                                           |

**Returns:** `matrix` - squared Euclidean distance matrix

<br><br>

### cosine_distance(input1, input2)
{: #cosine_distance}

Computes the cosine distance between the rows of two feature matrices.

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|--------------------------------------------------------------|
| `input1`       | matrix          | required     | 2-D feature matrix                                           |
| `input2`       | matrix          | required     | 2-D feature matrix                                           |

**Returns:** `matrix` - cosine distance matrix

<br><br>

### compute_distance_matrix(input1, input2, metric="euclidean")
{: #compute_distance_matrix}

Computes a distance matrix between two feature matrices using the specified metric.

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|--------------------------------------------------------------|
| `input1`       | matrix          | required     | 2-D feature matrix                                           |
| `input2`       | matrix          | required     | 2-D feature matrix                                           |
| `metric`       | character       | "euclidean"  | Distance metric: "euclidean" (squared) or "cosine"           |

**Returns:** `matrix` - distance matrix

<br><br>

### compute_batched_distance_matrix(input1, input2, metric="cosine", batch_size=10)
{: #compute_batched_distance_matrix}

Computes a distance matrix between two feature matrices in batches, using the specified metric.
This is useful for large datasets that may not fit in memory when computing the full distance matrix at once.

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|--------------------------------------------------------------|
| `input1`       | matrix          | required     | 2-D matrix of query features                                 |
| `input2`       | matrix          | required     | 2-D matrix of database features                              |
| `metric`       | character       | "cosine"     | Distance metric: "euclidean" (squared) or "cosine"           |
| `batch_size`   | integer         | 10           | Number of rows from input1 to process at a time              |

**Returns:** `matrix` - distance matrix

<br><br>

---
## Model Training
{: #training}
<br>

### train_classifier(cfg)
{: #train_classifier}

Trains a classifier model based on the provided configuration.
For details on the configuration parameters, see the [config README](https://github.com/conservationtechlab/animl-py/blob/main/src/animl/config/README.md).

| Parameter | Type | Default | Description |
|---|---|---|---|
| `cfg` | character | required | Path to config .yml file containing training parameters and data paths |

**Returns:** none

<br><br>

### test_classifier(cfg)
{: #test_classifier}

Tests a classifier model based on the provided configuration, evaluating performance on a test dataset and generating a confusion matrix.
For details on the configuration parameters, see the [config README](https://github.com/conservationtechlab/animl-py/blob/main/src/animl/config/README.md).

| Parameter | Type | Default | Description |
|---|---|---|---|
| `cfg` | character | required | Path to config .yml file containing test parameters and data paths |

**Returns:** none

<br><br>

### save_classifier(model, out_dir, epoch, stats, optimizer=NULL, scheduler=NULL)
{: #save_classifier}

Saves model state weights and optional optimizer/scheduler states to disk.

| Parameter   | Type             | Default   | Description                                                  |
|-------------|------------------|-----------|--------------------------------------------------------------|
| `model`     | object           | required  | PyTorch model to save                                        |
| `out_dir`   | character        | required  | Directory path where model weights will be saved             |
| `epoch`     | integer          | required  | Current training epoch (used as filename)                    |
| `stats`     | list             | required  | Performance metrics of the current epoch, e.g. `list(acc = 0.85)` |
| `optimizer` | object           | NULL      | (Optional) PyTorch optimizer state to save                   |
| `scheduler` | object           | NULL      | (Optional) PyTorch scheduler state to save                   |

**Returns:** none

<br><br>

---
## Visualization
{: #visualization}
<br>

### get_frame_as_image(video_path, frame=0)
{: #get_frame_as_image}

Given a video path, return a specific frame as an RGB image.

| Parameter      | Type      | Default   | Description                 |
|----------------|-----------|-----------|-----------------------------|
| `video_path`   | character | required  | File path to video                    |
| `frame`        | integer   | 0         | Frame number to extract |

**Returns:** `array` - height x width x 3 RGB image

<br><br>

### plot_box(rows, file_col="filepath", min_conf=0, classifier_label_col=NULL, detector_category_col="category", show_confidence=FALSE, ...)
{: #plot_box}

Plot bounding box(es) for a single image based on the input rows of a data.frame.

`plot_box()` is designed for plotting boxes on a single image, while `plot_all_bounding_boxes()` can handle multiple images and saves the outputs.

`rows` must contain the bounding box coordinates (`bbox_x`, `bbox_y`, `bbox_w`, `bbox_h`), and file path for the image to be plotted.

If `classifier_label_col` is specified, it will also display the predicted class label on the box. The color of the box(es) is determined by the detector category column specified by `detector_category_col` and the `colors` list.

If `show_confidence` is TRUE, `rows` must contain a `confidence` or `conf` column, and the confidence score will also be displayed on the box.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `rows`                  | data.frame | required | Manifest rows to plot bounding boxes for |
| `file_col`              | character | "filepath" | Column name containing file paths |
| `min_conf`              | numeric | 0 | Minimum confidence threshold to display a bounding box |
| `classifier_label_col`  | character | NULL | Column name containing classifier labels to display on boxes, if applicable |
| `detector_category_col` | character | "category" | Column name containing detector category to determine box color |
| `show_confidence`       | logical | FALSE | If TRUE, show confidence score on box |
| `colors`                | named list | NULL | Named list mapping detector categories to BGR colors, e.g. `list("1" = c(0, 255, 0))`; MegaDetector defaults if NULL |
| `detector_labels`       | named list | NULL | Named list mapping detector categories to human-readable labels; MegaDetector defaults if NULL |
| `return_img`            | logical | FALSE | If TRUE, return the plotted image as an array instead of displaying it |

**Returns:** none, or `array` (if `return_img` is TRUE)

<br><br>

### plot_all_bounding_boxes(manifest, out_dir, file_col="filepath", min_conf=0.1, classifier_label_col=NULL, detector_category_col="category", show_confidence=FALSE, ...)
{: #plot_all_bounding_boxes}

Plot bounding boxes for all rows in a manifest data.frame and save the plotted images.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `manifest`              | data.frame | required | Data frame containing detection results and file paths |
| `out_dir`               | character | required | Directory to save plotted images with bounding boxes |
| `file_col`              | character | "filepath" | Column name containing file paths |
| `min_conf`              | numeric | 0.1 | Minimum confidence threshold to display a bounding box |
| `classifier_label_col`  | character | NULL | Column name containing classifier labels to display on boxes, if applicable |
| `detector_category_col` | character | "category" | Column name containing detector category to determine box color |
| `show_confidence`       | logical | FALSE | If TRUE, show confidence score on box |
| `colors`                | named list | NULL | Named list mapping detector categories to BGR colors; MegaDetector defaults if NULL |
| `detector_labels`       | named list | NULL | Named list mapping detector categories to human-readable labels; MegaDetector defaults if NULL |

**Returns:** none

<br><br>

---
## Export
{: #export}
<br>

### export_folders(manifest, out_dir, out_file=NULL, label_col="prediction", file_col="filepath", timestamp_col="datetime", ...)
{: #export_folders}

Sorts images into one folder per label within `out_dir`, for browsing and human review.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `manifest`      | data.frame | required | Data frame containing file paths and labels |
| `out_dir`       | character  | required | Root directory for the label folders |
| `out_file`      | character  | NULL     | Optional file path to save a .csv of the returned manifest |
| `label_col`     | character  | "prediction" | Column containing labels to use for folder names, e.g. `"category_label"` for detection categories or `"prediction"` for species |
| `file_col`      | character  | "filepath" | Column containing file paths |
| `timestamp_col` | character  | "datetime" | Column containing timestamps in format "%Y-%m-%d %H:%M:%S" |
| `station_col`   | character  | "station"  | Column containing station identifiers |
| `unique_name`   | character  | "uniquename" | Column to use for unique file names in exported folders; if not in manifest, they will be created from `station_col`, `timestamp_col` and the original file name |
| `copy`          | logical    | FALSE    | If TRUE, files are copied to the new folders; if FALSE, they are hard linked |

**Returns:** `data.frame` — copy of manifest with additional column `link` containing the exported file paths, and the
`unique_name` column if it was created

<br><br>

### remove_link(manifest, link_col="link")
{: #remove_link}

Deletes the linked/copied images created by `export_folders()` or `export_camtrapR()`.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `manifest`    | data.frame | required | Data frame containing a column with file paths to remove |
| `link_col`    | character  | "link"   | Column containing file paths to remove |

**Returns:** `data.frame` — manifest with column `link_col` removed

<br><br>

### update_labels_from_folders(manifest, export_dir, unique_name="uniquename")
{: #update_labels_from_folders}

Update manifest after human review of the folders created by `export_folders()`.
Each file's label is taken from the name of the folder it is in after review.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `manifest`    | data.frame | required | Data frame containing file paths and labels to update |
| `export_dir`  | character  | required | Root directory containing the reviewed label folders |
| `unique_name` | character  | "uniquename" | Column containing the unique file names, which must match the file names within `export_dir` |

**Returns:** `data.frame` — manifest merged with a new `label` column containing the reviewed label (the folder name) for each file

<br><br>

### export_train_val_test(manifest, label_col="class", file_col="filepath", conf_col="confidence", out_dir=NULL, val_size=0.1, test_size=0.1, seed=42)
{: #export_train_val_test}

Splits a manifest into training, validation and test sets with `label_col` stratified.
`test_size` and `val_size` are fractions of the whole dataset (e.g., 0.2 -> 20%), and must sum to less than 1.

Only one row is kept per file: the highest confidence detection according to `conf_col`, or the first row if `conf_col` is not in the manifest. This keeps different detections from the same image from ending up in different splits, which would lead to data leakage and overly optimistic performance estimates.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `manifest`    | data.frame | required | Data frame containing file paths, labels, and confidence scores |
| `label_col`   | character  | "class"   | Column containing class labels to stratify on |
| `file_col`    | character  | "filepath" | Column containing file paths |
| `conf_col`    | character  | "confidence" | Column containing confidence scores, used to keep the best detection per file |
| `out_dir`     | character  | NULL     | Directory to save `train_data.csv`, `validate_data.csv` and `test_data.csv`; if NULL, CSVs are not saved |
| `val_size`    | numeric    | 0.1      | Fraction of dataset to use for validation set |
| `test_size`   | numeric    | 0.1      | Fraction of dataset to use for test set |
| `seed`        | integer    | 42       | Random seed for reproducibility of splits |

**Returns:** `list` — train, validation and test data.frames, in that order

<br><br>

### export_yolo(train_manifest, val_manifest, test_manifest, class_list, out_dir, label_col="prediction", file_col="filepath", hard_copy=FALSE)
{: #export_yolo}

Export manifests to YOLO format for model training.
Saves a .txt file for each image with bounding box coordinates and class labels.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `train_manifest` | data.frame | required | Training samples with file paths, labels, and bounding box coordinates |
| `val_manifest`   | data.frame | required | Validation samples with file paths, labels, and bounding box coordinates |
| `test_manifest`  | data.frame or NULL | required | Test samples with file paths, labels, and bounding box coordinates; pass `NULL` to skip the test split |
| `class_list`     | data.frame | required | Data frame with `class` and `id` columns mapping class names to YOLO class IDs |
| `out_dir`        | character  | required | Directory to save YOLO formatted images, .txt label files and `dataset.yaml` |
| `label_col`      | character  | "prediction"   | Column containing class labels |
| `file_col`       | character  | "filepath" | Column containing file paths |
| `hard_copy`      | logical    | FALSE     | If TRUE, image files are copied to the YOLO output directory; if FALSE, they are symlinked |

**Returns:** `list` — contents of the `dataset.yaml` saved to `out_dir`: `path`, `train`, `val` and `test` directories, number of classes `nc`, and class `names`

<br><br>

### export_coco(manifest, class_dict, out_file, info=NULL, licenses=NULL)
{: #export_coco}

Export a manifest to COCO format.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `manifest`    | data.frame  | required | Data frame containing detection results and file paths |
| `class_dict`  | named list  | required | Mapping of class labels to category IDs |
| `out_file`    | character   | required | File path to save the COCO JSON output |
| `info`        | named list  | NULL     | Optional dataset info to include in COCO output, e.g. `list(description = "My Dataset", version = "1.0")` |
| `licenses`    | list        | NULL     | Optional list of licenses to include in COCO output |

**Returns:** `list` — COCO formatted data containing `info`, `licenses`, `categories`, `images`, and `annotations`, which is also saved to `out_file` as JSON

<br><br>

### export_camptrapdp(manifest, out_dir, file_public=FALSE, classifier_name=NULL)
{: #export_camtrapdp}

Export a manifest to CamtrapDP format. Saves `media.csv`, `observations.csv` and `datapackage.json` to `out_dir`.
Requires scientific name for the species prediction label and bounding box coordinates for each detection.
Assumes MegaDetector category labels and uses the `category` column to determine which rows are "empty" vs "animal" detections.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `manifest`        | data.frame | required | Data frame containing classification results and file paths |
| `out_dir`         | character  | required | Directory to save the CamtrapDP formatted output |
| `file_public`     | logical    | FALSE    | Record whether the media files are publicly available |
| `classifier_name` | character  | NULL     | Optional name of the classifier model used for predictions, to include in the output metadata |

**Returns:** `list` — media data.frame, observations data.frame, and datapackage list

<br><br>

### export_camtrapR(manifest, out_dir, out_file=NULL, label_col="prediction", file_col="filepath", ...)
{: #export_camtrapR}

Export into species-labeled folders organized by station, for use with camtrapR.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `manifest`      | data.frame | required      | Data frame containing classification results and file paths |
| `out_dir`       | character  | required      | Directory to export sorted images |
| `out_file`      | character  | NULL          | Optional file path to save a .csv manifest of the exported data |
| `label_col`     | character  | "prediction"  | Column containing species labels to use for folder names |
| `file_col`      | character  | "filepath"    | Column containing file paths |
| `timestamp_col` | character  | "datetime"    | Column containing timestamps |
| `station_col`   | character  | "station"     | Column containing station identifiers |
| `unique_name`   | character  | "uniquename"  | Column to use for unique file names in exported folders; if not in manifest, they will be created from `station_col` and `timestamp_col` |
| `copy`          | logical    | FALSE         | If TRUE, files are copied to the new folders; if FALSE, they are hard linked |

**Returns:** `data.frame` — copy of manifest with additional column `link` for exported file paths

<br><br>

### export_timelapse(manifest, out_dir, only_animal=TRUE)
{: #export_timelapse}

Converts a manifest to .csv files that contain the columns needed for TimeLapse conversion.
Saves `animals.csv` (and `non-animals.csv` if `only_animal` is FALSE) plus the full `manifest.csv` to `out_dir`.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `manifest`    | data.frame | required      | Data frame containing classification results and file paths |
| `out_dir`     | character  | required      | Directory to save the TimeLapse formatted output |
| `only_animal` | logical    | TRUE          | Whether to export only animal detections (TRUE) or also non-animal detections (FALSE) |

**Returns:** path to the saved `manifest.csv`, for use with `export_megadetector()`

<br><br>

### export_megadetector(manifest, out_file=NULL, detector="MegaDetector v5a", prompt=TRUE)
{: #export_megadetector}

Converts a manifest of detections back into MegaDetector format and saves it as a .json file.

If `out_file` is NULL, the output is saved to `detections.json` in the current directory.

Author: Dan Morris https://github.com/agentmorris/MegaDetector/tree/main

| Parameter | Type | Default | Description |
|---|---|---|---|
| `manifest` | data.frame | required | Data frame containing images and associated detections |
| `out_file` | character  | NULL | Path to save the MD formatted file |
| `detector` | character  | "MegaDetector v5a" | Name of the detector used |
| `prompt`   | logical    | TRUE | Whether to prompt before overwriting existing file |

**Returns:** none — saves a .json file in MegaDetector format

<br><br>

---
## File Input/Output
{: #file-io}
<br>

### load_data(file)
{: #load_data}

Load a .csv file into a data.frame.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `file` | character | required | Full path of the .csv file to load |

**Returns:** `data.frame`

<br><br>

### save_json(data, out_file, prompt=TRUE)
{: #save_json}

Save a named list as a JSON file.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `data` | named list | required | Data to be saved |
| `out_file` | character | required | Full path to save the JSON file |
| `prompt` | logical | TRUE | Whether to prompt before overwriting existing file |

**Returns:** none

<br><br>

### load_json(file)
{: #load_json}

Load data from a JSON file, e.g. a `detect()` checkpoint for use with `parse_detections()`.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `file` | character | required | Full path of the JSON file to load |

**Returns:** `list`

<br><br>

### save_yaml(data, out_file, prompt=TRUE)
{: #save_yaml}

Save a named list as a YAML file.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `data` | named list | required | Data to be saved |
| `out_file` | character | required | Full path to save the YAML file |
| `prompt` | logical | TRUE | Whether to prompt before overwriting existing file |

**Returns:** none

<br><br>

### load_yaml(file)
{: #load_yaml}

Load data from a YAML file.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `file` | character | required | Full path of the YAML file to load |

**Returns:** `list`

<br><br>

---
## Utilities
{: #utilities}
<br>

### list_models()
{: #list_models}

Prints the names of the MegaDetector models, SDZWA classifier models and classifier class lists available for download.

**Returns:** none

<br><br>

### download_model(model_url, out_dir="models")
{: #download_model}

Download a model to the given directory.

```r
download_model('https://github.com/agentmorris/MegaDetector/releases/download/v5.0/md_v5a.0.0.pt', out_dir='models')
```

| Parameter | Type | Default | Description |
|---|---|---|---|
| `model_url` | character | required | URL of the model to download |
| `out_dir` | character | "models" | Directory to save the model to; created if it does not exist |

**Returns:** none

<br><br>

---
# Troubleshooting
