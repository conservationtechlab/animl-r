---
layout: default
title: Home
description: Developing open-source technology and machine learning tools for wildlife conservation and ecological research
---

# Getting Started

<br>

## About AniML
{: #about}


**Version 3.3.0**
[GitHub Repository](https://github.com/conservationtechlab/animl-r)

The AniML package is available in Python and R for AI-assisted camera trap image processing.

The AniML package provides functions for ingesting raw image and video files and outputs predictions for species using region-specific species classifier models. We provide several species models including for the African Savanna, the Peruvian Amazon, the Andes mountains, and the Western US. AniML provides the results in a number of export formats, including TimeLapse and CamTrapDP. The package also includes AI-based re-indentification tools and custom species model training. 


<br>

### Installation
{: #installation}

1. Install the package via CRAN.
  
```r
install.packages("animl")
```

2. Set up animl-r locally.

```r
library(animl)
animl::animl_install()
```

<br>

### Requirements
{: #requirements}

AniML-R uses AniML-py to interface with machine learning frameworks PyTorch, Ultralytics and Onnx.   
Upon setup, AniML-R will install AniML-py to a virtual environment. 

<ins>Required dependencies</ins>:
* Python >= 3.12
* reticulate
* animl-py

__Recommended__:
* [ExifTool](https://exiftool.org/)
* [CUDA/cuDNN](https://developer.nvidia.com/cuda/toolkit) (for GPU)


We recommend using AniML with a GPU. To use with an Nvidia GPU, be sure that to install the CUDA-compatible
version of [PyTorch](https://pytorch.org/get-started/locally/)

<br><br>

---
# Examples and Usage
{: #examples}
<br>

### Species Classification Inference
{: #inference}

The functionality of animl can be parcelated into its individual functions to suit your data and scripting needs.

1. It is recommended that you use the AniML Working Directory for storing intermediate steps.
    ```r
    library(animl)
    workingdir <- WorkingDirectory('/path/to/save/data')
    ```

2. Build the file manifest of your given directory. This will find both images and videos.
    ```python
    files = animl.build_file_manifest('/path/to/images', out_file=workingdir.filemanifest, exif=True)
    ```

3. If there are videos, extract individual frames for processing.
   Select either the number of frames or fps using the argumments.
   The other option can be set to None or removed.

    ```python
    allframes = animl.extract_frames(files, frames=3, out_file=workingdir.imageframes, parallel=True)
    ```

4. Pass all images into MegaDetector. We recommend [MDv5a](https://github.com/agentmorris/MegaDetector/releases/download/v5.0/md_v5a.0.0.pt).
   The function parse_MD will convert the json to a pandas DataFrame and merge detections with the original file manifest, if provided.

    ```python
    detector = animl.load_detector('/path/to/mdmodel.pt', model_type="mdv5", device='cuda:0')

    mdresults = animl.detect(detector,
                             allframes,
                             resize_width=animl.MEGADETECTORv5_SIZE,
                             resize_height=animl.MEGADETECTORv5_SIZE,
                             letterbox=True,
                             file_col="frame",
                             device='cuda:0',
                             checkpoint_path=working_dir.mdraw,
                             quiet=True)

    detections = animl.parse_detections(mdresults, manifest=allframes, out_file=workingdir.detections)
    ```

5. For speed and efficiency, extract the empty/human/vehicle detections before classification.
    ```python
    animals = animl.get_animals(detections)
    empty = animl.get_empty(detections)
    ```

6. Classify using the appropriate species model. Merge the output with the rest of the detections if desired.
    ```python
    classifier, class_list = animl.load_classifier('/path/to/model', '/path/to/classlist.txt', device='cuda:0')

    raw_predictions = animl.classify(classifier,
                                     animals,
                                     resize_width=480,
                                     resize_height=480, 
                                     file_col="filepath",
                                     batch_size=4,
                                     out_file=working_dir.predictions)
    ```

7. Apply labels from class list with or without utilizing timestamp-based sequences.
    ```python
    manifest = animl.single_classification(animals, empty, raw_predictions, class_list['class'])
    ```

    or, after defining a station column,

    ```python
    manifest = animl.sequence_classification(animals,
                                             empty, 
                                             raw_predictions,
                                             class_list['class'],
                                             station_col='station',
                                             empty_class="",
                                             sort_columns=None,
                                             file_col="filepath",
                                             maxdiff=60)
    ```

8. (OPTIONAL) Save the Pandas DataFrame's required columns to csv and then use it to create json for TimeLapse compatibility
    ```python
    csv_loc = animl.export_timelapse(manifest, imagedir, only_animal = True)
    animl.export_megadetector(manifest, out_file ="final_result.json", detector = 'MegaDetector v5a')
    ```

9. (OPTIONAL) Create symlinks within a given directory for file browser access.
    ```python
    manifest = animl.export_folders(manifest, out_dir=working_dir.linkdir, out_file=working_dir.results)
    ```

---
### Training

Training workflows are still under development. Please submit Issues as you come upon them.

1. Assuming a file manifest of training data with species labels, first split the data into training, validation and test splits.
   This function splits each label proportionally by the given percentages, by default 0.7 training, 0.2 validation, 0.1 Test.

    ```python
    train, val, test, stats = animl.train_val_test(manifest,
                                                   out_dir='path/to/save/data/', 
                                                   label_col="species",
                                                   val_size: float = 0.2,
                                                   test_size: float = 0.1,
                                                   random_state: int = 42)
    ```

2. Set up training configuration file. Specify the paths to the data splits from the previous step. See [config README](https://github.com/conservationtechlab/animl-py/blob/main/src/animl/config/README.md).

3. (Optional) Update train.py to include MLOPS connection. 

4. Using the config file, begin training
    ```bash
    python -m animl.train --config /path/to/config.yaml
    ```

    Every 10 epochs (or define custom 'checkpoint_frequency'), the model will be checkpointed to the 'experiment_folder' parameter in the config file, and will contain performance metrics for selection.

5. Testing of a model checkpoint can be done with the "test.py" module.  Add an 'active_model' parameter to the config 
file that contains the path of the checkpoint to test. This will produce a confusion matrix of the test dataset as well 
as a csv containing predicted and ground truth labels for each image.

    ```bash
    python -m animl.test --config /path/to/config.yaml
    ```

<br><br>

---
# Reference

## Full Pipeline
{: #full-pipeline}
<br><br>

### animl.from_paths(image_dir, detector_file, classifier_file, classlist_file, ...)
Runs the full detection + classification pipeline on a directory of images or videos.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `image_dir` | str | required | Path to image/video directory |
| `detector_file` | str | required | Path to MegaDetector model |
| `batch_size` | int | 4 | Batch size for inference |
| `sort` | bool | False | Create symlinks sorted by species |
| `visualize` | bool | False | Save bounding box visualizations |
| `sequence` | bool | False | Use sequence-level classification |
| `detect_only` | bool | False | Skip classification step |

<br><br>

### animl.from_config(config)
Runs the full detection + classification pipeline on a directory of images or videos.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `config` | str | required | Path to config yml file.

The config yml must contain the following fields:

<br><br>
  
---
## Data Ingestion and Processing
{: #data-ingestion}
<br>

### class animl.WorkingDirectory()
A WorkingDirectory object includes .

| Parameter | Type | Default | Description |
|---|---|---|---|
| `image_dir` | str | required | Path to image/video directory |
| `detector_file` | str | required | Path to MegaDetector model |
| `batch_size` | int | 4 | Batch size for inference |
| `sort` | bool | False | Create symlinks sorted by species |
| `visualize` | bool | False | Save bounding box visualizations |
| `sequence` | bool | False | Use sequence-level classification |
| `detect_only` | bool | False | Skip classification step |

<br><br>

### animl.build_file_manifest()
Runs the full detection + classification pipeline on a directory of images or videos.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `image_dir` | str | required | Path to image/video directory |
| `detector_file` | str | required | Path to MegaDetector model |
| `batch_size` | int | 4 | Batch size for inference |
| `sort` | bool | False | Create symlinks sorted by species |
| `visualize` | bool | False | Save bounding box visualizations |
| `sequence` | bool | False | Use sequence-level classification |
| `detect_only` | bool | False | Skip classification step |

**Returns:** pandas DataFrame object containing file manifest

Example manifest:

<br><br>
  
### animl.active_times()
Runs the full detection + classification pipeline on a directory of images or videos.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `image_dir` | str | required | Path to image/video directory |
| `detector_file` | str | required | Path to MegaDetector model |

**Returns:**

<br><br>
  
### animl.sequence_calculation()
Runs the full detection + classification pipeline on a directory of images or videos.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `image_dir` | str | required | Path to image/video directory |
| `detector_file` | str | required | Path to MegaDetector model |

**Returns:** 

<br><br>  

### animl.extract_frames()
Runs the full detection + classification pipeline on a directory of images or videos.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `image_dir` | str | required | Path to image/video directory |
| `detector_file` | str | required | Path to MegaDetector model |
| `batch_size` | int | 4 | Batch size for inference |
| `sort` | bool | False | Create symlinks sorted by species |
| `visualize` | bool | False | Save bounding box visualizations |
| `sequence` | bool | False | Use sequence-level classification |
| `detect_only` | bool | False | Skip classification step |
  
<br><br>

---
## Detection
{: #detection}
<br>

### animl.load_detector(model_path, model_type, device=None)
Loads a detector model from a file path.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `model_path` | str | required | Path to model file |
| `model_type` | str | required | Type of model: `"mdv5"`, `"mdv6"`, `"mdv1000-cedar"`, `"mdv1000-larch"`, `"mdv1000-sorrel"`, `"mdv1000-redwood"`, `"mdv1000-spruce"`, `"yolov5"`, `"yolo"`, `"onnx"` |
| `device` | str | None | Device to run model on: `"cpu"` or `"cuda"` |

**Returns:** loaded model object
  
<br><br>

### animl.detect(detector, image_file_names, resize_width, resize_height, ...)
Runs a detector model on batches of image files.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `detector` | object | required | Preloaded detector model |
| `image_file_names` | str / list / DataFrame | required | Single image path, list of paths, or manifest DataFrame |
| `resize_width` | int | required | Width to resize images to |
| `resize_height` | int | required | Height to resize images to |
| `letterbox` | bool | True | Resize and pad to preserve aspect ratio |
| `confidence_threshold` | float | 0.1 | Minimum confidence score to retain a detection |
| `file_col` | str | `"filepath"` | Column name in manifest containing file paths |
| `batch_size` | int | 1 | Number of images per batch |
| `num_workers` | int | 1 | Number of dataloader workers |
| `device` | str | None | Device to run inference on: `"cpu"` or `"cuda"` |
| `checkpoint_path` | str | None | Path to save intermediate checkpoint JSON |
| `checkpoint_frequency` | int | -1 | Save checkpoint every N batches; -1 disables checkpointing |

**Returns:** `list[dict]` — MegaDetector-format results, one dict per image

<br><br>
  
### animl.parse_detections(results, manifest=None, out_file=None, threshold=0.1, file_col="filepath")
Converts detector output into a detections DataFrame.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `results` | list[dict] | required | Raw detector output from `detect()` |
| `manifest` | DataFrame | None | Original file manifest to join metadata onto |
| `out_file` | str | None | Path to save detections CSV |
| `threshold` | float | 0.1 | Minimum confidence score; detections below are set to category 0 |
| `file_col` | str | `"filepath"` | Column name containing file paths |

**Returns:** `pd.DataFrame` — one row per detection with columns `filepath`, `category`, `conf`, `bbox_x`, `bbox_y`, `bbox_w`, `bbox_h`, `max_detection_conf`

<br><br>

---
## Classification
{: #classification}
<br>

### animl.save_classifier(model, out_dir, epoch, stats, optimizer=None, scheduler=None)
Saves model state weights and optional optimizer/scheduler states to disk.

| Parameter   | Type             | Default   | Description                                                  |
|-------------|------------------|-----------|--------------------------------------------------------------|
| `model`     | torch.nn.Module  | required  | The PyTorch model instance to save                           |
| `out_dir`   | str              | required  | Directory path where model weights will be saved             |
| `epoch`     | int              | required  | Current training epoch (used as filename)                    |
| `stats`     | dict             | required  | Training/validation stats/metrics to save with the model     |
| `optimizer` | torch.optim.Optimizer | None      | (Optional) Optimizer state to save                      |
| `scheduler` | torch.optim.lr_scheduler._LRScheduler | None      | (Optional) Scheduler state to save      |

**Returns:** `None`  

<br><br>

### animl.load_classifier(model_path, classes, device=None, architecture="efficientnet_v2_m", quiet=True)
Creates and loads a classifier model of the given architecture from disk, with the associated class list.

| Parameter      | Type                                   | Default                | Description                                                      |
|----------------|----------------------------------------|------------------------|------------------------------------------------------------------|
| `model_path`   | str                                    | required               | File or directory path to the model weights                      |
| `classes`      | int \| str \| Path \| pd.DataFrame      | required               | Number of classes, class list file, or DataFrame                 |
| `device`       | str                                    | None                   | Device to load model on ("cpu" or "cuda")                        |
| `architecture` | str                                    | "efficientnet_v2_m"    | Architecture name ("efficientnet_v2_m" or "convnext_base")       |
| `quiet`        | bool                                   | True                   | If `True`, suppresses device info messages                       |

**Returns:** `(model, class_list)` — loaded model (of given architecture) and class list  

<br><br>

### animl.load_classifier_checkpoint(model_path, model, optimizer, scheduler, device)
Loads the latest checkpoint to resume model training, restoring weights and optimizer/scheduler states.

| Parameter    | Type                      | Default   | Description                                               |
|--------------|---------------------------|-----------|-----------------------------------------------------------|
| `model_path` | str or Path               | required  | Path containing saved model `.pt` checkpoints             |
| `model`      | torch.nn.Module           | required  | Model object to load weights into                         |
| `optimizer`  | torch.optim.Optimizer     | required  | Optimizer object to load state into                       |
| `scheduler`  | torch.optim.lr_scheduler._LRScheduler | required  | Scheduler to load state into               |
| `device`     | str                       | required  | Device to map tensors onto ("cpu" or "cuda")              |

**Returns:** `int` — starting epoch restored from the latest checkpoint  

<br><br>
  
### animl.load_class_list(classlist_file)
Returns classlist file as DataFrame.

| Parameter         | Type   | Default | Description                 |
|-------------------|--------|---------|-----------------------------|
| `classlist_file`  | str    | required| File path to class list CSV |

**Returns:** `pd.DataFrame` — the class list file data  

<br><br>
  
### animl.classify(model, detections, resize_width=480, resize_height=480, file_col="filepath", crop=True, normalize=True, batch_size=1, num_workers=NUM_THREADS, device=None, out_file=None)
Runs prediction for input detections using a preloaded classifier model, managing batching and output saving.

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|--------------------------------------------------------------|
| `model`        | nn.Module       | required     | Preloaded classifier model                                   |
| `detections`   | DataFrame/list/str | required | Animal detections: can be DataFrame, list of filepaths, or a filepath string |
| `resize_width` | int             | 480          | Image width input size (pixels)                              |
| `resize_height`| int             | 480          | Image height input size (pixels)                             |
| `file_col`     | str             | "filepath"   | Column name for file paths                                   |
| `crop`         | bool            | True         | Whether to crop images based on bounding boxes               |
| `normalize`    | bool            | True         | Normalize tensors before inference                           |
| `batch_size`   | int             | 1            | Data generator batch size                                    |
| `num_workers`  | int             | NUM_THREADS  | Number of workers (CPU threads or processes)                 |
| `device`       | str             | None         | Device for inference ("cpu" or "cuda")                       |
| `out_file`     | str             | None         | Output file path to save prediction results                  |

**Returns:** `tuple` — (`predictions`, `failed_files`)  
- `predictions`: `np.array` of softmaxed logits for each class/image  
- `failed_files`: list of files that failed during processing (if any)  
  
<br><br>

### animl.single_classification(animals, empty, predictions_output, class_list, best=False, file_col="filepath", failed_files=None)
Assigns predicted class labels and confidences to each row in a detection DataFrame, handling failed files and "empty" detections.

| Parameter          | Type                          | Default     | Description                                                         |
|--------------------|-------------------------------|-------------|---------------------------------------------------------------------|
| `animals`          | pd.DataFrame                  | required    | Detections with animals (from manifest)                             |
| `empty`            | pd.DataFrame or None          | None        | Detections with no animals (from manifest)                          |
| `predictions_output`| np.array or tuple            | required    | Softmaxed logits or (logits, failed_files) from `classify()`        |
| `class_list`       | list or pd.Series             | required    | List/series of class labels                                         |
| `best`             | bool                          | False       | If True, returns best prediction for each file only                 |
| `file_col`         | str                           | "filepath"  | Column for file paths                                               |
| `failed_files`     | list or None                  | None        | List of files that failed during classification                     |

**Returns:** `pd.DataFrame` — DataFrame with columns `prediction`, `confidence`, and associated metadata  

<br><br>
  
### animl.sequence_classification(animals, empty, predictions_output, class_list, station_col, empty_class="", sort_columns=None, file_col="filepath", timestamp_col="datetime", failed_files=None, maxdiff=60)
Assigns class labels to detections at a sequence level (camera trap burst) using both spatial and temporal context, improving classification accuracy for image bursts.

| Parameter        | Type                    | Default    | Description                                                                |
|------------------|-------------------------|------------|----------------------------------------------------------------------------|
| `animals`        | pd.DataFrame            | required   | Detections with animals                                                    |
| `empty`          | pd.DataFrame or None    | None       | Detections with no animals                                                 |
| `predictions_output`| np.array or tuple    | required   | Softmaxed logits, or (logits, failed_files), from `classify()`             |
| `class_list`     | pd.DataFrame            | required   | Class labels associated with classifier model                               |
| `station_col`    | str                     | required   | Column indicating station/camera                                            |
| `empty_class`    | str                     | ""         | Value of "empty" label in class list                                       |
| `sort_columns`   | list[str] or None       | None       | Columns to sort groups by                                                  |
| `file_col`       | str                     | "filepath" | Column indicating image file paths                                         |
| `timestamp_col`  | str                     | "datetime" | Column with detection timestamps                                           |
| `failed_files`   | list or None            | None       | List of files that failed to classify                                      |
| `maxdiff`        | int                     | 60         | Maximum time (sec) separating images in the same burst/sequence            |

**Returns:** `pd.DataFrame` — sequence-classified results with columns including `prediction`, `confidence`, `sequence`

<br><br>
  
---
## Re-Identification
{: #re-id}
<br>

### animl.load_miew(file_path, device)

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|--------------------------------------------------------------|
| `file_path`    | str             | required     | file path to model file                                      |
| `device`       | str             | None         | Device for inference ("cpu" or "cuda")                       |

**Returns:** MiewID model object 

<br><br>

### animl.extract_miew_embeddings(miew_model, manifest, file_col="filepath", batch_size=1, num_workers=1, device=None)

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|--------------------------------------------------------------|
|`miew_model`    | model object    | required     | MiewID model object                                          |
| `animals`      | pd.DataFrame    | required     | Detections with animals                                      |
| `file_col`     | str             | "filepath"   | Column indicating image file paths                           |
| `batch_size`   | int             | 1            | Data generator batch size                                    |
| `num_workers`  | int             | 1            | Number of workers (CPU threads or processes)                 |
| `device`       | str             | None         | Device for inference ("cpu" or "cuda")                       |

**Returns:** `np.ndarray` — array of extracted embeddings

<br><br>

### animl.remove_diagonal(A)

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|--------------------------------------------------------------|
| `A`            | torch.Tensor    | required     | Input square matrix                                          |

**Returns:** torch.Tensor - Matrix with diagonal elements removed

<br><br>

### animl.euclidean_squared_distance(input1, input2)

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|--------------------------------------------------------------|
| `input1`       | torch.Tensor    | required     | 2-D feature matrix                                           |
| `input2`       | torch.Tensor    | required     | 2-D feature matrix                                           |

**Returns:** torch.Tensor - Euclidean squared distance matrix

<br><br>

### animl.cosine_distance(input1, input2)

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|--------------------------------------------------------------|
| `input1`       | torch.Tensor    | required     | 2-D feature matrix                                           |
| `input2`       | torch.Tensor    | required     | 2-D feature matrix                                           |

**Returns:** torch.Tensor - Cosine distance matrix

<br><br>

### animl.compute_distance_matrix(input1, input2, metric='euclidean')

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|------------------------------------------------------|
| `input1`       | torch.Tensor or np.ndarray | required | 2-D feature matrix                                           |
| `input2`       | torch.Tensor or np.ndarray | required | 2-D feature matrix                                           |
| `metric`       | str             | 'euclidean'  | Distance metric: "euclidean" or "cosine"                   |

**Returns:** np.ndarray - Distance matrix

<br><br>

### animl.compute_batched_distance_matrix(input1, input2, metric='cosine', batch_size=10)

| Parameter      | Type            | Default      | Description                                                  |
|----------------|-----------------|--------------|--------------------------------------------------------------|
| `input1`       | np.ndarray or torch.Tensor | required | 2-D array of query features                                  |
| `input2`       | np.ndarray or torch.Tensor | required | 2-D array of database features                               |
| `metric`       | str             | 'cosine'     | Distance metric (e.g., 'euclidean', 'cosine')               |
| `batch_size`   | int             | 10           | Number of rows from input1 to process at a time              |

**Returns:** np.ndarray - Computed distance matrix

<br><br>

---
## Model Training
{: #training}
<br>

### animl.train_classifier(config)

<br><br>

### animl.test_classifier(config)


<br><br>

---
## Visualization
{: #visualization}
<br>

<br><br>

---
## Export
{: #export}

### class `animl.save_data()`
Runs the full detection + classification pipeline on a directory of images or videos.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `image_dir` | str | required | Path to image/video directory |
| `detector_file` | str | required | Path to MegaDetector model |


### class `animl.load_data()`
Runs the full detection + classification pipeline on a directory of images or videos.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `image_dir` | str | required | Path to image/video directory |
| `detector_file` | str | required | Path to MegaDetector model |

### class `animl.save_json()`
Runs the full detection + classification pipeline on a directory of images or videos.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `image_dir` | str | required | Path to image/video directory |
| `detector_file` | str | required | Path to MegaDetector model |

### class `animl.load_data()`
Runs the full detection + classification pipeline on a directory of images or videos.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `image_dir` | str | required | Path to image/video directory |
| `detector_file` | str | required | Path to MegaDetector model |

### class `animl.check_file()`
Runs the full detection + classification pipeline on a directory of images or videos.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `image_dir` | str | required | Path to image/video directory |
| `detector_file` | str | required | Path to MegaDetector model |


### class `animl.get_frame_as_image()`
Runs the full detection + classification pipeline on a directory of images or videos.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `image_dir` | str | required | Path to image/video directory |
| `detector_file` | str | required | Path to MegaDetector model |

<br><br>

---
# Troubleshooting