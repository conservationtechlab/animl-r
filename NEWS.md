# animl 3.3.2

Bug fixes: 
- unpack detection results in parse_detections()

# animl 3.3.1

Bug fixes:
- not loading conda env correctly (returning Null read as error)
- update classifier and detector docs to reflect named object output
- build_file_manifest() now returns posixct for datetime, createdate, filemodifydate
- fix interactive messaging for .onLoad and .onAttach


# animl 3.3.0

## New Functions

- `active_times()`: Get start and stop dates for each camera folder from a manifest or directory.
- `export_yolo()`: Export a manifest to YOLO format for model training, including train/val/test splits.
- `export_camptrapdp()`: Export a manifest to Camera Trap Data Package (camtrapdp) format.
- `export_camtrapR()`: Export data into sorted folders organized by station in camtrapR format.

## API Changes

### Detection
- `load_detector()`: Expanded supported `model_type` values to include `"mdv5"`, `"mdv6"`, `"mdv1000-cedar"`, `"mdv1000-larch"`, `"mdv1000-sorrel"`, `"mdv1000-redwood"`, `"mdv1000-spruce"`, `"yolov5"`, `"yolo"`, and `"onnx"`.
- Detection category mapping (`MD_LABELS`): numeric categories in the `category` column now follow the MegaDetector convention — `0` = empty, `1` = animal, `2` = human, `3` = vehicle. This mapping is used consistently by `get_empty()`, `get_animals()`, and the visualization functions.
- `get_empty()`: Returns all non-animal frames (`category != 1`), including empty (`0`), human (`2`), and vehicle (`3`) detections. NA categories are treated as empty (`0`). Adds `prediction` and `confidence` columns to the result.
- `get_animals()`: Returns only frames where `category == 1` (animal detections).

### Classification
- `load_classifier()`: Changed default `architecture` from `"CTL"` to `"efficientnet_v2_m"`.
- `classify()`: Now automatically unpacks a model if it is passed as a list.
- `single_classification()`: Renamed parameter `predictions_raw` to `predictions_output`; added `file_col` and `failed_files` parameters.

### Visualization
- `plot_box()` and `plot_all_bounding_boxes()`: Replaced `label_col` parameter with `classifier_label_col` and added `detector_category_col` parameter (default `"category"`) for more granular control over bounding box display. The `detector_category_col` column is used to color-code and label boxes by detection category (0=empty, 1=animal, 2=human, 3=vehicle).

### File Management
- `sequence_calculation()`: Renamed parameter `datetime_col` to `timestamp_col`; added `sort_columns` and `file_col` parameters.

### Export
- `export_folders()`: Added `timestamp_col` and `station_col` parameters.
- `remove_link()`: Fixed bug where `pbapply` was used instead of `pbsapply`.
- `export_megadetector()`: Output JSON now includes a `detection_categories` field with the standard category mapping (0=empty, 1=animal, 2=human, 3=vehicle).

## Re-ID

Distance metric functions (`remove_diagonal()`, `euclidean_squared_distance()`, `cosine_distance()`, `compute_distance_matrix()`, `compute_batched_distance_matrix()`) are now implemented natively in R, removing the dependency on the Python back-end for these calculations.

## Other Changes

- `video_processing`: Increased default `num_workers` to 4 for faster video processing.
- `install_animl()`: Added conda environment handling for more flexible Python environment management.
- Added comprehensive `testthat` unit tests for all core functions.
