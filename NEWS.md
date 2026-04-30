# animl 3.2.1

## New Functions

- `active_times()`: Get start and stop dates for each camera folder from a manifest or directory.
- `export_yolo()`: Export a manifest to YOLO format for model training, including train/val/test splits.
- `export_camptrapdp()`: Export a manifest to Camera Trap Data Package (camtrapdp) format.
- `export_camtrapR()`: Export data into sorted folders organized by station in camtrapR format.

## API Changes

### Detection
- `load_detector()`: Expanded supported `model_type` values to include `"mdv5"`, `"mdv6"`, `"mdv1000-cedar"`, `"mdv1000-larch"`, `"mdv1000-sorrel"`, `"mdv1000-redwood"`, `"mdv1000-spruce"`, `"yolov5"`, `"yolo"`, and `"onnx"`.

### Classification
- `load_classifier()`: Changed default `architecture` from `"CTL"` to `"efficientnet_v2_m"`.
- `classify()`: Now automatically unpacks a model if it is passed as a list.
- `single_classification()`: Renamed parameter `predictions_raw` to `predictions_output`; added `file_col` and `failed_files` parameters.

### Visualization
- `plot_box()` and `plot_all_bounding_boxes()`: Replaced `label_col` parameter with `classifier_label_col` and added `detector_category_col` parameter for more granular control over bounding box display.

### File Management
- `sequence_calculation()`: Renamed parameter `datetime_col` to `timestamp_col`; added `sort_columns` and `file_col` parameters.

### Export
- `export_folders()`: Added `timestamp_col` and `station_col` parameters.
- `remove_link()`: Fixed bug where `pbapply` was used instead of `pbsapply`.

## Re-ID

Distance metric functions (`remove_diagonal()`, `euclidean_squared_distance()`, `cosine_distance()`, `compute_distance_matrix()`, `compute_batched_distance_matrix()`) are now implemented natively in R, removing the dependency on the Python back-end for these calculations.

## Other Changes

- `video_processing`: Increased default `num_workers` to 4 for faster video processing.
- `install_animl()`: Added conda environment handling for more flexible Python environment management.
- Added comprehensive `testthat` unit tests for all core functions.
