library(testthat)

# MD_LABELS ---------------------------------------------------------------
# Detection categories used by MegaDetector and related functions:
#   0 = empty  (no detection / background)
#   1 = animal
#   2 = human
#   3 = vehicle

test_that("MD_LABELS maps the correct detection categories", {
  labels <- animl:::MD_LABELS
  expect_equal(labels[["0"]], "empty")
  expect_equal(labels[["1"]], "animal")
  expect_equal(labels[["2"]], "human")
  expect_equal(labels[["3"]], "vehicle")
})

test_that("MD_LABELS contains exactly four categories (0–3)", {
  labels <- animl:::MD_LABELS
  expect_equal(length(labels), 4)
  expect_true(all(c("0", "1", "2", "3") %in% names(labels)))
})

# load_detector ----------------------------------------------------------

test_that("load_detector loads a real MegaDetector v5a checkpoint", {
  skip_if(!animl_py_available(), "animl_py not available")
  model_path <- get_mdv5a_test_asset()
  skip_if(is.null(model_path), "MegaDetector v5a asset unavailable")
  
  detector <- load_detector(model_path, model_type = "mdv5", device = "cpu")
  expect_false(is.null(detector))
})


# detect -----------------------------------------------------------------

test_that("detect returns detections and failed_files for real images", {
  skip_if(!animl_py_available(), "animl_py not available")
  model_path <- get_mdv5a_test_asset()
  skip_if(is.null(model_path), "MegaDetector v5a asset unavailable")
  
  detector <- load_detector(model_path, model_type = "mdv5", device = "cpu")
  
  examples_dir <- testthat::test_path("..", "..", "examples", "Southwest")
  img_paths <- list.files(examples_dir, pattern = "\\.(JPG|jpg)$", full.names = TRUE)[1:2]
  skip_if(length(img_paths) < 2 || anyNA(img_paths), "example images unavailable")
  
  # A list (rather than a single path string) is what makes animl_py$detect()
  # take the manifest/list branch and return a (results, failed_files) tuple
  # -- this wrapper's names(results) <- c('detections','failed_files') below
  # assumes exactly that shape.
  result <- detect(detector, as.list(img_paths), resize_width = 1280, resize_height = 1280,
                   device = "cpu")
  
  expect_type(result, "list")
  expect_named(result, c("detections", "failed_files"))
  expect_equal(length(result$detections), 2)
  for (d in result$detections) {
    expect_true("filepath" %in% names(d))
    expect_true("detections" %in% names(d))
  }
})

# parse_detections -------------------------------------------------------

test_that("parse_detections returns a data frame from synthetic MD results", {
  skip_if(!animl_py_available(), "animl_py not available")
  results <- list(
    list(
      filepath = "img1.jpg",
      frame = 0,
      detections = list(
        list(category = "1", category_label = 'animal', conf = 0.95, 
             bbox_x = 0.1, bbox_y = 0.2, bbox_w = 0.3, bbox_h = 0.4)
      ),
      max_detection_conf = 0.95
    ),
    list(
      filepath = "img2.jpg",
      frame = 0,
      detections = list(),
      max_detection_conf = 0.0
    )
  )
  result <- parse_detections(results)
  expect_s3_class(result, "data.frame")
  expect_true("category" %in% names(result))
  expect_true("conf" %in% names(result))
})

test_that("parse_detections filters by threshold", {
  skip_if(!animl_py_available(), "animl_py not available")
  results <- list(
    list(
      filepath = "img1.jpg",
      frame = 0,
      detections = list(
        list(category = "1", category_label = 'animal', conf = 0.05, 
             bbox_x = 0.1, bbox_y = 0.2, bbox_w = 0.3, bbox_h = 0.4)
      ),
      max_detection_conf = 0.05
    )
  )
  result <- parse_detections(results, threshold = 0.5)
  expect_equal(nrow(result), 0)
})


