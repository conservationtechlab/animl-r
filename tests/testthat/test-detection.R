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

test_that("load_detector requires a model file", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("load_detector requires a real model file — test manually with a local model")
})

# detect -----------------------------------------------------------------

test_that("detect requires a loaded detector model", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("detect requires a real detector model — test manually with a local model")
})

# parse_detections -------------------------------------------------------

test_that("parse_detections returns a data frame from synthetic MD results", {
  skip_if(!animl_py_available(), "animl_py not available")
  results <- list(
    list(
      filepath = "img1.jpg",
      frame = 0,
      detections = list(
        list(category = "1", conf = 0.95, 
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
        list(category = "1", conf = 0.05, 
             bbox_x = 0.1, bbox_y = 0.2, bbox_w = 0.3, bbox_h = 0.4)
      ),
      max_detection_conf = 0.05
    )
  )
  result <- parse_detections(results, threshold = 0.5)
  if (nrow(result) > 0) {
    expect_true(all(result$conf >= 0.5 | result$category == 0))
  }
})
