library(testthat)

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
      file = "img1.jpg",
      detections = list(
        list(category = "1", conf = 0.95, bbox = list(0.1, 0.2, 0.3, 0.4))
      ),
      max_detection_conf = 0.95
    ),
    list(
      file = "img2.jpg",
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
      file = "img1.jpg",
      detections = list(
        list(category = "1", conf = 0.05, bbox = list(0.1, 0.2, 0.3, 0.4))
      ),
      max_detection_conf = 0.05
    )
  )
  result <- parse_detections(results, threshold = 0.5)
  if (nrow(result) > 0) {
    expect_true(all(result$conf >= 0.5 | result$category == 0))
  }
})
