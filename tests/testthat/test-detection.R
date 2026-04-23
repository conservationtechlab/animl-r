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
  result <- animl::parse_detections(results)
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
  result <- animl::parse_detections(results, threshold = 0.5)
  if (nrow(result) > 0) {
    expect_true(all(result$conf >= 0.5 | result$category == 0))
  }
})
