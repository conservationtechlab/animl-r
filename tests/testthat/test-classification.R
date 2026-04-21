test_that("load_class_list reads a CSV and returns a data frame", {
  tmp <- withr::local_tempdir()
  csv_file <- file.path(tmp, "classes.csv")
  writeLines(c("id,class", "1,deer", "2,fox"), csv_file)
  result <- animl::load_class_list(csv_file)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("class" %in% names(result))
})

# animl_py-dependent tests ------------------------------------------------

test_that("classify requires a loaded model file", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("classify requires a real classifier model file — test manually with a local model")
})

test_that("single_classification returns a data frame with prediction and confidence", {
  skip_if(!animl_py_available(), "animl_py not available")
  animals <- data.frame(
    filepath = c("img1.jpg", "img2.jpg"),
    category = c(1L, 1L),
    conf     = c(0.9, 0.8),
    stringsAsFactors = FALSE
  )
  empty <- data.frame(
    filepath   = "img3.jpg",
    category   = 0L,
    conf       = 0.1,
    prediction = "empty",
    confidence = 1.0,
    stringsAsFactors = FALSE
  )
  predictions_raw <- matrix(c(0.8, 0.2, 0.3, 0.7), nrow = 2, ncol = 2)
  class_list <- c("deer", "fox")
  result <- animl::single_classification(animals, empty, predictions_raw, class_list)
  expect_s3_class(result, "data.frame")
  expect_true("prediction" %in% names(result))
  expect_true("confidence" %in% names(result))
})
