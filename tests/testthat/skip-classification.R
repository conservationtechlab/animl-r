library(testthat)

# load_class_list --------------------------------------------------------

test_that("load_class_list reads a CSV and returns a data frame", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  df <- data.frame(id = 1:3, class = c("cat", "dog", "bird"), stringsAsFactors = FALSE)
  write.csv(df, tmp, row.names = FALSE)

  result <- load_class_list(tmp)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("id", "class"))
  expect_equal(nrow(result), 3)
})

test_that("load_class_list column names match the CSV header", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  df <- data.frame(species_id = 1:2, species_name = c("lion", "cheetah"), stringsAsFactors = FALSE)
  write.csv(df, tmp, row.names = FALSE)

  result <- load_class_list(tmp)
  expect_named(result, c("species_id", "species_name"))
})

# animl_py-dependent tests ------------------------------------------------

test_that("save_classifier requires animl_py", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("save_classifier requires a loaded model — test manually")
})

test_that("load_classifier requires animl_py", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("load_classifier requires a real classifier model file — test manually with a local model")
})

test_that("classify requires a loaded model file", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("classify requires a real classifier model file — test manually with a local model")
})

test_that("single_classification returns a data frame with prediction and confidence", {
  skip_if(!animl_py_available(), "animl_py not available")
  animals <- data.frame(
    filepath = c("img1.jpg", "img2.jpg"),
    category = c(1L, 1L),
    category_label = c('animal', 'animal'),
    conf     = c(0.9, 0.8),
    stringsAsFactors = FALSE
  )
  empty <- data.frame(
    filepath   = "img3.jpg",
    category   = 0L,
    category_label = c('empty'),
    conf       = 0.1,
    prediction = "empty",
    confidence = 1.0,
    stringsAsFactors = FALSE
  )
  predictions_raw <- matrix(c(0.8, 0.2, 0.3, 0.7), nrow = 2, ncol = 2)
  class_list <- c("deer", "fox")
  result <- single_classification(animals, empty, predictions_raw, class_list)
  expect_s3_class(result, "data.frame")
  expect_true("prediction" %in% names(result))
  expect_true("confidence" %in% names(result))
})
