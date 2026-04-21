test_that("sequence_classification errors when animals is not a data frame", {
  expect_error(animl::sequence_classification(list(), NULL, matrix(1), "deer"))
})

test_that("sequence_classification errors when predictions_raw is not a matrix", {
  animals <- data.frame(filepath = "img.jpg", conf = 0.9, stringsAsFactors = FALSE)
  expect_error(animl::sequence_classification(animals, NULL, data.frame(1), "deer"))
})

test_that("sequence_classification errors when animals and predictions_raw row counts differ", {
  animals <- data.frame(
    filepath = c("img1.jpg", "img2.jpg"),
    conf     = c(0.9, 0.8),
    stringsAsFactors = FALSE
  )
  preds <- matrix(c(0.8, 0.2), nrow = 1)
  expect_error(animl::sequence_classification(animals, NULL, preds, c("deer", "fox")))
})

test_that("sequence_classification errors when class list length mismatches prediction columns", {
  animals <- data.frame(
    filepath = "img1.jpg",
    conf     = 0.9,
    station  = "A",
    datetime = "2024-01-01 12:00:00",
    stringsAsFactors = FALSE
  )
  preds <- matrix(c(0.8, 0.2), nrow = 1)
  expect_error(animl::sequence_classification(animals, NULL, preds, c("deer")))
})

test_that("sequence_classification errors when sort_columns are missing from animals", {
  animals <- data.frame(
    filepath = "img1.jpg",
    conf     = 0.9,
    station  = "A",
    datetime = "2024-01-01 12:00:00",
    stringsAsFactors = FALSE
  )
  preds <- matrix(c(0.8, 0.2), nrow = 1)
  expect_error(
    animl::sequence_classification(animals, NULL, preds, c("deer", "fox"),
                                   sort_columns = c("nonexistent_col"))
  )
})

test_that("sequence_classification returns a data frame with prediction and confidence", {
  animals <- data.frame(
    filepath = c("img1.jpg", "img2.jpg"),
    conf     = c(0.9, 0.8),
    station  = c("A", "A"),
    datetime = c("2024-01-01 12:00:00", "2024-01-01 12:00:30"),
    stringsAsFactors = FALSE
  )
  animals$datetime <- as.POSIXct(animals$datetime)
  preds <- matrix(c(0.8, 0.2, 0.3, 0.7), nrow = 2, ncol = 2)
  class_list <- c("deer", "fox")

  result <- animl::sequence_classification(animals, NULL, preds, class_list,
                                           station_col = "station")
  expect_s3_class(result, "data.frame")
  expect_true("prediction" %in% names(result))
  expect_true("confidence" %in% names(result))
  expect_equal(nrow(result), 2)
})
