library(testthat)

# sequence_classification ------------------------------------------------

test_that("sequence_classification errors when animals is not a data frame", {
  expect_error(
    sequence_classification(list(a = 1), NULL, matrix(1), c("cat")),
    "'animals' must be a Data Frame"
  )
})

test_that("sequence_classification errors when predictions_raw is not a matrix", {
  df <- data.frame(filepath = "a.jpg", station = "A", datetime = "2023-01-01 10:00:00", conf = 0.9)
  expect_error(
    sequence_classification(df, NULL, data.frame(x = 1), c("cat")),
    "'predictions_raw' must be a matrix"
  )
})

test_that("sequence_classification errors when nrow(animals) != nrow(predictions_raw)", {
  df <- data.frame(filepath = "a.jpg", station = "A", datetime = "2023-01-01 10:00:00", conf = 0.9)
  mat <- matrix(c(0.9, 0.1, 0.8, 0.2), nrow = 2, ncol = 2)
  expect_error(
    sequence_classification(df, NULL, mat, c("cat", "dog")),
    "same number of rows"
  )
})

test_that("sequence_classification errors when maxdiff is negative", {
  df <- data.frame(filepath = "a.jpg", station = "A", datetime = "2023-01-01 10:00:00", conf = 0.9)
  mat <- matrix(c(0.9, 0.1), nrow = 1, ncol = 2)
  expect_error(
    sequence_classification(df, NULL, mat, c("cat", "dog"), maxdiff = -1),
    "'maxdiff' must be a number"
  )
})

test_that("sequence_classification errors when classes length != ncol(predictions_raw)", {
  df <- data.frame(filepath = "a.jpg", station = "A", datetime = "2023-01-01 10:00:00", conf = 0.9)
  mat <- matrix(c(0.9, 0.1), nrow = 1, ncol = 2)
  expect_error(
    sequence_classification(df, NULL, mat, c("cat")),
    "'classes' must have the same length"
  )
})

test_that("sequence_classification errors when empty_class has length > 1", {
  df <- data.frame(filepath = "a.jpg", station = "A", datetime = "2023-01-01 10:00:00", conf = 0.9)
  mat <- matrix(c(0.9, 0.1), nrow = 1, ncol = 2)
  expect_error(
    sequence_classification(df, NULL, mat, c("cat", "dog"), empty_class = c("empty", "blank")),
    "'empty_class' must be a vector of length 1"
  )
})

test_that("sequence_classification returns data frame with prediction and confidence columns", {
  df <- data.frame(
    filepath = c("a.jpg", "b.jpg"),
    station  = c("A", "A"),
    datetime = c("2023-01-01 10:00:00", "2023-01-01 10:00:30"),
    conf     = c(0.9, 0.8),
    stringsAsFactors = FALSE
  )
  mat <- matrix(c(0.9, 0.1, 0.2, 0.8), nrow = 2, ncol = 2, byrow = TRUE)
  classes <- c("cat", "dog")

  result <- sequence_classification(
    animals         = df,
    empty           = NULL,
    predictions_raw = mat,
    classes         = classes,
    station_col     = "station",
    maxdiff         = 60
  )

  expect_s3_class(result, "data.frame")
  expect_true("prediction" %in% names(result))
  expect_true("confidence" %in% names(result))
})

