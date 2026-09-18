library(testthat)

# get_empty --------------------------------------------------------------
# Detection categories: 0 = empty, 1 = animal, 2 = human, 3 = vehicle
# get_empty() returns all non-animal frames (category != 1), assigning
# prediction/confidence based on category value.

test_that("get_empty errors when manifest is not a data frame", {
  expect_error(get_empty(list(category = 0)), "'manifest' must be Data Frame")
})

test_that("get_empty returns only rows where category != 1", {
  df <- data.frame(category = c(0, 1, 2, 3), conf = c(1, 0.9, 0.8, 0.7))
  result <- get_empty(df)
  expect_true(all(result$category != 1))
  expect_equal(nrow(result), 3)
})

test_that("get_empty returns 0-row data frame with correct columns when all rows are animals", {
  df <- data.frame(category = c(1, 1), conf = c(0.9, 0.8))
  result <- get_empty(df)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result, names(df))
})

test_that("get_empty treats NA category as empty (category 0)", {
  df <- data.frame(category = c(NA, NA), conf = c(1, 1))
  result <- get_empty(df)
  expect_equal(nrow(result), 2)
  expect_true(all(result$prediction == "empty"))
  expect_true(all(result$confidence == 1))
})

test_that("get_empty sets prediction = 'empty' for category 0 rows", {
  df <- data.frame(category = c(0, 0), conf = c(1, 1))
  result <- get_empty(df)
  expect_true(all(result$prediction == "empty"))
})

test_that("get_empty sets prediction = 'human' for category 2 rows", {
  df <- data.frame(category = c(2), conf = c(0.85))
  result <- get_empty(df)
  expect_equal(result$prediction, "human")
})

test_that("get_empty sets prediction = 'vehicle' for category 3 rows", {
  df <- data.frame(category = c(3), conf = c(0.75))
  result <- get_empty(df)
  expect_equal(result$prediction, "vehicle")
})

test_that("get_empty sets confidence = 1 for empty (category 0) rows", {
  df <- data.frame(category = c(0), conf = c(0.5))
  result <- get_empty(df)
  expect_equal(result$confidence, 1)
})

test_that("get_empty sets confidence equal to conf for human/vehicle rows", {
  df <- data.frame(category = c(2, 3), conf = c(0.8, 0.6))
  result <- get_empty(df)
  expect_equal(result$confidence[result$category == 2], 0.8)
  expect_equal(result$confidence[result$category == 3], 0.6)
})

test_that("get_empty handles mixed categories (0, 2, 3) with correct predictions and confidences", {
  df <- data.frame(category = c(0, 2, 3), conf = c(1, 0.9, 0.7))
  result <- get_empty(df)
  expect_equal(nrow(result), 3)
  expect_equal(result$prediction[result$category == 0], "empty")
  expect_equal(result$prediction[result$category == 2], "human")
  expect_equal(result$prediction[result$category == 3], "vehicle")
  expect_equal(result$confidence[result$category == 0], 1)
  expect_equal(result$confidence[result$category == 2], 0.9)
  expect_equal(result$confidence[result$category == 3], 0.7)
})

test_that("get_empty result contains prediction and confidence columns", {
  df <- data.frame(category = c(0, 2), conf = c(1, 0.8))
  result <- get_empty(df)
  expect_true("prediction" %in% names(result))
  expect_true("confidence" %in% names(result))
})

# get_animals ------------------------------------------------------------
# get_animals() returns only frames where category == 1 (animal detections).

test_that("get_animals errors when manifest is not a data frame", {
  expect_error(get_animals(list(category = 1)), "'manifest' must be Data Frame")
})

test_that("get_animals returns only rows where category == 1", {
  df <- data.frame(category = c(0, 1, 1, 2, 3))
  result <- get_animals(df)
  expect_true(all(result$category == 1))
  expect_equal(nrow(result), 2)
})

test_that("get_animals returns 0-row data frame when no animals", {
  df <- data.frame(category = c(0, 2, 3))
  result <- get_animals(df)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("get_animals preserves all columns from manifest", {
  df <- data.frame(category = c(1, 0), conf = c(0.9, 0.1), filepath = c("a.jpg", "b.jpg"))
  result <- get_animals(df)
  expect_named(result, names(df))
})

# animl_py-dependent tests ------------------------------------------------

test_that("train_val_test requires animl_py", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("train_val_test requires a real dataset — test manually")
})
