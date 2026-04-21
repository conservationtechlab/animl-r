test_that("get_empty returns non-animal rows with prediction and confidence columns", {
  manifest <- data.frame(
    category = c(0, 1, 1, 2, 3),
    conf     = c(0.1, 0.9, 0.8, 0.7, 0.6),
    stringsAsFactors = FALSE
  )
  result <- animl::get_empty(manifest)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(result$category != 1))
  expect_true("prediction" %in% names(result))
  expect_true("confidence" %in% names(result))
})

test_that("get_empty labels category 0 as empty with confidence 1", {
  manifest <- data.frame(
    category = c(0, 0),
    conf     = c(0.1, 0.2),
    stringsAsFactors = FALSE
  )
  result <- animl::get_empty(manifest)
  expect_true(all(result$prediction == "empty"))
  expect_true(all(result$confidence == 1))
})

test_that("get_empty labels category 2 as human", {
  manifest <- data.frame(
    category = c(2),
    conf     = c(0.7),
    stringsAsFactors = FALSE
  )
  result <- animl::get_empty(manifest)
  expect_equal(result$prediction, "human")
  expect_equal(result$confidence, 0.7)
})

test_that("get_empty labels category 3 as vehicle", {
  manifest <- data.frame(
    category = c(3),
    conf     = c(0.65),
    stringsAsFactors = FALSE
  )
  result <- animl::get_empty(manifest)
  expect_equal(result$prediction, "vehicle")
  expect_equal(result$confidence, 0.65)
})

test_that("get_empty returns empty data frame when all rows are animals", {
  manifest <- data.frame(category = c(1, 1), conf = c(0.9, 0.8), stringsAsFactors = FALSE)
  result <- animl::get_empty(manifest)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("get_empty errors when manifest is not a data frame", {
  expect_error(animl::get_empty(c(0, 1, 2)))
})

test_that("get_animals returns only rows with category == 1", {
  manifest <- data.frame(
    category = c(0, 1, 1, 2),
    conf     = c(0.1, 0.9, 0.8, 0.3),
    stringsAsFactors = FALSE
  )
  result <- animl::get_animals(manifest)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(result$category == 1))
})

test_that("get_animals returns empty data frame when no animals", {
  manifest <- data.frame(category = c(0, 2, 3), conf = c(0.1, 0.5, 0.4), stringsAsFactors = FALSE)
  result <- animl::get_animals(manifest)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("get_animals errors when manifest is not a data frame", {
  expect_error(animl::get_animals(list(category = c(1, 2))))
})
