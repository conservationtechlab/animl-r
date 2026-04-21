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

# Skipped stubs for Python-dependent functions ---------------------------

test_that("load_classifier requires animl_py", {
  skip("requires animl_py")
})

test_that("classify requires animl_py", {
  skip("requires animl_py")
})

test_that("single_classification requires animl_py", {
  skip("requires animl_py")
})
