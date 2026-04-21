test_that("WorkingDirectory creates expected subdirectories and assigns path variables", {
  tmp <- withr::local_tempdir()
  env <- new.env()
  animl::WorkingDirectory(tmp, env)
  expect_true(dir.exists(env$linkdir))
  expect_true(dir.exists(env$visdir))
  expect_true(!is.null(env$filemanifest_file))
  expect_true(!is.null(env$results_file))
})

test_that("WorkingDirectory errors when directory does not exist", {
  expect_error(animl::WorkingDirectory("/nonexistent/path/xyz", new.env()))
})

test_that("save_data writes a CSV and load_data reads it back", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "test.csv")
  df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  animl:::save_data(df, out, prompt = FALSE)
  expect_true(file.exists(out))
  loaded <- animl::load_data(out)
  expect_s3_class(loaded, "data.frame")
  expect_equal(nrow(loaded), 3)
  expect_equal(loaded$a, 1:3)
})

test_that("load_data errors on non-CSV file", {
  tmp <- withr::local_tempdir()
  bad_file <- file.path(tmp, "data.txt")
  writeLines("hello", bad_file)
  expect_error(animl::load_data(bad_file))
})

test_that("sequence_calculation adds a sequence column", {
  df <- data.frame(
    station  = c("A", "A", "A", "B"),
    datetime = c("2024-01-01 12:00:00", "2024-01-01 12:00:30",
                 "2024-01-01 12:05:00", "2024-01-01 12:00:00"),
    stringsAsFactors = FALSE
  )
  result <- animl::sequence_calculation(df, station_col = "station")
  expect_s3_class(result, "data.frame")
  expect_true("sequence" %in% names(result))
  expect_equal(nrow(result), 4)
})

test_that("sequence_calculation groups consecutive images within maxdiff into the same sequence", {
  df <- data.frame(
    station  = c("A", "A", "A"),
    datetime = c("2024-01-01 12:00:00", "2024-01-01 12:00:30", "2024-01-01 12:01:00"),
    stringsAsFactors = FALSE
  )
  result <- animl::sequence_calculation(df, station_col = "station", maxdiff = 120)
  expect_equal(length(unique(result$sequence)), 1)
})

test_that("sequence_calculation splits images beyond maxdiff into different sequences", {
  df <- data.frame(
    station  = c("A", "A"),
    datetime = c("2024-01-01 12:00:00", "2024-01-01 12:10:00"),
    stringsAsFactors = FALSE
  )
  result <- animl::sequence_calculation(df, station_col = "station", maxdiff = 60)
  expect_equal(length(unique(result$sequence)), 2)
})

test_that("sequence_calculation errors on invalid station_col", {
  df <- data.frame(
    station  = "A",
    datetime = "2024-01-01 12:00:00",
    stringsAsFactors = FALSE
  )
  expect_error(animl::sequence_calculation(df, station_col = ""))
  expect_error(animl::sequence_calculation(df, station_col = 123))
})

test_that("sequence_calculation errors when datetime column is missing", {
  df <- data.frame(station = "A", stringsAsFactors = FALSE)
  expect_error(animl::sequence_calculation(df, station_col = "station"))
})

test_that("sequence_calculation errors on negative maxdiff", {
  df <- data.frame(
    station  = "A",
    datetime = "2024-01-01 12:00:00",
    stringsAsFactors = FALSE
  )
  expect_error(animl::sequence_calculation(df, station_col = "station", maxdiff = -1))
})

# animl_py-dependent tests ------------------------------------------------

test_that("build_file_manifest returns a data frame for a temp dir with images", {
  skip_if(!animl_py_available(), "animl_py not available")
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "img1.jpg"))
  file.create(file.path(tmp, "img2.jpg"))
  result <- animl::build_file_manifest(tmp, exif = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 2)
  expect_true("filepath" %in% names(result))
})

test_that("build_file_manifest saves to out_file when provided", {
  skip_if(!animl_py_available(), "animl_py not available")
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "img1.jpg"))
  out <- tempfile(fileext = ".csv")
  animl::build_file_manifest(tmp, exif = FALSE, out_file = out)
  expect_true(file.exists(out))
})

test_that("save_json writes a file and load_json reads it back", {
  skip_if(!animl_py_available(), "animl_py not available")
  data <- list(key = "value", number = 42)
  out <- tempfile(fileext = ".json")
  animl::save_json(data, out, prompt = FALSE)
  expect_true(file.exists(out))
  loaded <- animl::load_json(out)
  expect_equal(loaded$key, "value")
})
