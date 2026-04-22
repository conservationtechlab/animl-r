library(testthat)

# WorkingDirectory -------------------------------------------------------

test_that("WorkingDirectory errors when directory does not exist", {
  expect_error(WorkingDirectory("/nonexistent/path", new.env()), "Output directory invalid")
})

test_that("WorkingDirectory creates subdirectories and assigns env keys", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  env <- new.env(parent = emptyenv())
  WorkingDirectory(tmp, env)

  expect_true(dir.exists(env$linkdir))
  expect_true(dir.exists(env$visdir))
  expect_true(grepl("Sorted", env$linkdir))
  expect_true(grepl("Plots", env$visdir))
})

test_that("WorkingDirectory assigns expected file path keys", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  env <- new.env(parent = emptyenv())
  WorkingDirectory(tmp, env)

  expect_true(grepl("FileManifest\\.csv$", env$filemanifest_file))
  expect_true(grepl("Results\\.csv$", env$results_file))
  expect_true(grepl("Predictions\\.csv$", env$predictions_file))
  expect_true(grepl("Detections\\.csv$", env$detections_file))
  expect_true(grepl("ImageFrames\\.csv$", env$imageframes_file))
  expect_true(grepl("MD_Raw\\.json$", env$mdraw_file))
})

# save_data / load_data --------------------------------------------------

test_that("save_data writes a CSV that load_data can read back", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  save_data(df, tmp, prompt = FALSE)

  expect_true(file.exists(tmp))
  result <- read.csv(tmp, stringsAsFactors = FALSE)
  expect_equal(result$a, df$a)
  expect_equal(result$b, df$b)
})

test_that("save_data re-saves without prompting when prompt=FALSE and file already exists", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  df1 <- data.frame(x = 1)
  save_data(df1, tmp, prompt = FALSE)

  df2 <- data.frame(x = 99)
  save_data(df2, tmp, prompt = FALSE)

  result <- read.csv(tmp)
  expect_equal(result$x, 99)
})

test_that("load_data returns a data frame from a CSV", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  df <- data.frame(col1 = c(1, 2), col2 = c("a", "b"), stringsAsFactors = FALSE)
  save_data(df, tmp, prompt = FALSE)

  loaded <- load_data(tmp)
  expect_s3_class(loaded, "data.frame")
  expect_equal(nrow(loaded), 2)
})

test_that("load_data errors when given a non-CSV file", {
  expect_error(load_data("file.txt"), "Expecting a .csv file")
})

# check_file -------------------------------------------------------------

test_that("check_file returns FALSE when file is NULL", {
  expect_false(check_file(NULL, "test"))
})

test_that("check_file returns FALSE when file does not exist", {
  expect_false(check_file("/nonexistent/file.csv", "test"))
})

# sequence_calculation ---------------------------------------------------

test_that("sequence_calculation returns a data frame with a sequence column", {
  df <- data.frame(
    station  = c("A", "A"),
    datetime = c("2023-01-01 10:00:00", "2023-01-01 10:00:30"),
    stringsAsFactors = FALSE
  )
  result <- sequence_calculation(df, station_col = "station", datetime_col = "datetime", maxdiff = 60)
  expect_s3_class(result, "data.frame")
  expect_true("sequence" %in% names(result))
})

test_that("sequence_calculation groups consecutive rows within maxdiff into same sequence", {
  df <- data.frame(
    station  = c("A", "A", "A"),
    datetime = c("2023-01-01 10:00:00", "2023-01-01 10:00:30", "2023-01-01 10:01:00"),
    stringsAsFactors = FALSE
  )
  result <- sequence_calculation(df, station_col = "station", datetime_col = "datetime", maxdiff = 60)
  expect_equal(length(unique(result$sequence)), 1)
})

test_that("sequence_calculation assigns different sequences to rows > maxdiff apart", {
  df <- data.frame(
    station  = c("A", "A"),
    datetime = c("2023-01-01 10:00:00", "2023-01-01 11:00:00"),
    stringsAsFactors = FALSE
  )
  result <- sequence_calculation(df, station_col = "station", datetime_col = "datetime", maxdiff = 60)
  expect_equal(length(unique(result$sequence)), 2)
})

test_that("sequence_calculation assigns different sequences to different stations even if close in time", {
  df <- data.frame(
    station  = c("A", "B"),
    datetime = c("2023-01-01 10:00:00", "2023-01-01 10:00:05"),
    stringsAsFactors = FALSE
  )
  result <- sequence_calculation(df, station_col = "station", datetime_col = "datetime", maxdiff = 60)
  expect_equal(length(unique(result$sequence)), 2)
})

test_that("sequence_calculation errors when station_col is empty string", {
  df <- data.frame(
    station  = "A",
    datetime = "2023-01-01 10:00:00",
    stringsAsFactors = FALSE
  )
  expect_error(sequence_calculation(df, station_col = "", datetime_col = "datetime"))
})

test_that("sequence_calculation errors when maxdiff is negative", {
  df <- data.frame(
    station  = "A",
    datetime = "2023-01-01 10:00:00",
    stringsAsFactors = FALSE
  )
  expect_error(sequence_calculation(df, station_col = "station", datetime_col = "datetime", maxdiff = -1))
})

test_that("sequence_calculation errors when datetime_col is not present in manifest", {
  df <- data.frame(station = "A", stringsAsFactors = FALSE)
  expect_error(sequence_calculation(df, station_col = "station", datetime_col = "datetime"))
})

# animl_py-dependent tests ------------------------------------------------

test_that("build_file_manifest returns a data frame for a temp dir with images", {
  skip_if(!animl_py_available(), "animl_py not available")
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  file.create(file.path(tmp, "img1.jpg"))
  file.create(file.path(tmp, "img2.jpg"))
  result <- build_file_manifest(tmp, exif = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 2)
  expect_true("filepath" %in% names(result))
})

test_that("build_file_manifest saves to out_file when provided", {
  skip_if(!animl_py_available(), "animl_py not available")
  tmp <- tempfile(); dir.create(tmp)
  out <- tempfile(fileext = ".csv")
  on.exit({ unlink(tmp, recursive = TRUE); unlink(out) })
  file.create(file.path(tmp, "img1.jpg"))
  build_file_manifest(tmp, exif = FALSE, out_file = out)
  expect_true(file.exists(out))
})

test_that("save_json writes a file and load_json reads it back", {
  skip_if(!animl_py_available(), "animl_py not available")
  data <- list(key = "value", number = 42)
  out <- tempfile(fileext = ".json")
  on.exit(unlink(out))
  save_json(data, out, prompt = FALSE)
  expect_true(file.exists(out))
  loaded <- load_json(out)
  expect_equal(loaded$key, "value")
})

test_that("download_model requires animl_py", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("download_model requires network access — test manually")
})

test_that("list_models requires animl_py", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("list_models requires network access — test manually")
})
