library(testthat)

write_test_ppm <- function(path) {
  writeLines(c("P3", "1 1", "255", "255 255 255"), path, useBytes = TRUE)
  path
}

test_that("save_classifier requires animl_py", {
  skip_if(!animl_py_available(), "animl_py not available")
  torch <- tryCatch(reticulate::import("torch", convert = FALSE), error = function(e) NULL)
  skip_if(is.null(torch), "torch not available in test Python environment")

  model <- torch$nn$Linear(4L, 2L)
  out_dir <- tempfile("classifier-save-")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  expect_no_error(save_classifier(model, out_dir, epoch = 1L, stats = list(loss = 0.1)))

  saved_files <- list.files(out_dir, full.names = TRUE)
  expect_gt(length(saved_files), 0)
  expect_true(any(grepl("checkpoint|\\.pt$|\\.pth$|\\.tar$", basename(saved_files), ignore.case = TRUE)))
})

test_that("load_classifier requires animl_py", {
  skip_if(!animl_py_available(), "animl_py not available")
  model_path <- get_animl_test_asset("model")
  skip_if(is.null(model_path), "classifier test model asset unavailable")

  classifier <- load_classifier(
    model_path = model_path,
    classes = 27L,
    architecture = "efficientnet_v2_m",
    quiet = TRUE
  )

  expect_type(classifier, "list")
  expect_named(classifier, c("model", "classes"))
  expect_false(is.null(classifier$model))
  expect_equal(length(classifier), 2L)
})

test_that("classify requires a loaded model file", {
  skip_if(!animl_py_available(), "animl_py not available")
  model_path <- get_animl_test_asset("model")
  skip_if(is.null(model_path), "classifier test model asset unavailable")

  classifier <- load_classifier(
    model_path = model_path,
    classes = 27L,
    architecture = "efficientnet_v2_m",
    quiet = TRUE
  )

  temp_img_dir <- tempfile("classifier-images-")
  dir.create(temp_img_dir, recursive = TRUE)
  on.exit(unlink(temp_img_dir, recursive = TRUE), add = TRUE)

  img_paths <- c(
    write_test_ppm(file.path(temp_img_dir, "img1.ppm")),
    write_test_ppm(file.path(temp_img_dir, "img2.ppm"))
  )
  detections <- data.frame(filepath = img_paths, stringsAsFactors = FALSE)

  results <- classify(
    model = classifier$model,
    detections = detections,
    file_col = "filepath",
    crop = FALSE,
    normalize = TRUE,
    batch_size = 1L,
    num_workers = 1L
  )

  expect_type(results, "list")
  expect_named(results, c("preditions", "failed_files"))
  expect_true(is.matrix(results$preditions) || is.data.frame(results$preditions))
  expect_true(is.character(results$failed_files) || is.list(results$failed_files))
})
