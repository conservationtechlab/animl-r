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
