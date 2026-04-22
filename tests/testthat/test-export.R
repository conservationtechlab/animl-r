library(testthat)

# remove_link ------------------------------------------------------------

test_that("remove_link deletes files and removes link column", {
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  writeLines("a", tmp1)
  writeLines("b", tmp2)
  on.exit({
    if (file.exists(tmp1)) unlink(tmp1)
    if (file.exists(tmp2)) unlink(tmp2)
  })

  df <- data.frame(
    name = c("img1", "img2"),
    link = c(tmp1, tmp2),
    stringsAsFactors = FALSE
  )

  result <- remove_link(df, link_col = "link")

  expect_false(file.exists(tmp1))
  expect_false(file.exists(tmp2))
  expect_false("link" %in% names(result))
  expect_s3_class(result, "data.frame")
})

# update_labels_from_folders ---------------------------------------------

test_that("update_labels_from_folders errors when export_dir does not exist", {
  df <- data.frame(uniquename = "img1.jpg", stringsAsFactors = FALSE)
  expect_error(
    update_labels_from_folders(df, "/nonexistent/dir", unique_name = "uniquename"),
    "does not exist"
  )
})

test_that("update_labels_from_folders errors when unique_name column is missing", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  df <- data.frame(other_col = "img1.jpg", stringsAsFactors = FALSE)
  expect_error(
    update_labels_from_folders(df, tmp, unique_name = "uniquename"),
    "cannot match"
  )
})

test_that("update_labels_from_folders merges label from folder structure", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  species_dir <- file.path(tmp, "lion")
  dir.create(species_dir)
  file.create(file.path(species_dir, "img1.jpg"))

  df <- data.frame(uniquename = "img1.jpg", stringsAsFactors = FALSE)
  result <- update_labels_from_folders(df, tmp, unique_name = "uniquename")

  expect_s3_class(result, "data.frame")
  expect_true("label" %in% names(result))
  expect_equal(result$label[1], "lion")
})

# animl_py-dependent tests ------------------------------------------------

test_that("export_folders creates species subdirectories and returns manifest with link column", {
  skip_if(!animl_py_available(), "animl_py not available")
  tmp_src <- tempfile(); dir.create(tmp_src)
  tmp_dst <- tempfile(); dir.create(tmp_dst)
  on.exit({ unlink(tmp_src, recursive = TRUE); unlink(tmp_dst, recursive = TRUE) })

  f1 <- file.path(tmp_src, "img1.jpg"); file.create(f1)
  f2 <- file.path(tmp_src, "img2.jpg"); file.create(f2)

  manifest <- data.frame(
    filepath   = c(f1, f2),
    prediction = c("deer", "deer"),
    stringsAsFactors = FALSE
  )
  result <- export_folders(manifest, tmp_dst, copy = TRUE)
  expect_s3_class(result, "data.frame")
  expect_true("link" %in% names(result))
  expect_true(dir.exists(file.path(tmp_dst, "deer")))
})

test_that("export_folders errors when label_col is missing from manifest", {
  skip_if(!animl_py_available(), "animl_py not available")
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  manifest <- data.frame(filepath = "img.jpg", stringsAsFactors = FALSE)
  expect_error(export_folders(manifest, tmp))
})

test_that("export_megadetector writes a valid JSON file", {
  skip_if(!animl_py_available(), "animl_py not available")
  manifest <- data.frame(
    filepath   = c("img1.jpg", "img1.jpg"),
    category   = c(1L, 1L),
    conf       = c(0.9, 0.8),
    bbox_x     = c(0.1, 0.2),
    bbox_y     = c(0.1, 0.2),
    bbox_w     = c(0.3, 0.2),
    bbox_h     = c(0.3, 0.2),
    prediction = c("deer", "deer"),
    confidence = c(0.85, 0.75),
    stringsAsFactors = FALSE
  )
  out <- tempfile(fileext = ".json")
  on.exit(unlink(out))
  export_megadetector(manifest, out_file = out, prompt = FALSE)
  expect_true(file.exists(out))
  parsed <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_true("images" %in% names(parsed))
  expect_true("detection_categories" %in% names(parsed))
})

test_that("export_megadetector errors on missing required columns", {
  skip_if(!animl_py_available(), "animl_py not available")
  manifest <- data.frame(filepath = "img.jpg", stringsAsFactors = FALSE)
  expect_error(export_megadetector(manifest, out_file = tempfile(), prompt = FALSE))
})

test_that("export_coco writes a valid COCO JSON file", {
  skip_if(!animl_py_available(), "animl_py not available")
  manifest <- data.frame(
    filepath           = "img1.jpg",
    filename           = "img1.jpg",
    filemodifydate     = "2024-01-01",
    frame              = 0L,
    max_detection_conf = 0.9,
    category           = 1L,
    conf               = 0.9,
    bbox_x             = 0.1,
    bbox_y             = 0.1,
    bbox_w             = 0.3,
    bbox_h             = 0.3,
    prediction         = "deer",
    confidence         = 0.85,
    width              = 1280L,
    height             = 960L,
    stringsAsFactors   = FALSE
  )
  class_list <- data.frame(id = 1L, class = "deer", stringsAsFactors = FALSE)
  out <- tempfile(fileext = ".json")
  on.exit(unlink(out))
  export_coco(manifest, class_list, out)
  expect_true(file.exists(out))
  parsed <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_true(all(c("images", "annotations", "categories", "info", "licenses") %in% names(parsed)))
})

test_that("export_camtrapR requires animl_py", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("export_camtrapR requires a real model setup — test manually")
})

test_that("export_timelapse writes animals.csv to out_dir", {
  skip_if(!animl_py_available(), "animl_py not available")
  out <- tempfile(); dir.create(out)
  on.exit(unlink(out, recursive = TRUE))
  manifest <- data.frame(
    filepath           = c("img1.jpg", "img2.jpg"),
    filename           = c("img1.jpg", "img2.jpg"),
    filemodifydate     = c("2024-01-01", "2024-01-01"),
    frame              = c(0L, 0L),
    max_detection_conf = c(0.9, 0.1),
    category           = c(1L, 0L),
    conf               = c(0.9, 0.1),
    bbox_x             = c(0.1, NA),
    bbox_y             = c(0.1, NA),
    bbox_w             = c(0.3, NA),
    bbox_h             = c(0.3, NA),
    prediction         = c("deer", "empty"),
    confidence         = c(0.85, 1.0),
    stringsAsFactors   = FALSE
  )
  export_timelapse(manifest, out, only_animal = TRUE)
  expect_true(file.exists(file.path(out, "animals.csv")))
})
