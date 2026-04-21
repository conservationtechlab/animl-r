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

  # Create a species subfolder with a file named after the uniquename value
  species_dir <- file.path(tmp, "lion")
  dir.create(species_dir)
  file.create(file.path(species_dir, "img1.jpg"))

  df <- data.frame(uniquename = "img1.jpg", stringsAsFactors = FALSE)
  result <- update_labels_from_folders(df, tmp, unique_name = "uniquename")

  expect_s3_class(result, "data.frame")
  expect_true("label" %in% names(result))
  expect_equal(result$label[1], "lion")
})

# Skipped stubs for Python-dependent functions ---------------------------

test_that("export_folders requires animl_py", {
  skip("requires animl_py")
})

test_that("export_coco requires animl_py", {
  skip("requires animl_py")
})

test_that("export_camtrapR requires animl_py", {
  skip("requires animl_py")
})

test_that("export_timelapse requires animl_py", {
  skip("requires animl_py")
})

test_that("export_megadetector requires animl_py", {
  skip("requires animl_py")
})
