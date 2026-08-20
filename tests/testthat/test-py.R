library(testthat)

# Simple tests to check that animl_py is available and can be imported.test_that("check_animl_py returns a list", {

test_that("check_animl_py returns a list", {
  skip_if_not(animl_module_installed(), "animl-py not installed")
  
  result <- check_animl_py()
  expect_type(result, "list")
  expect_length(result, 3)
})

test_that("check_animl_py list has correct structure", {
  skip_if_not(animl_module_installed(), "animl-py not installed")
  
  result <- check_animl_py()
  
  expect_named(result, c("exiftool", "torch_cuda", "onnx_cuda"))
  # exiftool can be logical (FALSE) or character (version string)
  expect_true(is.logical(result$exiftool) || is.character(result$exiftool))
  expect_type(result$torch_cuda, "logical")
  expect_type(result$onnx_cuda, "logical")
})

test_that("check_animl_py exiftool returns version or FALSE", {
  skip_if_not(animl_module_installed(), "animl-py not installed")
  
  result <- check_animl_py()
  
  # Should be either FALSE or a version string
  if (isFALSE(result$exiftool)) {
    expect_false(result$exiftool)
  } else {
    expect_type(result$exiftool, "character")
    # Version strings typically contain numbers
    expect_match(result$exiftool, "[0-9]")
  }
})

test_that("check_animl_py torch_cuda returns logical", {
  skip_if_not(animl_module_installed(), "animl-py not installed")
  
  result <- check_animl_py()
  expect_type(result$torch_cuda, "logical")
})

test_that("check_animl_py onnx_cuda returns logical", {
  skip_if_not(animl_module_installed(), "animl-py not installed")
  
  result <- check_animl_py()
  expect_type(result$onnx_cuda, "logical")
})

test_that("check_animl_py returns FALSE values when animl not installed", {
  skip_if(animl_module_installed(), "animl-py is installed")
  
  result <- check_animl_py()
  expect_false(result$exiftool)
  expect_false(result$torch_cuda)
  expect_false(result$onnx_cuda)
})

test_that("check_animl_py prints system configuration", {
  skip_if_not(animl_module_installed(), "animl-py not installed")
  
  cat("\n=== System Configuration ===\n")
  result <- check_animl_py()
  cat("=============================\n")
  
  cat("\nDetailed Results:\n")
  exif_status <- if(isFALSE(result$exiftool)) "NOT FOUND" else result$exiftool
  cat("  Exiftool:", exif_status, "\n")
  cat("  PyTorch CUDA:", result$torch_cuda, "\n")
  cat("  ONNX CUDA:", result$onnx_cuda, "\n")
  
  expect_true(TRUE)
})