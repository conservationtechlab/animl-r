animl_py_available <- function() {
  !is.null(animl:::.animl_internal$animl_py)
}

write_test_ppm <- function(path) {
  writeLines(c("P3", "1 1", "255", "255 255 255"), path, useBytes = TRUE)
  path
}