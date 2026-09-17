library(testthat)

skip_if_not_installed("reticulate")

if (!reticulate::py_available(initialize = FALSE)) {
  skip("Python not available")
}

test_that("CI Python setup is available for animl", {
  expect_true(reticulate::virtualenv_exists("animl_env"))

  expect_equal(
    reticulate::py_eval(
      "'{}.{}'.format(__import__('sys').version_info[0], __import__('sys').version_info[1])"
    ),
    "3.12"
  )

  expect_true(reticulate::py_module_available("animl"))

  animl_py <- reticulate::import("animl", delay_load = FALSE)

  expect_false(is.null(animl_py))
  expect_false(is.null(animl:::.animl_internal$animl_py))
  expect_equal(animl_py$`__version__`, animl:::.animl_internal$animl_py$`__version__`)
  expect_equal(
    normalizePath(reticulate::py_eval("__import__('sys').executable"), winslash = "/", mustWork = FALSE),
    normalizePath(reticulate::py_config()$python, winslash = "/", mustWork = FALSE)
  )
})
