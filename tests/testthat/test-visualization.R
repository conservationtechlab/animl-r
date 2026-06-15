library(testthat)

# plot_box ---------------------------------------------------------------
# Visualization functions use a 'detector_category_col' parameter (default 'category')
# to map numeric detection categories (0=empty, 1=animal, 2=human, 3=vehicle)
# to box colors and labels.

test_that("plot_box has detector_category_col parameter defaulting to 'category'", {
  params <- formals(plot_box)
  expect_true("detector_category_col" %in% names(params))
  expect_equal(as.character(params$detector_category_col), "category")
})

test_that("plot_box has classifier_label_col parameter", {
  params <- formals(plot_box)
  expect_true("classifier_label_col" %in% names(params))
})

test_that("plot_box requires animl_py and a real image", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("plot_box requires a real image file — test manually")
})

# plot_all_bounding_boxes ------------------------------------------------

test_that("plot_all_bounding_boxes has detector_category_col parameter defaulting to 'category'", {
  params <- formals(plot_all_bounding_boxes)
  expect_true("detector_category_col" %in% names(params))
  expect_equal(as.character(params$detector_category_col), "category")
})

test_that("plot_all_bounding_boxes has classifier_label_col parameter", {
  params <- formals(plot_all_bounding_boxes)
  expect_true("classifier_label_col" %in% names(params))
})

test_that("plot_all_bounding_boxes requires animl_py and real images", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("plot_all_bounding_boxes requires real image files — test manually")
})
