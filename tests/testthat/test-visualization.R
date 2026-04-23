library(testthat)

# plot_box ---------------------------------------------------------------

test_that("plot_box requires animl_py and a real image", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("plot_box requires a real image file — test manually")
})

# plot_all_bounding_boxes ------------------------------------------------

test_that("plot_all_bounding_boxes requires animl_py and real images", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("plot_all_bounding_boxes requires real image files — test manually")
})
