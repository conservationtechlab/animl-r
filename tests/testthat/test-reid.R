library(testthat)

# load_miew --------------------------------------------------------------

test_that("load_miew loads a fetched MiewID model", {
  skip_if(!animl_py_available(), "animl_py not available")
  model_path <- get_miewid_test_asset()
  skip_if(is.null(model_path), "MiewID model asset unavailable (Hugging Face unreachable)")
  
  model <- load_miew(model_path, device = "cpu")
  expect_false(is.null(model))
})


# extract_miew_embeddings ------------------------------------------------

test_that("extract_miew_embeddings returns embeddings for real images", {
  skip_if(!animl_py_available(), "animl_py not available")
  model_path <- get_miewid_test_asset()
  skip_if(is.null(model_path), "MiewID model asset unavailable (Hugging Face unreachable)")
  
  model <- load_miew(model_path, device = "cpu")
  
  temp_img_dir <- tempfile("miewid-images-")
  dir.create(temp_img_dir, recursive = TRUE)
  on.exit(unlink(temp_img_dir, recursive = TRUE), add = TRUE)
  
  img_paths <- c(
    write_test_ppm(file.path(temp_img_dir, "img1.jpg")),
    write_test_ppm(file.path(temp_img_dir, "img2.jpg"))
  )
  manifest <- data.frame(
    filepath = img_paths,
    bbox_x = 0, bbox_y = 0, bbox_w = 1, bbox_h = 1,
    stringsAsFactors = FALSE
  )
  
  
  embeddings <- extract_miew_embeddings(model, manifest, file_col = "filepath",
                                        batch_size = 1, num_workers = 1, device = "cpu")
  
  expect_equal(nrow(embeddings), 2)
  expect_true(ncol(embeddings) > 0)
})


# remove_diagonal --------------------------------------------------------

test_that("remove_diagonal removes diagonal from a square matrix", {
  skip_if(!animl_py_available(), "animl_py not available")
  m <- matrix(1:9, nrow = 3, ncol = 3)
  result <- remove_diagonal(m)
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)  # n x (n-1)
})

test_that("remove_diagonal errors on non-square matrix", {
  skip_if(!animl_py_available(), "animl_py not available")
  m <- matrix(1:6, nrow = 2, ncol = 3)
  expect_error(remove_diagonal(m))
})

# euclidean_squared_distance ---------------------------------------------

test_that("euclidean_squared_distance returns correct shape", {
  skip_if(!animl_py_available(), "animl_py not available")
  a <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  result <- euclidean_squared_distance(a, a)
  expect_equal(dim(result), c(2L, 2L))
})

test_that("euclidean_squared_distance of identical vectors is zero", {
  skip_if(!animl_py_available(), "animl_py not available")
  a <- matrix(c(1.0, 2.0), nrow = 1, ncol = 2)
  result <- euclidean_squared_distance(a, a)
  expect_equal(as.numeric(result[1, 1]), 0, tolerance = 1e-5)
})

test_that("euclidean_squared_distance returns non-negative values", {
  skip_if(!animl_py_available(), "animl_py not available")
  a <- matrix(rnorm(6), nrow = 3, ncol = 2)
  result <- euclidean_squared_distance(a, a)
  expect_true(all(result >= 0))
})

# cosine_distance --------------------------------------------------------

test_that("cosine_distance of identical vectors is zero", {
  skip_if(!animl_py_available(), "animl_py not available")
  a <- matrix(c(1.0, 0.0), nrow = 1, ncol = 2)
  result <- cosine_distance(a, a)
  expect_equal(as.numeric(result[1, 1]), 0, tolerance = 1e-5)
})

test_that("cosine_distance of orthogonal vectors is one", {
  skip_if(!animl_py_available(), "animl_py not available")
  a <- matrix(c(1.0, 0.0), nrow = 1, ncol = 2)
  b <- matrix(c(0.0, 1.0), nrow = 1, ncol = 2)
  result <- cosine_distance(a, b)
  expect_equal(as.numeric(result[1, 1]), 1, tolerance = 1e-5)
})

test_that("cosine_distance returns correct shape", {
  skip_if(!animl_py_available(), "animl_py not available")
  a <- matrix(rnorm(6), nrow = 3, ncol = 2)
  b <- matrix(rnorm(4), nrow = 2, ncol = 2)
  result <- cosine_distance(a, b)
  expect_equal(dim(result), c(3L, 2L))
})

# compute_distance_matrix ------------------------------------------------

test_that("compute_distance_matrix with euclidean metric returns correct shape", {
  skip_if(!animl_py_available(), "animl_py not available")
  a <- matrix(rnorm(6), nrow = 3, ncol = 2)
  result <- compute_distance_matrix(a, a, metric = "euclidean")
  expect_equal(dim(result), c(3L, 3L))
})

test_that("compute_distance_matrix with cosine metric returns correct shape", {
  skip_if(!animl_py_available(), "animl_py not available")
  a <- matrix(rnorm(6), nrow = 3, ncol = 2)
  result <- compute_distance_matrix(a, a, metric = "cosine")
  expect_equal(dim(result), c(3L, 3L))
})

test_that("compute_distance_matrix errors on unknown metric", {
  skip_if(!animl_py_available(), "animl_py not available")
  a <- matrix(rnorm(4), nrow = 2, ncol = 2)
  expect_error(compute_distance_matrix(a, a, metric = "unknown"))
})

# compute_batched_distance_matrix ----------------------------------------

test_that("compute_batched_distance_matrix returns correct shape", {
  skip_if(!animl_py_available(), "animl_py not available")
  a <- matrix(rnorm(6), nrow = 3, ncol = 2)
  b <- matrix(rnorm(4), nrow = 2, ncol = 2)
  result <- compute_batched_distance_matrix(a, b, metric = "cosine", batch_size = 2)
  expect_equal(dim(result), c(3L, 2L))
})

test_that("compute_batched_distance_matrix matches unbatched result", {
  skip_if(!animl_py_available(), "animl_py not available")
  set.seed(42)
  a <- matrix(rnorm(6), nrow = 3, ncol = 2)
  unbatched <- compute_batched_distance_matrix(a, a, metric = "cosine", batch_size = 10)
  batched   <- compute_batched_distance_matrix(a, a, metric = "cosine", batch_size = 1)
  expect_equal(unbatched, batched, tolerance = 1e-5)
})
