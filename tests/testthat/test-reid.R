test_that("load_miew requires a model file", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("load_miew requires a real model file — test manually with a local model")
})

test_that("extract_miew_embeddings requires a loaded model", {
  skip_if(!animl_py_available(), "animl_py not available")
  skip("extract_miew_embeddings requires a real MiewID model")
})
