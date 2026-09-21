# Helper functions for downloading and managing test assets

get_test_model <- function() {
  model_url <- "https://github.com/conservationtechlab/animl-r/releases/download/v3.3.0/test_model.pt"
  model_path <- file.path(tempdir(), "test_model.pt")
  
  if (!file.exists(model_path)) {
    tryCatch(
      download.file(model_url, model_path, quiet = TRUE, mode = "wb"),
      error = function(e) {
        warning("Could not download test model: ", conditionMessage(e))
        return(NULL)
      }
    )
  }
  
  return(if (file.exists(model_path)) model_path else NULL)
}

create_test_detections <- function(n_files = 2) {
  # Create minimal test dataframe
  data.frame(
    filepath = rep(system.file("extdata", package = "animl"), n_files),
    confidence = runif(n_files, 0.8, 0.99)
  )
}

animl_test_asset_url <- function(asset = c("model", "classes")) {
  asset <- match.arg(asset)

  switch(asset,
    model = "https://github.com/conservationtechlab/animl-py/releases/download/model/sdzwa_southwest_v3.pt",
    classes = "https://github.com/conservationtechlab/animl-py/releases/download/model/sdzwa_southwest_v3_classes.csv"
  )
}

animl_test_asset_path <- function(asset = c("model", "classes")) {
  asset <- match.arg(asset)
  cache_dir <- file.path(tempdir(), "animl-test-assets")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  file.path(cache_dir, basename(animl_test_asset_url(asset)))
}

get_animl_test_asset <- function(asset = c("model", "classes")) {
  asset <- match.arg(asset)
  asset_path <- animl_test_asset_path(asset)

  if (file.exists(asset_path)) {
    return(asset_path)
  }

  ok <- tryCatch({
    utils::download.file(
      url = animl_test_asset_url(asset),
      destfile = asset_path,
      mode = "wb",
      quiet = TRUE
    )
    TRUE
  }, error = function(e) {
    FALSE
  }, warning = function(w) {
    FALSE
  })

  if (!ok || !file.exists(asset_path)) {
    return(NULL)
  }

  asset_path
}

fetch_and_convert_miewid <- function(out_path) {
  animl_py <- .animl_internal$animl_py
  animl_py$fetch_and_convert_miewid(out_path)
}

get_miewid_test_asset <- function() {
  cache_path <- testthat::test_path("_cache", "miewid_v3.bin")
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  ok <- fetch_and_convert_miewid(cache_path)
  if (isTRUE(ok) && file.exists(cache_path)) cache_path else NULL
}
