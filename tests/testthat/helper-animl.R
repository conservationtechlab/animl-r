animl_py_available <- function() {
  !is.null(animl:::.animl_internal$animl_py)
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
