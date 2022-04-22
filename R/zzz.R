.onLoad <- function(libname, pkgname) {
  print("Setting up conda environment.")
  reticulate::configure_environment(pkgname)
  reticulate::use_condaenv("animl")
}