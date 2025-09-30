.onLoad <- function(libname, pkgname) {
  message("Loading animl package...")
  load_animl_py()
  message("animl-py loaded successfully.")
}
