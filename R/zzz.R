.onLoad <- function(libname, pkgname) {
  message("Loading animl package...")
  load_animl-py()
  message("animl-py loaded successfully.")
}
