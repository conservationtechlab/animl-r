.onLoad <- function(libname, pkgname) {
 message("Loading animl package...")
 animl_env_avail <- animl_install()
 if (animl_env_avail) {
   animl_py <- load_animl_py()
   assign("animl_py", animl_py, envir = parent.env(environment()))
 }
}
