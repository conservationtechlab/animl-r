.onLoad <- function(libname, pkgname) {
  animl_env_avail <- animl_install()
  if (animl_env_avail) {
    animl_py <- load_animl_py()
    assign("animl_py", animl_py, envir = parent.env(environment()))
  }
  else{
    .stop_animl_failed()
  }
}

.stop_animl_failed <- function(){
  if (interactive()) {
    warning('animl_env load failed')
  } else {
    # non-interactive / scripts should get a clean error
    stop('animl_env load failed', call. = FALSE)
  }
}