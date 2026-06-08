.animl_internal <- new.env(parent = emptyenv())
.animl_internal$animl_py <- NULL

.onLoad <- function(libname, pkgname) {
  animl_py <- load_animl()
  invisible()
}


.onAttach <- function(libname, pkgname) {
  # Only give a very small hint to interactive users (not required)
  if (interactive()) {
    if (requireNamespace("reticulate", quietly = TRUE)) {
      pkgmsg <- paste0(
        sprintf("animl: requires Python 3.12 and animl-py %s. If animl fails to load, ", ANIML_VERSION),
        "see `?animl::animl_install_instructions`."
      )
    } else {
      pkgmsg <- paste0(
        "animl: For all features install the 'reticulate' package ",
        "(install.packages('reticulate')) and follow `?animl::animl_install_instructions`."
      )
    }
    packageStartupMessage(pkgmsg)
  }
  invisible()
}