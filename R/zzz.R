.animl_internal <- new.env(parent = emptyenv())
.animl_internal$animl_py <- NULL
.animl_internal$animl_py_available <- FALSE


.onLoad <- function(libname, pkgname) {
  # Skip Python loading during R CMD check to avoid interference with checks
  if (nchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) > 0L) {
    return(invisible())
  }
  # Try to load animl-py, but don't fail if it's not available
  tryCatch(
    {
      load_animl(interactive = interactive())
      assign("animl_py_available", TRUE, envir = .animl_internal)
    },
    error = function(e) {
      # Store the error state so we can warn users later
      assign("animl_py_available", FALSE, envir = .animl_internal)
    })
}

.onAttach <- function(libname, pkgname) {
  # Skip startup message during R CMD check
  if (nchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) > 0L) {
    return(invisible())
  }
  if (!exists("animl_py_available", envir = .animl_internal) ||
      !get("animl_py_available", envir = .animl_internal)) {
    packageStartupMessage(
      "Warning: animl-py is not installed.\n",
      "animl functionality requires Python 3.12 and animl-py.\n",
      "Run animl::animl_install() to set up the Python environment.\n",
      "See ?animl::animl_install_instructions for more details."
    )
  }
}