.animl_internal <- new.env(parent = emptyenv())
.animl_internal$animl_py <- NULL

.onLoad <- function(libname, pkgname) {
  animl_py <- load_animl()
  assign("animl_py", animl_py, envir = .animl_internal)
  invisible()
}


.onAttach <- function(libname, pkgname) {
  # Only give a very small hint to interactive users (not required)
  if (interactive()) {
    if (requireNamespace("reticulate", quietly = TRUE)) {
      pkgmsg <- paste0(
        "animl: requires Python 3.12 and animl-py 3.1.1. If animl fails to load, ",
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


#' Installation Instructions for animl-r Python dependencies
#' 
#' 
#' @export
animl_install_instructions <- function() {
  cat(
    "animl: instructions to prepare a Python environment for optional features\n\n",
    "Run animl::animl_install() to set up Python 3.12 environment and install animl-py dependency.\n\n",
    "Virtualenv/pip alternative (requires python 3.12 installed):\n",
    "  python -m venv ~/venvs/animl_env\n",
    "  source ~/venvs/animl_env/bin/activate\n",
    "  pip install --upgrade pip\n",
    "  pip install numpy pandas # add other required pkgs\n",
    "  In R: reticulate::use_python('~/venvs/animl-py/bin/python', required = TRUE)\n\n",
    "Note: Do NOT rely on automatic installation from inside the package; ",
    "install Python packages manually and configure reticulate.",
    sep = ""
  )
  invisible(NULL)
}