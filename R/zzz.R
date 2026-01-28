.onLoad <- function(libname, pkgname) {
  load_animl_py()
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
    "Recommended (conda):\n",
    "  conda create -n animl-py python=3.10\n",
    "  conda activate animl-py\n",
    "  conda install -c conda-forge numpy pandas # add other required pkgs\n",
    "  In R: reticulate::use_condaenv('animl-py', required = TRUE)\n\n",
    "Virtualenv/pip alternative:\n",
    "  python -m venv ~/venvs/animl-py\n",
    "  source ~/venvs/animl-py/bin/activate\n",
    "  pip install --upgrade pip\n",
    "  pip install numpy pandas # add other required pkgs\n",
    "  In R: reticulate::use_python('~/venvs/animl-py/bin/python', required = TRUE)\n\n",
    "Note: Do NOT rely on automatic installation from inside the package; ",
    "install Python packages manually and configure reticulate.",
    sep = ""
  )
  invisible(NULL)
}