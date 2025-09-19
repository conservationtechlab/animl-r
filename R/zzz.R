.onLoad <- function(libname, pkgname) {
  requireNamespace("reticulate", quietly = TRUE)
  
  if (is.null(reticulate::miniconda_path())) {
    message("Miniconda not found. You will be prompted to install and accept the TOS.")
    # This will prompt user to accept when called:
    reticulate::install_miniconda()
  }
  
  # animl_env does not exist, create
  if (!reticulate::condaenv_exists("animl_env")){
    message("animl_env not found, installing... ")
    animl_install()
  }
  # use animl_env
  Sys.unsetenv("RETICULATE_PYTHON")
  reticulate::use_condaenv("animl_env", required = TRUE)
  
  # animl-py not available in environment, create
  if(!reticulate::py_module_available("animl")){ animl_install() }


  animl_py <- reticulate::import("animl")
 # if (animl_py$`__version__` != ANIML_VERSION){
#    animl_update()
#  }

  print("animl-py loaded")
}