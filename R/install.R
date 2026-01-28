# VARIABLE FOR VERSION
ANIML_VERSION <- "3.1.1"

#' Load animl-py if available
#'
#' @param py_env name of python environment
#' @param animl_version version of animl to install
#' @param python_version version of python to install
#'
#' @returns none
#' @export
#'
#' @examples
#' \dontrun{animl_install("animl_env", ANIML_VERSION, python_version="3.12")}
load_animl_py <- function(py_env = "animl_env",
                          animl_version = ANIML_VERSION,
                          python_version = "3.12") {
  # 1. Load environment if exists
  packageStartupMessage(sprintf("1. Loading Python Environment (%s)...", py_env))
  try_error <- try(reticulate::use_virtualenv(py_env, required = TRUE), silent=TRUE)
  
  # 2. Install if not exists
  if (inherits(try_error, "try-error")) {
    #packageStartupMessage(try_error)
    msg <- sprintf(paste0("%s python environment not found. Run animl::animl_install().\n", 
                          "See `?animl::animl_install_instructions` for more detail."), py_env)
    packageStartupMessage(msg)
  }
  # conda env exists
  else{
    # check animl-py installed
    packageStartupMessage("\n2. Checking animl-py version...")
    if(reticulate::py_module_available("animl")){
      animl_py <- reticulate::import("animl", delay_load = TRUE)
      py_version <- animl_py$'__version__'
      # check version match
      if (!identical(animl_version, py_version)){
        # tell user to update animl-py
        packageStartupMessage(paste0("animl-py version conflicts with current version.\n
                                     To update animl-py, run animl::update_animl_py()."))
      }
      # correct version
      else{
        packageStartupMessage(paste0("animl successfully loaded."))
        assign("animl_py", animl_py, envir = parent.env(environment()))
      }
      
    }
    else{
      # animl_env exists but animl-py not installed
      packageStartupMessage(paste0("Python environment found but animl-py not installed.\n",
                                   "See `?animl::animl_install_instructions`."))
    }
  }
}


#' Load animl-py if available
#'
#' @return animl-py module
#' @export
#'
#' @examples
#' \dontrun{animl_py <- load_animl_py()}
animl_install <- function(py_env = "animl_env",
                          animl_version = ANIML_VERSION,
                          python_version = "3.12",
                          confirm=TRUE) {
  # 1. Load environment if exists
  packageStartupMessage(sprintf("1. Loading Python Environment (%s)...", py_env))
  try_error <- try(reticulate::use_virtualenv(py_env, required = TRUE), silent=TRUE)
  
  # 2. Install if not exists
  if (inherits(try_error, "try-error")) {
    packageStartupMessage(try_error)
    # 2. Create new environment
    packageStartupMessage("\n", sprintf("2. Creating a Python Environment (%s)", py_env))
    animl_path <- tryCatch(expr = create_pyenv(python_version = python_version, py_env = py_env),
                           error = function(e) stop(e, "An error occur when animl_install was creating the Python Environment.",
                                                    "Check that you've accepted the conda TOS and restart the R session, before trying again."))
    # 3. Install animl-py
    packageStartupMessage("\n3. Installing animl-py...")
    package <- sprintf("animl==%s", animl_version)
    reticulate::py_install(package, envname=py_env, pip=TRUE)
    
    packageStartupMessage("animl successfully installed. Restart R session to see changes.\n")
  }
  # env exists
  else{
    # check animl-py installed
    packageStartupMessage("\n2. Checking animl-py version...")
    if(reticulate::py_module_available("animl")){
      animl_py <- reticulate::import("animl", delay_load = TRUE)
      py_version <- animl_py$'__version__'
      # check version match
      if (!identical(animl_version, py_version)){update_animl_py()}
    }
    # animl-py not yet installed
    else{
      packageStartupMessage("\n3. Installing animl-py...")
      package <- sprintf("animl==%s", animl_version)
      reticulate::use_virtualenv(py_env)
      reticulate::py_install(package, pip=TRUE)
    }
  }
  invisible()
}


#' Update animl-py version
#'
#' @param py_env name of python environment
#' @param animl_version version of animl to install
#'
#' @returns None
#' @export
#'
#' @examples
#' \dontrun{update_animl_py(py_env = "animl_env", animl_version = ANIML_VERSION)}
update_animl_py <- function(py_env = "animl_env",
                            animl_version = ANIML_VERSION) {
  # load animl-py, check version
  packageStartupMessage("3. animl-py version mismatch, reinstalling...")
  reticulate::use_virtualenv(py_env)
  reticulate::py_install(sprintf("animl==%s", animl_version), pip=TRUE)
  packageStartupMessage("")
  # assign animl-py for use
  animl_py <- reticulate::import("animl", delay_load = TRUE)
  assign("animl_py", animl_py, envir = parent.env(environment()))
}



#' Check that the python version is compatible with the current version of animl-py
#'
#' @param python_version version of python to install
#' @param initialize load reticulate library if true
#'
#' @returns none
#' @export
#'
#' @examples
#' \dontrun{check_python(initialize=FALSE)}
check_python <- function(python_version="3.12", initialize = TRUE) {
  python_test <- reticulate::py_available(initialize=initialize)
  if (python_test) { 
    py_version <- reticulate::py_discover_config()[["version"]]
    # check if correct version
    if (utils::compareVersion(as.character(py_version), "3.12") == -1) {
      packageStartupMessage(sprintf("Python %s not found, installing.", py_version))
      reticulate::install_python(version=python_version)
    }
    else{
      packageStartupMessage(sprintf("Found Python version %s compatible with animl.", py_version))
    }
  } 
  else {
    packageStartupMessage(sprintf("Python %s not found, installing.", py_version))
    reticulate::install_python(version=python_version)
  }
}



#' Create the environment animl_env 
#'
#' @param python_version python version for new environment
#' @param py_env name of python environment
#'
#' @returns python path for new environment
#' @export
#'
#' @examples
#' \dontrun{create_env("3.12", py_env='animl_env')}
create_pyenv <- function(python_version, py_env = "animl_env") {
  #Check is Python is greather than 3.9
  check_python(initialize=TRUE)
  pyenv_path <- reticulate::virtualenv_create(py_env, python_version = python_version)
  pyenv_path
}


#' Delete the animl_env environment
#'
#' @param py_env python environment to remove
#'
#' @returns none
#' @export
#'
#' @examples
#' \dontrun{delete_pyenv('animl_env')}
delete_pyenv <- function(py_env = "animl_env") {
  try(reticulate::virtualenv_remove(py_env), silent = TRUE)
}
