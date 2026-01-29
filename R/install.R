# VARIABLE FOR VERSION
ANIML_VERSION <- "3.2.0"

#' Load animl-py if available
#'
#' @param envname name of python environment
#' @param python_version version of python to install
#'
#' @returns none
#' @export
#'
#' @examples
#' \dontrun{animl_install("animl_env", ANIML_VERSION, python_version="3.12")}
load_animl <- function(envname = "animl_env",
                          python_version = "3.12") {
  # 1. Load environment if exists
  packageStartupMessage(sprintf("1. Loading Python Environment (%s)...", envname))
  try_error <- try(reticulate::use_virtualenv(envname, required = TRUE), silent=TRUE)
  
  # 2. Install if not exists
  if (inherits(try_error, "try-error")) {
    #packageStartupMessage(try_error)
    msg <- sprintf(paste0("%s python environment not found. Run animl::animl_install().\n", 
                          "See `?animl::animl_install_instructions` for more detail."), envname)
    packageStartupMessage(msg)
    return(NULL)
  }
  # env exists
  else{
    # check animl-py installed
    packageStartupMessage("\n2. Checking animl-py version...")
    if(reticulate::py_module_available("animl")){
      animl_py <- reticulate::import("animl", delay_load = TRUE)
      current_version <- animl_py$'__version__'
      # check version match
      if (!identical(ANIML_VERSION, current_version)){
        # tell user to update animl-py
        packageStartupMessage(paste0("animl-py version conflicts with current version.\n",
                                     "To update animl-py, run animl::update_animl_py()."))
        return(NULL)
      }
      # correct version
      else{
        packageStartupMessage(paste0("animl successfully loaded."))
        return(animl_py)
      }
    }
    # animl_env exists but animl-py not installed
    else{
      packageStartupMessage(paste0("Python environment found but animl-py not installed.\n",
                                   "See `?animl::animl_install_instructions`."))
      return(NULL)
    }
  }
}


#' Load animl-py if available
#'
#' @param envname name of python environment
#' @param python_version version of python to install
#'
#' @return animl-py module
#' @export
#'
#' @examples
#' \dontrun{animl_py <- load_animl_py()}
animl_install <- function(envname = "animl_env", python_version = "3.12") {
  if (!interactive()) {
    stop("animl_install() must be run interactively.",call. = FALSE)
  }
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Please install the 'reticulate' package first: install.packages('reticulate')", call. = FALSE)
  }
  
  # 1. Load environment if exists
  packageStartupMessage(sprintf("1. Loading Python Environment (%s)...", envname))
  try_error <- try(reticulate::use_virtualenv(envname, required = TRUE), silent=TRUE)
  
  # 2. Install if not exists
  if (inherits(try_error, "try-error")) {
    packageStartupMessage(try_error)
    # 2. Create new environment
    packageStartupMessage("\n", sprintf("2. Creating a Python Environment (%s)", envname))
    animl_path <- tryCatch(expr = create_pyenv(python_version = python_version, envname = envname),
                           error = function(e) stop(e, "An error occur when animl_install was creating the Python Environment."))
    packageStartupMessage("animl successfully installed. Restart R session to see changes.\n")
  }
  # animl_env exists
  else{
    # check animl-py installed
    packageStartupMessage("\n2. Checking animl-py version...")
    if(reticulate::py_module_available("animl")){
      animl_py <- reticulate::import("animl", delay_load = TRUE)
      current_version <- animl_py$'__version__'
      # check version match
      if (!identical(ANIML_VERSION, current_version)){
        update_animl_py()
      }
    }
    # animl-py not yet installed
    else{
      packageStartupMessage("\n3. Installing animl-py...")
      package <- sprintf("animl==%s", ANIML_VERSION)
      reticulate::py_install(package, pip=TRUE)
      packageStartupMessage("animl successfully installed. Restart R session to see changes.\n")
    }
  }
  invisible()
}


#' Update animl-py version for the given environment
#'
#' @param envname name of python environment
#'
#' @returns None
#' @export
#'
#' @examples
#' \dontrun{update_animl_py(py_env = "animl_env")}
update_animl_py <- function(envname = "animl_env") {
  if (!interactive()) {
    stop(paste0("update_animl_py() must be run interactively.",
                "For non-interactive/CI installs, use system installers or CI actions.",
                call. = FALSE))
  }
  if (!requireNamespace("reticulate", quietly=TRUE)) {
    stop("Please install the 'reticulate' package first: install.packages('reticulate')", call. = FALSE)
  }
  
  # load animl-py, check version
  packageStartupMessage("animl-py version mismatch, reinstalling...")
  reticulate::use_virtualenv(envname)
  reticulate::py_install(sprintf("animl==%s", ANIML_VERSION), pip=TRUE)
  packageStartupMessage("animl successfully installed. Restart R session to see changes.\n")
}


#' Install python if necessary and create the environment animl_env 
#'
#' @param envname name of the conda environment to create / use (default "animl-py")
#' @param python_version python version to add to environment
#' 
#' @return invisible TRUE on success, otherwise stops or returns FALSE invisibly on failure
#' @export
create_pyenv <- function(envname = "animl_env", python_version = "3.12") {
  if (!interactive()) {
    stop(paste0("create_pyenv() must be run interactively.",
                "For non-interactive/CI installs, use system installers or CI actions.",
                call. = FALSE))
  }
  if (!requireNamespace("reticulate", quietly=TRUE)) {
    stop("Please install the 'reticulate' package first: install.packages('reticulate')", call. = FALSE)
  }
  
  # 1) Check is Python is installed and greater than 3.12
  check_python(initialize=TRUE)
  
  # 2) Create venv
  packageStartupMessage(paste0("Creating virtual environment '", envname, "' ..."))
  reticulate::virtualenv_create(envname=envname, python_version=python_version)
  
  # 3) Install animl-py
  packageStartupMessage("\n3. Installing animl-py...")
  package <- sprintf("animl==%s", ANIML_VERSION)
  reticulate::py_install(package, envname=envname, pip=TRUE)
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
check_python <- function(python_version = "3.12", initialize = TRUE) {
  if (!interactive()) {
    stop(paste0("check_python() must be run interactively.",
                "For non-interactive/CI installs, use system installers or CI actions.",
                call. = FALSE))
  }
  if (!requireNamespace("reticulate", quietly=TRUE)) {
    stop("Please install the 'reticulate' package first: install.packages('reticulate')", call. = FALSE)
  }
  # check if python is available
  python_test <- reticulate::py_available(initialize=initialize)
  if (python_test) { 
    current_version <- reticulate::py_discover_config()[["version"]]
    # check if correct version
    if (!identical(as.character(current_version), "3.12")) {
      packageStartupMessage(sprintf("Python %s not found, installing...", python_version))
      reticulate::install_python(version=python_version)
    }
    # 3.12 installed
    else{
      packageStartupMessage(sprintf("Found Python version %s compatible with animl.", current_version))
    }
  } 
  # no python installed at all
  else {
    packageStartupMessage(sprintf("Python %s not found, installing...", python_version))
    reticulate::install_python(version=python_version)
  }
  invisible()
}



#' Delete the animl_env environment
#'
#' @param envname python environment to remove
#'
#' @returns none
#' @export
#'
#' @examples
#' \dontrun{delete_pyenv('animl_env')}
delete_pyenv <- function(envname = "animl_env") {
  if (!interactive()) {
    stop(paste0("delete_pyenv() must be run interactively.",
                "For non-interactive/CI installs, use system installers or CI actions.",
                call. = FALSE))
  }
  if (!requireNamespace("reticulate", quietly=TRUE)) {
    stop("Please install the 'reticulate' package first: install.packages('reticulate')", call. = FALSE)
  }
  
  try(reticulate::virtualenv_remove(envname), silent = TRUE)
}
