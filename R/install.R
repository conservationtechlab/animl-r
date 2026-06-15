# VARIABLE FOR VERSION
ANIML_VERSION <- "3.3.0"

#' Load animl-py if available
#'
#' @param envname name of python environment
#' @param python_version version of python to install
#' @param .silent suppress onload packageStartupMessage
#'
#' @returns none
#' @export
#'
#' @examples
#' \dontrun{animl_install("animl_env", ANIML_VERSION, python_version="3.12")}
load_animl <- function(envname = "animl_env",
                       python_version = "3.12",
                       .silent = FALSE) {
  
  if (!interactive()) {return(invisible())}
  
  msg <- function(text) {
    if (!.silent) {
      packageStartupMessage(text)
    }
  }
  
  # 1. Load environment if exists — try venv first, then conda
  msg(sprintf("1. Loading Python Environment (%s)...", envname))
  
  # Try venv first
  try_venv <- tryCatch(
    reticulate::use_virtualenv(envname, required = TRUE),
    error = function(e) {
      return(NULL)
    }
  )
  
  # If venv fails, try conda
  if (is.null(try_venv)) {
    packageStartupMessage("virtualenv not found, trying conda...")
    try_conda <- tryCatch(
      reticulate::use_condaenv(envname, required = TRUE),
      error = function(e) {
        return(NULL)
      }
    )
    try_error <- try_conda
  } 
  else {
    try_error <- try_venv
  }
  
    # 2. Install if neither found
  if (is.null(try_error)) {
    msg(sprintf(paste0("%s python environment not found. Run animl::animl_install().\n",
                       "See `?animl::animl_install_instructions` for more detail."), envname))
  }
  # env exists
  else {
    # check animl-py installed
    msg("\n2. Checking animl-py version...")
    if (reticulate::py_module_available("animl")) {
      animl_py <- reticulate::import("animl", delay_load = TRUE)
      current_version <- animl_py$'__version__'
      # check version match
      if (!identical(ANIML_VERSION, current_version)) {
        msg(paste0("animl-py version conflicts with current version.\n",
                   "To update animl-py, run animl::update_animl_py()."))
      }
      # correct version
      else {
        msg("animl successfully loaded.")
        assign("animl_py", animl_py, envir = .animl_internal)
      }
      # 4) Check external dependencies
      check_animl_py()
    }
    # animl_env exists but animl-py not installed
    else {
      msg(paste0("Python environment found but animl-py not installed.\n",
                 "See `?animl::animl_install_instructions`."))
    }
  }
  invisible()
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
    check_animl_py()
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
  
  # get env
  try_venv  <- try(reticulate::use_virtualenv(envname, required = TRUE), silent = TRUE)
  if (inherits(try_venv, "try-error")) {
    try_conda <- try(reticulate::use_condaenv(envname, required = TRUE), silent = TRUE)
    try_error <- try_conda
  } 
  else {try_error <- try_venv}
  # env not found
  if (inherits(try_error, "try-error")) {
    msg <- sprintf(paste0("%s python environment not found. Run animl::animl_install().\n",
                          "See `?animl::animl_install_instructions` for more detail."), envname)
    packageStartupMessage(msg)
  }
  # reinstall animl
  else{
    reticulate::py_install(sprintf("animl==%s", ANIML_VERSION), pip=TRUE)
    packageStartupMessage("animl successfully installed. Restart R session to see changes.\n")
    
  }
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
  
  # 4) Check external dependencies
  check_animl_py()
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


#' Installation Instructions for animl-r Python dependencies
#' 
#' 
#' @export
animl_install_instructions <- function() {
  cat(
    "animl: instructions to prepare a Python environment for optional features\n\n",
    "Run animl::animl_install() to set up Python 3.12 environment and install animl-py dependency.\n\n",
    "Manual virtualenv/pip alternative (requires python 3.12 installed):\n",
    "  python -m venv ~/venvs/animl_env\n",
    "  WIN: ~\\.virtualenvs\\animl_env\\Scripts\\activate\n",
    "  LIN: source ~/.virtualenvs/animl_env/bin/activate\n",
    "  pip install --upgrade pip\n",
    "  pip install animl\n\n",
    "Restart R session and reload animl library.",
    sep = ""
  )
  invisible(NULL)
}


#' Check if animl-py can connect to exiftool and CUDA
#'
#' @export
check_animl_py <- function(){
  if(reticulate::py_module_available("animl")){
    animl_py <- reticulate::import("animl", delay_load = TRUE)
    
    exif <- animl_py$check_exiftool()
    packageStartupMessage(sprintf("Exiftool installed and available: %s", as.character(exif)))
    
    torch_cuda <- animl_py$check_torch_cuda()
    packageStartupMessage(sprintf("CUDA available to PyTorch: %s", as.character(torch_cuda)))
    
    torch_onnx <- animl_py$check_onnx_cuda()
    packageStartupMessage(sprintf("CUDA available to Onnx: %s", as.character(torch_onnx)))
  }
  else{
    packageStartupMessage("Error: animl-py is not installed.")
  }
}
