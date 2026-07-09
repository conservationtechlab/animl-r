# VARIABLE FOR VERSION
ANIML_VERSION <- "3.3.1"

#' Load animl-py if available
#'
#' @param envname name of python environment
#' @param python_version version of python to install
#' @param interactive bool, only message if interactive
#'
#' @returns none
#' @export
#'
#' @examples
#' \dontrun{animl_install("animl_env", ANIML_VERSION, python_version="3.12")}
load_animl <- function(envname = "animl_env",
                       python_version = "3.12",
                       interactive = TRUE) {
  
  msg <- function(text) {
    if (interactive) {
      message(text)
    }
  }
  
  # 1. Load environment if exists — try venv first, then conda
  msg(sprintf("1. Loading Python Environment (%s)...", envname))
  
  # Try venv first
  try_venv <- tryCatch(
    reticulate::use_virtualenv(envname, required = TRUE),
    error = function(e) {
      return('not_found')
    }
  )
  
  # If venv fails, try conda
  if (is.null(try_venv)) {
    msg("virtualenv not found, trying conda...")
    try_conda <- tryCatch(
      reticulate::use_condaenv(envname, required = TRUE),
      error = function(e) {
        return('not_found')
      }
    )
    try_error <- try_conda
  } 
  else {
    try_error <- try_venv
  }
  
  # 2. Install if neither found
  if (identical(try_error,'not_found')) {
    msg(sprintf(paste0("%s python environment not found. Run animl::animl_install().\n",
                       "See `?animl::animl_install_instructions` for more detail."), envname))
  }
  # env exists
  else {
    # check animl-py installed
    msg("\n2. Checking animl-py version...")
    if (reticulate::py_module_available("animl")) {
      animl_py <- reticulate::import("animl", delay_load = TRUE)
      animl_py_version <- animl_py$'__version__'
      
      # check version match
      if (!(extract_major_minor(animl_py_version) == extract_major_minor(ANIML_VERSION))) {
        msg(paste0("animl-py version conflicts with current version.\n",
                   "To update animl-py, run animl::update_animl_py()."))
      }
      # correct version
      else {
        msg(sprintf("animl %s successfully loaded.", ANIML_VERSION))
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


#' Compare to Animl-Py version, ignoring patch number
#'
#' @param version_string the version to extract substring for
#'
#' @returns version_string with only major and minor release
#'
#' @examples
#' \dontrun{extract_major_minor("3.4.1")}
extract_major_minor <- function(version_string) {
  parts <- strsplit(version_string, "\\.")[[1]]
  paste(parts[1], parts[2], sep = ".")
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
  message(sprintf("1. Loading Python Environment (%s)...", envname))
  try_error <- try(reticulate::use_virtualenv(envname, required = TRUE), silent=TRUE)
  
  # 2. Install if not exists
  if (inherits(try_error, "try-error")) {
    message(try_error)
    # 2. Create new environment
    message("\n", sprintf("2. Creating a Python Environment (%s)", envname))
    animl_path <- tryCatch(expr = create_pyenv(python_version = python_version, envname = envname),
                           error = function(e) stop(e, "An error occur when animl_install was creating the Python Environment."))
    message("animl successfully installed. Restart R session to see changes.\n")
  }
  # animl_env exists
  else{
    # check animl-py installed
    message("\n2. Checking animl-py version...")
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
      message("\n3. Installing animl-py...")
      package <- sprintf("animl==%s", ANIML_VERSION)
      reticulate::py_install(package, pip=TRUE)
      message("animl successfully installed. Restart R session to see changes.\n")
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
  message("animl-py version mismatch, reinstalling...")
  
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
    message(msg)
  }
  # reinstall animl
  else{
    reticulate::py_install(sprintf("animl==%s", ANIML_VERSION), pip=TRUE)
    message("animl successfully installed. Restart R session to see changes.\n")
    
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
  message(paste0("Creating virtual environment '", envname, "' ..."))
  reticulate::virtualenv_create(envname=envname, python_version=python_version)
  
  # 3) Install animl-py
  message("\n3. Installing animl-py...")
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
      message(sprintf("Python %s not found, installing...", python_version))
      reticulate::install_python(version=python_version)
    }
    # 3.12 installed
    else{
      message(sprintf("Found Python version %s compatible with animl.", current_version))
    }
  } 
  # no python installed at all
  else {
    message(sprintf("Python %s not found, installing...", python_version))
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
    torch_cuda <- animl_py$check_torch_cuda()
    torch_onnx <- animl_py$check_onnx_cuda()
    
    if(interactive()){
      message(sprintf("Exiftool installed and available: %s", as.character(exif)))
      message(sprintf("CUDA available to PyTorch: %s", as.character(torch_cuda)))
      message(sprintf("CUDA available to Onnx: %s", as.character(torch_onnx)))
    }
  }
  else{ if(interactive()){ message("Error: animl-py is not installed.")} }
}
