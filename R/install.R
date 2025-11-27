# VARIABLE FOR VERSION
ANIML_VERSION <- "3.1.0"
animl_py <- NULL

#' Create a miniconda environment for animl and install animl-py
#'
#' @param py_env name of python environment
#' @param animl_version version of animl to install
#' @param python_version version of python to install
#' @param confirm allow user input
#'
#' @returns none
#' @export
#'
#' @examples
#' \dontrun{animl_install("animl_env", ANIML_VERSION, python_version="3.9", confirm=TRUE)}
animl_install <- function(py_env = "animl_env",
                          animl_version = ANIML_VERSION,
                          python_version = "3.12",
                          confirm=TRUE) {
  # 1. Load environment if exists
  packageStartupMessage(sprintf("1. Loading Python Environment (%s)...", py_env))
  try_error <- try(reticulate::use_condaenv(py_env, required = TRUE), silent=TRUE)
  
  # 2. Install if not exists
  if (inherits(try_error, "try-error")) {
    packageStartupMessage(sprintf("%s not found \n", py_env))
    # 2. Create new environment
    packageStartupMessage("\n", sprintf("2. Creating a Python Environment (%s)", py_env))
    animl_path <- tryCatch(expr = create_pyenv(python_version = python_version, py_env = py_env),
                           error = function(e) stop(e, "An error occur when animl_install was creating the Python Environment.",
                                                    "Check that you've accepted the conda TOS and restart the R session, before trying again."))
    #print(animl_path)
    # 3. Install animl-py
    packageStartupMessage("\n3. Installing animl-py...")
    package <- sprintf("animl==%s", animl_version)
    reticulate::py_install(package, envname=py_env, pip=TRUE)
    
    packageStartupMessage("animl successfully installed. Restart R session to see changes.\n")
    invisible(TRUE)
    return(FALSE)
  }
  # conda env exists
  else{
    # check animl version
    update_animl_py()
    return(TRUE)
  }
}


#' Load animl-py if available
#'
#' @return animl-py module
#' @export
#'
#' @examples
#' \dontrun{animl_py <- load_animl_py()}
load_animl_py <- function() {
  if(reticulate::py_module_available("animl")){
    animl_py <- reticulate::import("animl", delay_load = TRUE)
  }
  else{ stop('animl_env environment must be loaded first via reticulate') }
  
  packageStartupMessage("animl-py loaded successfully.")
  return(animl_py)
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
  animl_py <- reticulate::import("animl", delay_load = TRUE)
  version_error <- try(animl_py$'__version__')
  if (inherits(version_error, "try-error")){
    print("animl-py version: ", version_error)
    reticulate::py_install(sprintf("animl==%s", animl_version), envname=py_env, pip=TRUE)
  }
  else{
    r_version <- strsplit(animl_version, ".", fixed = TRUE)[[1]]
    py_version <- strsplit(version_error, ".", fixed = TRUE)[[1]]

    #r == py
    if (!identical(r_version, py_version)){
      packageStartupMessage("animl-py version mismatch, reinstalling...")
      reticulate::py_install(sprintf("animl==%s", animl_version), envname=py_env, pip=TRUE)
    }
  }
}




#' Check that the python version is compatible with the current version of animl-py
#'
#' @param initialize load reticulate library if true
#'
#' @returns none
#' @export
#'
#' @examples
#' \dontrun{check_python(initialize=FALSE)}
check_python <- function(initialize = TRUE) {
  python_test <- reticulate::py_available(initialize=initialize)
  if (python_test) { py_version <- reticulate::py_discover_config()[["version"]]
  } 
  else { stop("Unable to find a Python installation.",
              "Please install Python befor running animl_initiaialzer().",
              "For more details run reticulate::py_discover_config()")
  }
  if (utils::compareVersion(as.character(py_version), "3.12") == -1) {
    stop("animl needs Python >=3.12")
  }
  packageStartupMessage(sprintf("Python version %s compatible with animl.", py_version))
}



#' Create the miniconda environment animl_env 
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
  pyenv_path <- reticulate::conda_create(py_env, python_version = python_version)
  print(pyenv_path)
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
  try(reticulate::conda_remove(py_env), silent = TRUE)
}
