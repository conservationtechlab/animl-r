# VARIABLE FOR VERSION
ANIML_VERSION <- "3.0.0"
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
  message(sprintf("1. Loading Python Environment (%s)...", py_env))
  try_error <- try(reticulate::use_condaenv(py_env, required = TRUE), silent=TRUE)
  
  # 2. Install if not exists
  if (inherits(try_error, "try-error")) {
    message(sprintf("%s not found \n", py_env))
    # 2. Create new environment
    message("\n", sprintf("2. Creating a Python Environment (%s)", py_env))
    animl_path <- tryCatch(expr = create_pyenv(python_version = python_version, py_env = py_env),
                           error = function(e) stop(e, "An error occur when animl_install was creating the Python Environment.",
                                                    "Check that you've accepted the conda TOS and restart the R session, before trying again."))
    #print(animl_path)
    # 3. Install animl-py
    message("\n3. Installing animl-py...")
    package = sprintf("animl==%s", animl_version)
    reticulate::py_install(package, envname=py_env, pip=TRUE)
    
    message("animl successfully installed. Restart R session to see changes.\n")
    invisible(TRUE)
    return(FALSE)
  }
  # conda env exists
  else{
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
  
  message("animl-py loaded successfully.")
  return(animl_py)
}



animl_update <- function(){
  

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
  else { stop("Unable to find a Python version, you will need to fix before run ",
              "animl_initialize(). For more details run reticulate::py_available()")
  }
  if (utils::compareVersion(as.character(py_version), "3.9") == -1) {
    stop("animl needs Python >=3.9")
  }
  message(sprintf("Python version %s compatible with animl.", py_version))
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
  check_python(initialize=FALSE)
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
