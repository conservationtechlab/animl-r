#' Model Training
#'
#' This module provides functions for training and testing classifier models
#'
#' Kyra Swanson 2025



#' Train a model with a Config file
#'
#' @param cfg config .yml file containing training settings
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{train_classifier('training_cfg.yml')}
train_classifier <- function(cfg){
  animl_py <- .animl_internal$animl_py
  animl_py$train_classifier(cfg)
}


#' Test a model with a Config file
#'
#' @param cfg config .yml file containing test settings
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{test_classifier('training_cfg.yml')}
test_classifier <- function(cfg){
  animl_py <- .animl_internal$animl_py
  animl_py$test_classifier(cfg)
}