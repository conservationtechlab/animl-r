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
#' \dontrun{train_main('training_cfg.yml')}
train_main <- function(cfg){
  animl_py <- load_animl_py()
  animl_py$train_main(cfg)
}


#' Test a model with a Config file
#'
#' @param cfg config .yml file containing test settings
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{test_main('training_cfg.yml')}
test_main <- function(cfg){
  animl_py <- load_animl_py()
  animl_py$test_main(cfg)
}