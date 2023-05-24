#' Setup Conda Environment
#'
#' @param config environment config yaml file
#'
#' @return none
#' @import reticulate
#' @export
#'
#' @examples
#' \dontrun{
#' setupEnv()
#' }
setupEnv <- function(config = "animl/animlenv.yml"){
  try(
    reticulate::conda_create(environment = config),
    silent=TRUE
  )
  reticulate::use_condaenv("animlenv")
}
