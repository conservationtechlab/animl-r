#' Select Best Classification From Multiple Frames
#'
#' @param animals dataframe of all frames including species classification
#' @param how method for selecting best prediction, defaults to most frequent
#' @param outfile file path to which the data frame should be saved
#'
#' @return dataframe with new prediction in "Species" column
#' @export
#'
#' @examples
#' \dontrun{
#' mdanimals <- classifyVideo(mdanimals)
#' }
poolCrops <- function(animals, how = "count", outfile = NA) {
  if (checkFile(outfile)) { return(loadData(outfile))}
  if (!is(animals, "data.frame")) { stop("'animals' must be DataFrame")}

  animals$Species <- animals$prediction

  sort <- with(animals, order(Site, Camera, NewName))
  animals <- animals[sort, ]

  videonames <- unique(animals$NewName)

  steps <- length(videonames)
  pb <- pbapply::startpb(1, steps)

  for (i in 1:steps) {
    v <- videonames[i]
    sequence <- animals[animals$NewName == v, ]
    guesses <- sequence %>%
      dplyr::group_by(sequence$prediction) %>%
      dplyr::summarise(mean = mean(sequence$confidence), n = dplyr::n())

    if (how == "conf") {
      best <- guesses[which.max(guesses$mean), ]
    } else {
      best <- which.max(guesses$n)
      guess <- guesses[best, ]
      if (guess$prediction == "empty" && nrow(guesses) > 1) {
        newguesses <- guesses[-best, ]
        guess <- newguesses[which.max(newguesses$mean), ]
      }
    }
    animals[animals$NewName == v, ]$Species <- guess$prediction
    pbapply::setpb(pb, i)
  }
  pbapply::setpb(pb, steps)
  pbapply::closepb(pb)
  
  # save data
  if(!is.na(outfile)){ saveData(animals, outfile)}
  animals
}