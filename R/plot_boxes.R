#' Plot bounding boxes on image from md results
#'
#' @param rows row or rows of images in which the bounding box will be plotted
#' @param file_col  Column name containing file paths
#' @param min_conf minimum confidence to plot box
#' @param prediction If True, display the prediction label alongside the bounding box.

#'
#' @return no return value, produces bounding box in plot panel
#' @export
#'
#' @examples
#' \dontrun{
#' test_image <- classify(classifier_model, test_image, file_col='filepath')
#' plot_box(test_image, file_col='filepath', minconf = 0.5, prediction=TRUE)
#' }
plot_box <- function(rows, file_col='filepath', min_conf = 0, prediction=FALSE) {
  animl_py <- load_animl_py()
  animl_py$plot_box(rows, file_col=file_col, min_conf=min_conf, prediction=prediction)

}


#' Plot all bounding boxes in a manifest
#'
#' @param manifest manifest of detections
#' @param out_dir Name of the output directory
#' @param file_col Column name containing file paths
#' @param min_conf Confidence threshold to plot the box
#' @param prediction flag determining whether prediction be printed alongside bounding box
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{plot_all_bounding_boxes(manifest, 'Plots/''')}
plot_all_bounding_boxes <- function(manifest, out_dir, file_col='frame',
                                    min_conf=0.1, prediction=FALSE){
  animl_py <- load_animl_py()
  animl_py$plot_all_bounding_boxes(manifest, out_dir, file_col=file_col, 
                                   min_conf=min_conf, prediction=prediction)
}


#' Read a CSV manifest file and perform box plotting on the images.
#'
#' @param csv_file Path to the CSV file.
#' @param output_dir Saved location  of boxed images output dir.
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{plot_from_file('manifest.csv', 'Plots/''')}
plot_from_file <- function(csv_file, output_dir){
  animl_py <- load_animl_py()
  animl_py$plot_from_file(csv_file, output_dir)
}