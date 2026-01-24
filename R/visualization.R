#' Plot bounding boxes on image from md results
#'
#' @param rows row or rows of images in which the bounding box will be plotted
#' @param file_col  Column name containing file paths
#' @param min_conf minimum confidence to plot box
#' @param label_col Column name containing class to print above the box. If None, no label is printed.
#' @param show_confidence If true, show confidence score above the box.
#' @param colors Named list mapping class labels to BGR color tuples for the bounding boxes.
#' @param detector_labels Named list mapping detector categories to human-readable labels.
#' @param return_img If true, return the image array with boxes overlaid, otherwise display it 
#'
#' @return no return value, produces bounding box in plot panel
#' @export
#'
#' @examples
#' \dontrun{
#' test_image <- classify(classifier_model, test_image, file_col='filepath')
#' plot_box(test_image, file_col='filepath', minconf = 0.5, prediction=TRUE)
#' }
plot_box <- function(rows, file_col='filepath', min_conf=0, label_col=NULL,
                     show_confidence=FALSE, colors=NULL, detector_labels=NULL,
                     return_img=FALSE) {
  animl_py <- get("animl_py", envir = parent.env(environment()))
  animl_py$plot_box(rows, file_col=file_col, min_conf=min_conf, label_col=label_col,
                    show_confidence=show_confidence, colors=colors, detector_labels=detector_labels,
                    return_img=return_img)
  }


#' Plot all bounding boxes in a manifest
#'
#' @param manifest manifest of detections
#' @param out_dir Name of the output directory
#' @param file_col Column name containing file paths
#' @param min_conf Confidence threshold to plot the box
#' @param label_col Column name containing class to print above the box. If None, no label is printed.
#' @param show_confidence If true, show confidence score above the box.
#' @param colors Named list mapping class labels to BGR colors for the bounding boxes.
#' @param detector_labels Named list mapping detector categories to human-readable labels.
#' 
#' @return None
#' @export
#'
#' @examples
#' \dontrun{plot_all_bounding_boxes(manifest, 'Plots/', label_col='prediction',
#'                                  show_confidence=TRUE, 
#'                                  colors=list("1" = c(0, 255, 0),"2" = c(0, 0, 255),"3" = c(255, 0, 0)))}
plot_all_bounding_boxes <- function(manifest, out_dir, file_col='filepath', min_conf=0.1, 
                                    label_col=FALSE, show_confidence=FALSE,
                                    colors=NULL, detector_labels=NULL){
  animl_py <- get("animl_py", envir = parent.env(environment()))
  animl_py$plot_all_bounding_boxes(manifest, out_dir, file_col=file_col, min_conf=min_conf, 
                                   label_col=label_col, show_confidence=show_confidence,
                                   colors=colors, detector_labels=detector_labels)
}