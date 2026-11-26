#' Count Module
#'
#' This module provides functions for counting animal detections
#' across image sequences. Ported from animl-py's Python implementation
#' as native R code (not a reticulate wrapper around animl-py) -- see
#' count_detections() and deduplicate() below, which reuse the
#' existing native sequence_calculation() from file_management.R
#' rather than calling into Python.
#'
#' Nikita Sharma 2026


#' Compute intersection-over-union between two bounding boxes
#'
#' @param bbox_a numeric vector of length 4: x, y, w, h (top-left x/y,
#'   width, height -- matching the bbox_x/bbox_y/bbox_w/bbox_h column
#'   convention used throughout animl)
#' @param bbox_b numeric vector of length 4: x, y, w, h
#'
#' @returns numeric IOU value between 0 and 1. Returns 0 if either box
#'   has any NA coordinate (e.g. an "empty" detection row with no real
#'   box) -- two boxes that can't be compared are never treated as
#'   duplicates of each other.
#'
#' @noRd
get_iou <- function(bbox_a, bbox_b) {
  if (anyNA(bbox_a) || anyNA(bbox_b)) {
    return(0)
  }

  xA <- max(bbox_a[1], bbox_b[1])
  yA <- max(bbox_a[2], bbox_b[2])
  xB <- min(bbox_a[1] + bbox_a[3], bbox_b[1] + bbox_b[3])
  yB <- min(bbox_a[2] + bbox_a[4], bbox_b[2] + bbox_b[4])

  inter_w <- max(0, xB - xA)
  inter_h <- max(0, yB - yA)
  inter_area <- inter_w * inter_h

  area_a <- bbox_a[3] * bbox_a[4]
  area_b <- bbox_b[3] * bbox_b[4]

  union_area <- area_a + area_b - inter_area
  if (union_area <= 0) { return(0) }
  inter_area / union_area
}


#' Remove duplicate detections within a single image using IOU
#'
#' When two detections overlap above the IOU threshold, keeps the one
#' with higher confidence.
#'
#' @param image_detections dataframe of detections for a single image.
#'   Expected columns: bbox_x, bbox_y, bbox_w, bbox_h, conf.
#' @param iou_threshold IOU threshold above which two detections are
#'   considered the same animal, default = 0.9
#'
#' @returns dataframe of deduplicated detections
#' @export
#'
#' @examples
#' \dontrun{
#' deduped <- deduplicate(image_detections, iou_threshold = 0.9)
#' }
deduplicate <- function(image_detections, iou_threshold = 0.9) {
  if (nrow(image_detections) == 0) {
    return(image_detections)
  }

  kept <- list()

  for (i in seq_len(nrow(image_detections))) {
    row <- image_detections[i, ]
    bbox_a <- c(row$bbox_x, row$bbox_y, row$bbox_w, row$bbox_h)
    is_duplicate <- FALSE

    if (length(kept) > 0) {
      for (j in seq_along(kept)) {
        kept_row <- kept[[j]]
        bbox_b <- c(kept_row$bbox_x, kept_row$bbox_y, kept_row$bbox_w, kept_row$bbox_h)
        iou <- get_iou(bbox_a, bbox_b)

        if (iou >= iou_threshold) {
          is_duplicate <- TRUE
          if (!is.na(row$conf) && (is.na(kept_row$conf) || row$conf > kept_row$conf)) {
            kept[[j]] <- row
          }
          break
        }
      }
    }

    if (!is_duplicate) {
      kept[[length(kept) + 1]] <- row
    }
  }

  do.call(rbind, kept)
}


#' Count detections per species across image sequences
#'
#' Groups detections into sequences per station (via the existing
#' sequence_calculation()), then counts detections per species,
#' averaged across the images within each sequence.
#'
#' @param detections dataframe, output of parse_detections()
#' @param station_col column name representing the station or camera,
#'   default = "station"
#' @param confidence_threshold minimum confidence to consider a
#'   detection, default = 0.5
#' @param iou_threshold IOU threshold for duplicate detection removal
#'   (see deduplicate()), default = 0.9
#' @param maxdiff max time difference in seconds between images in a
#'   sequence, default = 60
#' @param max_n optional max number of images per sequence to
#'   consider, default = NULL (no limit)
#' @param classes character vector of category_label values to count.
#'   NULL (default) counts every class found, except "empty".
#'
#' @returns dataframe, one row per sequence with averaged counts per species
#' @export
#'
#' @examples
#' \dontrun{
#' counts <- count_detections(detections, station_col = "station")
#' }
count_detections <- function(detections,
                              station_col = "station",
                              confidence_threshold = 0.5,
                              iou_threshold = 0.9,
                              maxdiff = 60,
                              max_n = NULL,
                              classes = NULL) {
  detections <- sequence_calculation(detections, station_col = station_col, maxdiff = maxdiff)

  if (is.null(classes)) {
    classes <- unique(detections$category_label)
    classes <- classes[!is.na(classes) & classes != "empty"]
  }

  detections <- detections[!is.na(detections$conf), ]
  detections <- detections[detections$conf >= confidence_threshold, ]
  detections <- detections[detections$category_label %in% classes, ]

  results <- list()

  for (seq_id in unique(detections$sequence)) {
    seq_group <- detections[detections$sequence == seq_id, ]
    image_counts <- list()

    filepaths <- unique(seq_group$filepath)
    for (i in seq_along(filepaths)) {
      if (!is.null(max_n) && i > max_n) { break }

      image_group <- seq_group[seq_group$filepath == filepaths[i], ]
      kept <- deduplicate(image_group, iou_threshold)

      counts <- stats::setNames(as.list(rep(0, length(classes))), classes)
      for (k in seq_len(nrow(kept))) {
        label <- kept$category_label[k]
        if (label %in% names(counts)) {
          counts[[label]] <- counts[[label]] + 1
        }
      }
      image_counts[[length(image_counts) + 1]] <- counts
    }

    if (length(image_counts) > 0) {
      avg_counts <- stats::setNames(
        lapply(classes, function(cls) {
          mean(vapply(image_counts, function(c) c[[cls]], numeric(1)))
        }),
        classes
      )
      avg_counts$sequence <- seq_id
      results[[length(results) + 1]] <- as.data.frame(avg_counts, stringsAsFactors = FALSE)
    }
  }

  if (length(results) == 0) {
    return(data.frame())
  }

  do.call(rbind, results)
}
