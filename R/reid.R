#' Load MiewID model
#'
#' @param file_path path to model weights
#' @param device device to load model to
#'
#' @returns miewid model
#' @export
#'
#' @examples
#' \dontrun{miew = load_miew("miewid_v3.bin")}
load_miew <- function(file_path, device=NULL){
  animl_py <- .animl_internal$animl_py
  animl_py$load_miew(file_path, device=device)
}


#' Extract Embeddings from MiewID
#'
#' @param miew_model loaded miewid model
#' @param manifest list of files 
#' @param file_col column name containing file paths
#' @param batch_size batch size for generator
#' @param num_workers number of workers for generator
#' @param device device to run model on
#'
#' @returns matrix of embeddings
#' @export
#'
#' @examples
#' \dontrun{embeddings = extract_embeddings(miew, manifest)}
extract_miew_embeddings <- function(miew_model,
                                    manifest,
                                    file_col="filepath",
                                    batch_size=1,
                                    num_workers=1,
                                    device=NULL){
  
  animl_py <- .animl_internal$animl_py
  animl_py$extract_miew_embeddings(miew_model,
                                   manifest,
                                   file_col=file_col,
                                   batch_size=as.integer(batch_size), 
                                   num_workers=as.integer(num_workers),
                                   device=device)
}


#' Removes the diagonal elements from a square matrix.
#'
#' @param A square matrix
#'
#' @return martix A with diagonals removed
#' @export
#'
#' @examples
#' \dontrun{cleaned_dist <- remove_diagonal(dist_matrix)}
remove_diagonal <- function(A) {
  if (nrow(A) != ncol(A)) stop("Input must be a square matrix")
  n <- nrow(A)
  matrix(A[!diag(TRUE, n)], nrow = n-1, ncol = n, byrow = F)
}


#' Computes euclidean squared distance of two sets of matrices
#'
#' @param input1 2-D feature matrix
#' @param input2 2-D feature matrix
#'
#' @return distance matrix
#' @export
#'
#' @examples
#' \dontrun{dist_matrix <- euclidean_squared_distance(embeddings, embeddings)}
euclidean_squared_distance <- function(input1, input2) {
  # ||a - b||^2 = ||a||^2 + ||b||^2 - 2 * a . b^T
  s1 <- rowSums(input1^2)
  s2 <- rowSums(input2^2)
  outer(s1, s2, "+") - 2 * tcrossprod(input1, input2)
}


#' Computes cosine distance of two sets of matrices
#'
#' @param input1 2-D feature matrix
#' @param input2 2-D feature matrix 
#' 
#' @return distance matrix
#' @export
#'
#' @examples
#' \dontrun{dist_matrix <- cosine_distance(embeddings, embeddings)}
cosine_distance <- function(input1, input2) {
  # normalize rows then 1 - dot product
  norm1 <- input1 / sqrt(rowSums(input1^2))
  norm2 <- input2 / sqrt(rowSums(input2^2))
  1 - tcrossprod(norm1, norm2)
}


#'A wrapper function for computing distance matrix.
#'
#' @param input1 2-D feature matrix
#' @param input2 2-D feature matrix
#' @param metric "euclidean" or "cosine", Default is "euclidean"
#'
#' @return distance matrix
#' @export
#'
#' @examples
#' \dontrun{dist_matrix <- compute_distance_matrix(embeddings, embeddings, metric='cosine')}
compute_distance_matrix <- function(input1, input2, metric = "euclidean") {
  if (metric == "euclidean") return(euclidean_squared_distance(input1, input2))
  if (metric == "cosine")    return(cosine_distance(input1, input2))
  stop(sprintf("Unknown distance metric: '%s'. Use 'euclidean' or 'cosine'.", metric))
}


#' Computes the distance matrix in a batched manner to save memory.
#'
#' @param input1 2-D array of query features
#' @param input2 2-D array of database features
#' @param metric The distance metric to use. Options include 'euclidean', 'cosine', etc
#' @param batch_size The number of rows from input1 to process at a time
#'
#' @return distance matrix
#' @export
#'
#' @examples
#' \dontrun{
#' dist_matrix <- compute_batched_distance_matrix(query_embeddings, database_embeddings, 
#'                                                metric='cosine', batch_size=12)}
compute_batched_distance_matrix <- function(input1, input2, metric = "cosine", batch_size = 10) {
  n <- nrow(input1)
  batches <- split(seq_len(n), ceiling(seq_len(n) / batch_size))
  result <- lapply(batches, function(idx) {
    compute_distance_matrix(input1[idx, , drop = FALSE], input2, metric = metric)
  })
  do.call(rbind, result)
}