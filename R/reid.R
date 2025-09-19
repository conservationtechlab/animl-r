#' Load MiewID model
#'
#' @param file_path path to model weights
#' @param device toggle cpu or gpu
#'
#' @returns meiwid model
#' @export
#'
#' @examples
#' \dontrun{miew = load_miewid("miewid_v3.bin")}
load_miew <- function(file_path, device=NULL){
  animl_py <- load_animl_py()
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
extract_miew_embeddings <- function(miew_model, manifest, file_col="filepath", 
                                    batch_size=1, num_workers=1, device=NULL){
  animl_py <- load_animl_py()
  animl_py$extract_miew_embeddings(miew_model, manifest, file_col=file_col,
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
remove_diagonal <- function(A){
  animl_py <- load_animl_py()
  animl_py$remove_diagonal(A)
  
}

#' Computes euclidean squared distance of two sets of vectors
#'
#' @param input1 2-D feature matrix
#' @param input2 2-D feature matrix
#'
#' @return distance matrix
#' @export
#'
#' @examples
#' \dontrun{dist_matrix <- euclidean_squared_distance(embeddings, embeddings)}
euclidean_squared_distance <- function(input1, input2){
  animl_py <- load_animl_py()
  animl_py$euclidean_squared_distance(input1, input2)
}

#' Computes cosine distance of two sets of vectors
#'
#' @param input1 2-D feature matrix
#' @param input2 2-D feature matrix 
#' 
#' @return distance matrix
#' @export
#'
#' @examples
#' \dontrun{dist_matrix <- cosine_distance(embeddings, embeddings)}
cosine_distance <- function(input1, input2){
  animl_py <- load_animl_py()
  animl_py$cosine_distance(input1, input2)
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
compute_distance_matrix <- function(input1, input2, metric='euclidean'){
  animl_py <- load_animl_py()
  animl_py$compute_distance_matrix(input1, input2, metric=metric)
  
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
#' \dontrun{dist_matrix <- compute_batched_distance_matrix(query_embeddings, database_embeddings, metric='cosine', batch_size=12)}
compute_batched_distance_matrix <- function(input1, input2, metric='cosine', batch_size=10){
  animl_py <- load_animl_py()
  animl_py$compute_batched_distance_matrix(input1, input2, metric=metric, batch_size=batch_size)
}  