#' Generate All Directed Networks with Optional Bidirectional Edges
#'
#' Given an undirected adjacency matrix, generate all valid directed versions,
#' optionally including bidirectional edges (2-node feedback loops).
#' Redundant networks due to symmetric bidirectional flips are removed.
#'
#' @param adj_matrix A symmetric adjacency matrix representing the undirected skeleton.
#' @param allow_bidirectional Logical. If TRUE, include bidirectional (A <-> B) edge options.
#' @return A list of unique directed adjacency matrices.
#' @export
generate_directed_networks <- function(adj_matrix, allow_bidirectional = TRUE) {
  if (!is.matrix(adj_matrix)) stop("Input must be a matrix.")
  if (!all(adj_matrix == t(adj_matrix))) stop("Matrix must be symmetric.")

  edge_list <- which(upper.tri(adj_matrix) & adj_matrix != 0, arr.ind = TRUE)
  n_edges <- nrow(edge_list)
  n_nodes <- nrow(adj_matrix)

  # Each edge: 0 = A->B, 1 = B->A, optionally 2 = A<->B
  direction_options <- if (allow_bidirectional) 0:2 else 0:1
  directions <- expand.grid(rep(list(direction_options), n_edges))

  unique_networks <- list()
  seen_hashes <- character(0)

  for (i in seq_len(nrow(directions))) {
    mat <- matrix(0, n_nodes, n_nodes)
    for (j in seq_len(n_edges)) {
      from <- edge_list[j, 1]
      to <- edge_list[j, 2]
      dir <- directions[i, j]
      if (dir == 0) {
        mat[from, to] <- adj_matrix[from, to]
      } else if (dir == 1) {
        mat[to, from] <- adj_matrix[to, from]
      } else if (dir == 2) {
        mat[from, to] <- adj_matrix[from, to]
        mat[to, from] <- adj_matrix[to, from]
      }
    }
    # Hash network to avoid duplicates
    hash <- paste(mat, collapse = "")
    if (!(hash %in% seen_hashes)) {
      unique_networks[[length(unique_networks) + 1]] <- mat
      seen_hashes <- c(seen_hashes, hash)
    }
  }

  unique_networks
}
