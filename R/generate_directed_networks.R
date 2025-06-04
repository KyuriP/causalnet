#' Generate All Directed Networks from a Skeleton (qgraph version)
#'
#' Given an undirected adjacency matrix, generate all possible directed versions using qgraph.
#' @param adj_matrix A symmetric adjacency matrix representing the undirected skeleton.
#' @return A list of adjacency matrices representing all possible directed networks.
#' @export
generate_directed_networks <- function(adj_matrix) {
  if (!is.matrix(adj_matrix)) stop("Input must be a matrix.")
  if (!all(adj_matrix == t(adj_matrix))) stop("Matrix must be symmetric (undirected).")

  edge_list <- which(upper.tri(adj_matrix) & adj_matrix != 0, arr.ind = TRUE)
  n_edges <- nrow(edge_list)
  n_nodes <- nrow(adj_matrix)
  directions <- expand.grid(rep(list(c(FALSE, TRUE)), n_edges))

  directed_networks <- vector("list", nrow(directions))

  for (i in seq_len(nrow(directions))) {
    mat <- matrix(0, n_nodes, n_nodes)
    for (j in seq_len(n_edges)) {
      from <- edge_list[j, ifelse(directions[i, j], 1, 2)]
      to <- edge_list[j, ifelse(directions[i, j], 2, 1)]
      mat[from, to] <- adj_matrix[from, to]
    }
    directed_networks[[i]] <- mat
  }

  directed_networks
}
