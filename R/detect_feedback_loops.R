#' Detect Feedback Loops
#'
#' Detect cycles (feedback loops) in a directed adjacency matrix.
#' @param adj_matrix A directed adjacency matrix.
#' @return A list of cycles, each represented as a vector of node indices.
#' @export
detect_feedback_loops <- function(adj_matrix) {
  visited <- rep(FALSE, nrow(adj_matrix))
  stack <- rep(FALSE, nrow(adj_matrix))
  cycles <- list()

  dfs <- function(node, path) {
    visited[node] <<- TRUE
    stack[node] <<- TRUE
    path <- c(path, node)

    neighbors <- which(adj_matrix[node, ] != 0)
    for (nbr in neighbors) {
      if (!visited[nbr]) {
        dfs(nbr, path)
      } else if (stack[nbr]) {
        idx <- which(path == nbr)
        cycles[[length(cycles) + 1]] <<- path[idx:length(path)]
      }
    }
    stack[node] <<- FALSE
  }

  for (i in seq_len(nrow(adj_matrix))) {
    if (!visited[i]) {
      dfs(i, integer(0))
    }
  }

  cycles
}
