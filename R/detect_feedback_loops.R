#' Detect Unique Feedback Loops in a Directed Network
#'
#' Detects all directed cycles in a graph, including 2-node loops,
#' and returns each as a unique set of nodes (ignores order and entry point).
#'
#' @param adj_matrix A square directed adjacency matrix (non-zero = edge).
#' @return A list of unique loops, each as a sorted vector of node indices.
#' @export
detect_feedback_loops <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  visited <- rep(FALSE, n)
  stack <- rep(FALSE, n)
  cycles <- list()
  seen_sets <- character(0)

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
        if (length(idx) > 0) {
          cycle <- path[idx:length(path)]
          sorted_set <- sort(unique(cycle))
          key <- paste(sorted_set, collapse = "-")
          if (!(key %in% seen_sets)) {
            cycles[[length(cycles) + 1]] <<- sorted_set
            seen_sets <<- c(seen_sets, key)
          }
        }
      }
    }

    stack[node] <<- FALSE
  }

  for (i in seq_len(n)) {
    visited[] <- FALSE
    stack[] <- FALSE
    dfs(i, integer(0))
  }

  unname(cycles)
}
