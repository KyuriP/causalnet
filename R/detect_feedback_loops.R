#' Detect Unique Feedback Loops in a Directed Network
#'
#' Detects all directed cycles (including 2-node loops) and returns each as a
#' unique set of nodes (ignores order and entry point).
#'
#' @param adj_matrix Square directed adjacency (non-zero = edge).
#' @param include_self_loops Logical; include 1-node self-loops (default FALSE).
#' @param use_names Logical; return node names if available (default TRUE).
#' @return List of unique loops, each as a sorted vector (names if available, else indices).
#' @export
detect_feedback_loops <- function(adj_matrix,
                                  include_self_loops = FALSE,
                                  use_names = TRUE) {
  if (!is.matrix(adj_matrix) || nrow(adj_matrix) != ncol(adj_matrix)) {
    stop("adj_matrix must be a square matrix.")
  }
  n <- nrow(adj_matrix)
  node_names <- colnames(adj_matrix)

  visited <- rep(FALSE, n)
  stack   <- rep(FALSE, n)
  cycles <- list()
  seen_sets <- character(0)

  dfs <- function(node, path) {
    visited[node] <<- TRUE
    stack[node]   <<- TRUE
    path <- c(path, node)

    neighbors <- which(adj_matrix[node, ] != 0)
    for (nbr in neighbors) {
      if (!visited[nbr]) {
        dfs(nbr, path)
      } else if (stack[nbr]) {
        idx <- match(nbr, path)  # first occurrence on the current path
        if (!is.na(idx)) {
          cyc <- path[idx:length(path)]
          if (include_self_loops || length(cyc) >= 2) {
            sorted_set <- sort(unique(cyc))
            key <- paste(sorted_set, collapse = "-")
            if (!(key %in% seen_sets)) {
              cycles[[length(cycles) + 1]] <<- sorted_set
              seen_sets <<- c(seen_sets, key)
            }
          }
        }
      }
    }

    stack[node] <<- FALSE
    invisible(NULL)
  }

  for (i in seq_len(n)) {
    visited[] <- FALSE
    stack[]   <- FALSE
    dfs(i, integer(0))
  }

  if (use_names && !is.null(node_names)) {
    cycles <- lapply(cycles, function(idx) unname(node_names[idx]))
  }

  unname(cycles)
}
