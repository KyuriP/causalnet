#' Detect Feedback Loops (Cycles) in a Directed Network
#'
#' This function finds all feedback loops (i.e., directed cycles) in a directed adjacency matrix.
#' It includes both multi-node and 2-node bidirectional cycles, while avoiding duplicate reporting
#' of the same logical loop in different orders.
#'
#' @param adj_matrix A square adjacency matrix representing a directed network.
#'        Non-zero entries indicate directed edges.
#' @return A list of feedback loops. Each loop is a vector of node indices (1-based).
#'         Each loop appears only once in canonical order.
#' @examples
#' adj <- matrix(0, 3, 3)
#' adj[1, 2] <- 1
#' adj[2, 3] <- 1
#' adj[3, 1] <- 1  # Forms a cycle: 1 -> 2 -> 3 -> 1
#' detect_feedback_loops(adj)
#'
#' @export

detect_feedback_loops <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  visited <- rep(FALSE, n)  # Tracks visited nodes during DFS
  stack <- rep(FALSE, n)    # Tracks current path (recursion stack)
  cycles <- list()          # Stores unique cycles

  # Internal DFS function
  dfs <- function(node, path) {
    visited[node] <<- TRUE
    stack[node] <<- TRUE
    path <- c(path, node)

    # Explore all out-neighbors
    neighbors <- which(adj_matrix[node, ] != 0)
    for (nbr in neighbors) {
      if (!visited[nbr]) {
        dfs(nbr, path)
      } else if (stack[nbr]) {
        # Found a back edge (cycle)
        idx <- which(path == nbr)
        cycle <- path[idx:length(path)]

        # Canonicalize cycle: rotate to start with smallest node
        cycle <- as.integer(cycle)
        min_pos <- which.min(cycle)
        cycle <- c(cycle[min_pos:length(cycle)], cycle[1:(min_pos - 1)])

        # Use a string key to ensure uniqueness
        cycle_key <- paste(cycle, collapse = "-")
        if (!(cycle_key %in% names(cycles))) {
          cycles[[cycle_key]] <<- cycle
        }
      }
    }

    stack[node] <<- FALSE  # Done exploring this branch
  }

  # Run DFS from every unvisited node
  for (i in seq_len(n)) {
    if (!visited[i]) {
      dfs(i, integer(0))
    }
  }

  # Manually detect 2-node bidirectional cycles (i → j and j → i)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (adj_matrix[i, j] != 0 && adj_matrix[j, i] != 0 && i < j) {
        cycle <- c(i, j)
        cycle_key <- paste(cycle, collapse = "-")
        if (!(cycle_key %in% names(cycles))) {
          cycles[[cycle_key]] <- cycle
        }
      }
    }
  }

  # Return all unique cycles
  unname(cycles)
}
