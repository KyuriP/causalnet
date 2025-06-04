#' Compute Feedback Loop Overlap Metrics
#'
#' Calculate node participation and overlap metrics for feedback loops.
#' @param loops A list of loops (each a vector of node indices).
#' @param n_nodes Total number of nodes in the network.
#' @return A list with node frequencies and overlap index.
#' @export
compute_overlap_metrics <- function(loops, n_nodes) {
  if (length(loops) < 2) return(list(node_freq = rep(0, n_nodes), overlap = 0))

  freq <- rep(0, n_nodes)
  for (loop in loops) {
    freq[loop] <- freq[loop] + 1
  }

  overlap_index <- sum(freq^2) / length(loops)^2
  list(node_freq = freq, overlap = overlap_index)
}
